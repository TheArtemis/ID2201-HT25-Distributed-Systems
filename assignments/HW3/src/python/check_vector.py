import os
import re
import sys
from pathlib import Path
from collections import defaultdict

def parse_vector_clock(vector_str):
    """Parse vector clock string like '[{eve,73},{carol,75},{dave,60},{bob,58},{alice,60}]' to dict"""
    vector_dict = {}
    # Remove brackets
    vector_str = vector_str.strip('[]')
    if not vector_str:
        return vector_dict
    
    # Split by },{ but be careful about the first and last elements
    entries = vector_str.split('},{')
    
    for entry in entries:
        # Clean up the entry by removing any remaining braces
        entry = entry.strip('{}')
        parts = entry.split(',')
        if len(parts) == 2:
            process_name = parts[0]
            time_value = int(parts[1])
            vector_dict[process_name] = time_value
    
    return vector_dict

def parse_message(message_str):
    """Parse message to extract type and content"""
    # Remove outer braces and split
    message_str = message_str.strip('{}')
    if message_str.startswith('sending,'):
        # Extract the inner message: {sending,{hello,58}} -> {hello,58}
        inner = message_str[8:]  # Remove 'sending,'
        return 'sending', inner
    elif message_str.startswith('received,'):
        # Extract the inner message: {received,{hello,58}} -> {hello,58}
        inner = message_str[9:]  # Remove 'received,'
        return 'received', inner
    return 'unknown', message_str

def extract_message_id(inner_msg):
    """Extract message ID from inner message like {hello,58}"""
    inner_msg = inner_msg.strip('{}')
    parts = inner_msg.split(',')
    if len(parts) >= 2:
        return parts[1]
    return None

def vector_clock_less_than_or_equal(vc1, vc2):
    """Check if vc1 <= vc2 (vc1 happened before or concurrent with vc2)"""
    all_processes = set(vc1.keys()) | set(vc2.keys())
    
    for process in all_processes:
        time1 = vc1.get(process, 0)
        time2 = vc2.get(process, 0)
        if time1 > time2:
            return False
    return True

def vector_clock_less_than(vc1, vc2):
    """Check if vc1 < vc2 (vc1 strictly happened before vc2)"""
    return vector_clock_less_than_or_equal(vc1, vc2) and vc1 != vc2

def check_vector_time(log_file_path):
    current_file_path = os.path.abspath(__file__)
    
    # Use the specified log file
    if os.path.isabs(log_file_path):
        log_file = Path(log_file_path)
    else:
        # Treat as relative to the logs directory
        logs_path = Path(current_file_path).parent.parent.parent / "dumps" / "logs"
        log_file = logs_path / log_file_path
    
    if not log_file.exists():
        print(f"Error: Log file {log_file} does not exist!")
        return
    
    print(f"Checking Vector Clock consistency in file: {log_file}")
    
    log_files = [log_file]
    
    for log_file in log_files:
        print(f"\n=== Analyzing {log_file.name} ===")
        
        # Track last known vector clock for each process
        process_vectors = defaultdict(dict)
        
        # Track sent messages with their vector clocks
        sent_messages = {}  # message_id -> (sender, vector_clock, timestamp)
        
        # Track received messages
        received_messages = {}  # message_id -> [(receiver, vector_clock, timestamp), ...]
        
        violations = []
        sender_receiver_violations = []
        vector_violations = []
        warnings = []
        total_events = 0
        
        with open(log_file, "r") as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()
                if not line:
                    continue
                
                # Parse the line more carefully due to commas in vector clocks
                # Format: timestamp,name,[vector_clock],{message}
                parts = line.split(',', 1)  # Split on first comma to get timestamp
                if len(parts) < 2:
                    continue
                
                timestamp = int(parts[0])
                rest = parts[1]
                
                # Find the next comma after the process name
                next_comma = rest.find(',')
                if next_comma == -1:
                    continue
                
                process_name = rest[:next_comma]
                rest = rest[next_comma + 1:]
                
                # Find the vector clock by looking for [...] pattern
                if not rest.startswith('['):
                    continue
                
                bracket_count = 0
                vector_end = -1
                for i, char in enumerate(rest):
                    if char == '[':
                        bracket_count += 1
                    elif char == ']':
                        bracket_count -= 1
                        if bracket_count == 0:
                            vector_end = i
                            break
                
                if vector_end == -1:
                    continue
                
                vector_clock_str = rest[:vector_end + 1]
                message = rest[vector_end + 2:]  # Skip ],
                
                total_events += 1
                
                # Parse vector clock
                current_vector = parse_vector_clock(vector_clock_str)
                
                # Check vector clock monotonicity for each process
                if process_name in process_vectors:
                    prev_vector = process_vectors[process_name]
                    
                    # Check if current vector is properly advanced from previous
                    if not vector_clock_less_than_or_equal(prev_vector, current_vector):
                        vector_violations.append(f"Line {line_num}: {process_name} vector clock not monotonic")
                    
                    # Check if process incremented its own clock
                    prev_own_time = prev_vector.get(process_name, 0)
                    curr_own_time = current_vector.get(process_name, 0)
                    
                    msg_type, inner_msg = parse_message(message)
                    # For sending events, the process should increment its own clock
                    if msg_type == 'sending' and curr_own_time <= prev_own_time:
                        vector_violations.append(f"Line {line_num}: {process_name} didn't increment own clock on send: {prev_own_time} -> {curr_own_time}")
                
                process_vectors[process_name] = current_vector.copy()
                
                # Parse message content
                msg_type, inner_msg = parse_message(message)
                msg_id = extract_message_id(inner_msg)
                
                if msg_type == 'sending' and msg_id:
                    # Record sent message
                    sent_messages[msg_id] = (process_name, current_vector.copy(), timestamp)
                
                elif msg_type == 'received' and msg_id:
                    # Record received message
                    if msg_id not in received_messages:
                        received_messages[msg_id] = []
                    received_messages[msg_id].append((process_name, current_vector.copy(), timestamp))
                    
                    # Check if we have the corresponding send event
                    if msg_id in sent_messages:
                        sender, send_vector, send_timestamp = sent_messages[msg_id]
                        
                        # Check sender/receiver relationship
                        if sender == process_name:
                            sender_receiver_violations.append(f"Line {line_num}: {process_name} sent and received the same message {msg_id}")
                        
                        # Check timestamp ordering (send should happen before receive)
                        if timestamp < send_timestamp:
                            sender_receiver_violations.append(f"Line {line_num}: Message {msg_id} received by {process_name} at timestamp {timestamp} before it was sent at timestamp {send_timestamp}")
                        
                        # For Vector clocks: send_vector should be < receive_vector
                        if not vector_clock_less_than(send_vector, current_vector):
                            violations.append(f"Line {line_num}: Vector clock violation - {process_name} received msg {msg_id} but send vector is not < receive vector")
                            violations.append(f"  Send vector: {send_vector}")
                            violations.append(f"  Receive vector: {current_vector}")
                    else:
                        # Message received but no corresponding send found yet
                        # We'll check this at the end
                        pass
        
        # Check for orphaned messages
        for msg_id, receivers in received_messages.items():
            if msg_id not in sent_messages:
                for receiver, receive_vector, receive_timestamp in receivers:
                    warnings.append(f"Message {msg_id} was received by {receiver} but never sent by anyone")
        
        for msg_id, (sender, send_vector, send_timestamp) in sent_messages.items():
            if msg_id not in received_messages:
                warnings.append(f"Message {msg_id} was sent by {sender} but never received by anyone")
        
        # Report results
        print(f"Total events processed: {total_events}")
        print(f"Vector clock violations: {len(violations)}")
        print(f"Vector clock monotonicity violations: {len(vector_violations)}")
        print(f"Sender/Receiver violations: {len(sender_receiver_violations)}")
        print(f"Warnings: {len(warnings)}")
        
        if violations:
            print("\n🚨 VECTOR CLOCK ORDERING VIOLATIONS:")
            for violation in violations[:20]:  # Show first 20 violations
                print(f"  - {violation}")
            if len(violations) > 20:
                print(f"  ... and {len(violations) - 20} more violations")
        else:
            print("\n✅ No vector clock ordering violations found!")
        
        if vector_violations:
            print("\n🚨 VECTOR CLOCK MONOTONICITY VIOLATIONS:")
            for violation in vector_violations[:10]:  # Show first 10 violations
                print(f"  - {violation}")
            if len(vector_violations) > 10:
                print(f"  ... and {len(vector_violations) - 10} more violations")
        else:
            print("\n✅ No vector clock monotonicity violations found!")
        
        if sender_receiver_violations:
            print("\n🚨 SENDER/RECEIVER VIOLATIONS:")
            for violation in sender_receiver_violations[:10]:  # Show first 10 violations
                print(f"  - {violation}")
            if len(sender_receiver_violations) > 10:
                print(f"  ... and {len(sender_receiver_violations) - 10} more violations")
        else:
            print("\n✅ No sender/receiver violations found!")
        
        if warnings:
            print("\n⚠️  WARNINGS:")
            for warning in warnings[:10]:  # Show first 10 warnings
                print(f"  - {warning}")
            if len(warnings) > 10:
                print(f"  ... and {len(warnings) - 10} more warnings")
        else:
            print("\n✅ No warnings found!")
        
        # Show message statistics
        print(f"\nMessage Statistics:")
        print(f"  Total messages sent: {len(sent_messages)}")
        print(f"  Total messages received: {sum(len(receivers) for receivers in received_messages.values())}")
        print(f"  Unique message IDs: {len(set(sent_messages.keys()) | set(received_messages.keys()))}")
        
        # Show process vector clock final states
        print(f"\nProcess final vector clocks:")
        for process, final_vector in sorted(process_vectors.items()):
            # Sort the vector clock entries alphabetically by process name
            sorted_vector = dict(sorted(final_vector.items()))
            print(f"  {process}: {sorted_vector}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        # Use the specified log file
        log_file = sys.argv[1]
        check_vector_time(log_file)
    else:
        # Require a log file to be specified
        print("Error: Please provide a log file to analyze")
        print("Usage: python check_vector.py <log_file>")
        print("")
        print("Examples:")
        print("  python check_vector.py test_vector_logs.txt")
        print("  python check_vector.py /full/path/to/logfile.txt")
        sys.exit(1)