import os
import re
import sys
from pathlib import Path
from collections import defaultdict

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

def check_lamport_time(log_file_path):
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
    
    print(f"Checking Lamport time consistency in file: {log_file}")
    
    
    

    print(f"\n=== Analyzing {log_file.name} ===")
    
    # Track last known time for each process
    process_times = defaultdict(int)
    
    # Track sent messages with their timestamps
    sent_messages = {}  # message_id -> (sender, send_time, timestamp)
    
    # Track received messages
    received_messages = {}  # message_id -> [(receiver, receive_time, timestamp), ...]
    
    violations = []
    sender_receiver_violations = []
    total_events = 0
    
    with open(log_file, "r") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            
            # Parse CSV format: timestamp,name,logical_time,message
            parts = line.split(',', 3)
            if len(parts) < 4:
                continue
            
            timestamp = int(parts[0])
            process_name = parts[1]
            logical_time = int(parts[2])
            message = parts[3]
            
            total_events += 1
            
            # Check monotonicity for each process
            if process_name in process_times:
                if logical_time <= process_times[process_name]:
                    violations.append(f"Line {line_num}: {process_name} time not increasing: {process_times[process_name]} -> {logical_time}")
            
            process_times[process_name] = logical_time
            
            # Parse message content
            msg_type, inner_msg = parse_message(message)
            msg_id = extract_message_id(inner_msg)
            
            if msg_type == 'sending' and msg_id:
                # Record sent message
                sent_messages[msg_id] = (process_name, logical_time, timestamp)
            
            elif msg_type == 'received' and msg_id:
                # Record received message
                if msg_id not in received_messages:
                    received_messages[msg_id] = []
                received_messages[msg_id].append((process_name, logical_time, timestamp))
                
                # Check if we have the corresponding send event
                if msg_id in sent_messages:
                    sender, send_time, send_timestamp = sent_messages[msg_id]
                    
                    # Check sender/receiver relationship
                    if sender == process_name:
                        sender_receiver_violations.append(f"Line {line_num}: {process_name} sent and received the same message {msg_id}")
                    
                    # Check timestamp ordering (send should happen before receive)
                    if timestamp < send_timestamp:
                        sender_receiver_violations.append(f"Line {line_num}: Message {msg_id} received by {process_name} at timestamp {timestamp} before it was sent at timestamp {send_timestamp}")
                    
                    # For Lamport time: receiver_time should be > sender_time
                    if logical_time <= send_time:
                        violations.append(f"Line {line_num}: Lamport violation - {process_name} received msg {msg_id} at time {logical_time}, but {sender} sent it at time {send_time}")
                else:
                    # Message received but no corresponding send found yet
                    # We'll check this at the end
                    pass
        
        # Check for orphaned messages
        for msg_id, receivers in received_messages.items():
            if msg_id not in sent_messages:
                for receiver, receive_time, receive_timestamp in receivers:
                    sender_receiver_violations.append(f"Message {msg_id} was received by {receiver} but never sent by anyone")
        
        for msg_id, (sender, send_time, send_timestamp) in sent_messages.items():
            if msg_id not in received_messages:
                sender_receiver_violations.append(f"Message {msg_id} was sent by {sender} but never received by anyone")
        
        # Report results
        print(f"Total events processed: {total_events}")
        print(f"Lamport time violations: {len(violations)}")
        print(f"Sender/Receiver violations: {len(sender_receiver_violations)}")
        
        if violations:
            print("\n🚨 LAMPORT TIME VIOLATIONS:")
            for violation in violations[:10]:  # Show first 10 violations
                print(f"  - {violation}")
            if len(violations) > 10:
                print(f"  ... and {len(violations) - 10} more violations")
        else:
            print("\n✅ No Lamport time violations found!")
        
        if sender_receiver_violations:
            print("\n🚨 SENDER/RECEIVER VIOLATIONS:")
            for violation in sender_receiver_violations[:10]:  # Show first 10 violations
                print(f"  - {violation}")
            if len(sender_receiver_violations) > 10:
                print(f"  ... and {len(sender_receiver_violations) - 10} more violations")
        else:
            print("\n✅ No sender/receiver violations found!")
        
        # Show message statistics
        print(f"\nMessage Statistics:")
        print(f"  Total messages sent: {len(sent_messages)}")
        print(f"  Total messages received: {sum(len(receivers) for receivers in received_messages.values())}")
        print(f"  Unique message IDs: {len(set(sent_messages.keys()) | set(received_messages.keys()))}")
        
        # Show process time ranges
        print(f"\nProcess time ranges:")
        for process, final_time in sorted(process_times.items()):
            print(f"  {process}: 1 -> {final_time}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        # Use the specified log file
        log_file = sys.argv[1]
        check_lamport_time(log_file)
    else:
        # Require a log file to be specified
        print("Error: Please provide a log file to analyze")
        print("Usage: python check_lamport.py <log_file>")
        print("")
        print("Examples:")
        print("  python check_lamport.py test_random_logs.txt")
        print("  python check_lamport.py /full/path/to/logfile.txt")
        sys.exit(1)