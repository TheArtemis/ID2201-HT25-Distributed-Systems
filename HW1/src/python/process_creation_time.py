def main():
    with open("../report/spawn_benches/process_creation_time.txt") as f:
        times_us = [int(line.strip()) for line in f if line.strip().isdigit()]

    if not times_us:
        print("No valid data found.")
        return

    avg_ms = sum(times_us) / len(times_us) / 1000
    max_ms = max(times_us) / 1000
    min_ms = min(times_us) / 1000

    print(f"Average process creation time: {avg_ms:.3f} ms")
    print(f"Maximum process creation time: {max_ms:.3f} ms")
    print(f"Minimum process creation time: {min_ms:.3f} ms")

if __name__ == "__main__":
    main()