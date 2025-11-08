import os
import pandas as pd
import glob

base_dir = os.path.dirname(os.path.abspath(__file__))
search_path = os.path.abspath(os.path.join(base_dir, '../../dumps/bench/many/tm_*.csv'))
output_path = os.path.abspath(os.path.join(base_dir, '../../report/many_benches/bench.csv'))

print(f"Searching for files in: {search_path}")

csv_files = glob.glob(search_path)
print(f"Found {len(csv_files)}")
print(f"Output path: {output_path}")

all_data = []

for file in csv_files:
    df = pd.read_csv(file, header=None, names=['idx', 'timestamp', 'latency'])
    df['id'] = len(all_data)  # assign sequential id per file
    all_data.append(df[['id', 'timestamp', 'latency']])

if all_data:
    result = pd.concat(all_data, ignore_index=True)
    result.to_csv(output_path, index=False)
else:
    print("No CSV files found. No output file created.")
