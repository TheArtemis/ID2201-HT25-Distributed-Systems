import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os

BIN_MS = 100  # Global bin size in milliseconds

BASE_PATH = os.path.dirname(os.path.abspath(__file__))
csv_file = os.path.join(BASE_PATH, '../../report/many_benches/bench.csv')
output_dir = os.path.join(BASE_PATH, '../../report/many_benches/')
os.makedirs(output_dir, exist_ok=True)

df = pd.read_csv(csv_file)
ts_col = [c for c in df.columns if 'time' in c.lower() or 'timestamp' in c.lower() or 'start' in c.lower()][0]
try:
    df[ts_col] = pd.to_datetime(df[ts_col], unit='us')
except Exception:
    df[ts_col] = pd.to_datetime(df[ts_col], errors='coerce')
if not np.issubdtype(df[ts_col].dtype, np.datetime64):
    df[ts_col] = pd.to_datetime(df[ts_col], errors='coerce')
if df[ts_col].isna().all():
    raise ValueError('Could not parse timestamps in column "{}"'.format(ts_col))

df['lat_ms'] = df['latency'].astype(float) / 1000.0 if df['latency'].astype(float).median() > 1000 else df['latency'].astype(float)

# Normalize timestamps to start at zero and convert to milliseconds
start_time = df[ts_col].min()
df['ts_ms'] = (df[ts_col] - start_time).dt.total_seconds() * 1000

# Bin timestamps into BIN_MS intervals
df['ts_bin'] = (df['ts_ms'] // BIN_MS).astype(int) * BIN_MS

# Compute mean latency per BIN_MS bin
lat_per_bin = df.groupby('ts_bin')['lat_ms'].mean()

plt.figure(figsize=(12,5))
colors = ['#1f77b4', '#aec7e8'] 
bar_colors = [colors[i % 2] for i in range(len(lat_per_bin))]
plt.bar(lat_per_bin.index, lat_per_bin.values, width=BIN_MS, align='edge', alpha=0.7, color=bar_colors)
plt.xlabel(f'Time since start (ms, binned in {BIN_MS} ms)')
plt.ylabel('Mean Latency (ms)')
plt.title(f'Mean Latency per {BIN_MS} ms Interval')
plt.tight_layout()
plt.savefig(os.path.join(output_dir, 'latency_histogram.png'))
plt.close()
