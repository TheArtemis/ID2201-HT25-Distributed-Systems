import pandas as pd
import matplotlib.pyplot as plt
import os
import glob
import numpy as np

csv_files = glob.glob('../dumps/concurrent_bench_*.csv')
output_path = '../report/concurrent_benches/latency_plot.png'

plt.figure(figsize=(14, 8))

# I like these colors
colors = [(0.090, 0.298, 0.505), (0.850, 0.325, 0.098), (0.137, 0.427, 0.137)]

for i, csv_path in enumerate(csv_files):
    df = pd.read_csv(csv_path, header=None, names=['index', 'current_time', 'response_time'])
    df['response_time_ms'] = df['response_time'] / 1000.0

    # Extract filename for legend
    filename = os.path.basename(csv_path).replace('concurrent_bench_', '').replace('.csv', '')

    # Sort by current_time to ensure proper line plotting
    df = df.sort_values('current_time')

    plt.plot(df['current_time'], df['response_time_ms'],
             label=f'Run of {filename}', color=colors[i], alpha=0.7, linewidth=1.5)

    # Add vertical line at the first occurrence 
    first_time = df['current_time'].iloc[0]
    plt.axvline(x=first_time, color=colors[i], linestyle='--', alpha=0.5, linewidth=1)
    
    # Label Start
    ymin = df['response_time_ms'].min()
    plt.text(
        first_time, ymin, f'Start of {filename}',
        color=colors[i], rotation=0, va='bottom', ha='center',
        fontsize=8, alpha=0.95, fontweight='bold',
        bbox=dict(facecolor='white', edgecolor=colors[i], boxstyle='round,pad=0.3', alpha=0.8)
    )

    # Add vertical line at the last occurrence 
    last_time = df['current_time'].iloc[-1]
    plt.axvline(x=last_time, color=colors[i], linestyle=':', alpha=0.5, linewidth=1)
    
    # Label End
    plt.text(
        last_time, ymin, f'End of {filename}',
        color=colors[i], rotation=0, va='bottom', ha='center',
        fontsize=8, alpha=0.95, fontweight='bold',
        bbox=dict(facecolor='white', edgecolor=colors[i], boxstyle='round,pad=0.3', alpha=0.8)
    )

plt.xlabel('Time')
plt.ylabel('Response Time (ms)')
plt.title('Latency Comparison')
plt.grid(True, alpha=0.3)
plt.legend()

os.makedirs(os.path.dirname(output_path), exist_ok=True)
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f'Plot saved to {output_path}')
