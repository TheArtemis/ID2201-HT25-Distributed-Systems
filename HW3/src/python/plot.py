import os
import pandas as pd
import matplotlib.pyplot as plt

# File and directory setup
FILE = 'test_queue_len_50'
csv_file = f'/home/happylemon/kth/ID2201-HT25-Distributed-Systems/assignments/HW3/dumps/{FILE}.csv'
output_image = f'{FILE}_histogram.png'
output_dir = '/home/happylemon/kth/ID2201-HT25-Distributed-Systems/assignments/HW3/report/imgs/'

# Ensure output directory exists
os.makedirs(output_dir, exist_ok=True)

# Read CSV data
df = pd.read_csv(csv_file)

# Select the column to plot
y_col = df.columns[1]

# Alternate colors
colors = ['#1f77b4', "#adddf5"]
bar_colors = [colors[i % 2] for i in range(len(df))]

# Plotting as histogram (bar plot)
plt.figure(figsize=(10, 6))
plt.bar(df.index, df[y_col], color=bar_colors)

plt.xlabel('Iteration Number')
plt.ylabel('Queue Length')
plt.title('Queue Length Over Iterations (Histogram)')
plt.grid(axis='y')
plt.tight_layout()

# Optionally, set y-axis limits to spread out the data
y_min, y_max = df[y_col].min(), df[y_col].max()
plt.ylim(y_min - 0.1 * abs(y_max - y_min), y_max + 0.1 * abs(y_max - y_min))

# Save the plot
plt.savefig(os.path.join(output_dir, output_image))

