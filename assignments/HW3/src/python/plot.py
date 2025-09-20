import pandas as pd
import os
import matplotlib.pyplot as plt

FILE = 'test_asint_10_queue_len'

csv_file = f'/home/happylemon/kth/ID2201-HT25-Distributed-Systems/assignments/HW3/dumps/{FILE}.csv'
output_image = f'{FILE}_plot.png'
output_dir = '/home/happylemon/kth/ID2201-HT25-Distributed-Systems/assignments/HW3/report/imgs/'

os.makedirs(output_dir, exist_ok=True)
df = pd.read_csv(csv_file)

y_col = df.columns[1]

plt.plot(df.index, df[y_col], label=y_col)

plt.xlabel('Message Number')
plt.ylabel('Queue Length')
plt.title('Queue Length Over Messages')
plt.grid(True)
plt.tight_layout()
plt.savefig(os.path.join(output_dir, output_image))

