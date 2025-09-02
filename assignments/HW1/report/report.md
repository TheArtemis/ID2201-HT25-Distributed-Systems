# Simple Tests
## First Try
First try, 100 sequential requests:
```erlang
(client@DORORO)1> test:bench(localhost, 8080).
Time Elapsed 174.338 ms
ok
```
We can serve up to **573 requests/s.**

Adding a 40ms processing delay we get:
```erlang
(client@DORORO)1> test:bench(localhost, 8080).
Time Elapsed 4408.464 ms
ok
```
The artificial delay radically increases the processing time, while the parsing overhead stays minimal.

## Multiple Clients at the same time
If we launch multiple clients at the same time and we run the bench function, saving the latency time of each request, we can see that it increases each time an new node starts to message the server as is it shown into this plot.

![Latency increases with more clients](./sequential_benches/latency_plot.png)

We need to make ours server concurrent so it can handle multiple clients at the same time and increase the throughput.

Here it goes, Multi Rudy, using a connection pool he's capable of handling connections concurrently. Compared to the previous graph, here we can see how the latency doesn't increase when adding more clients.

![Latency remains the same](./concurrent_benches/latency_plot.png)
