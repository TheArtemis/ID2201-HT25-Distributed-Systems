# Simple Tests
First try, 100 sequential requests:
```erlang
(client@DORORO)1> test:bench(localhost, 8080).
Time Elapsed 174.338 ms
ok
```
We can serve up to 573 requests/s.

Averaging 11 runs of 100 sequential requests each:

Time Elapsed (avg): 158.472 ms

We can serve up to 631 requests/s on average.

Adding a 40ms processing delay we get:
```erlang
(client@DORORO)1> test:bench(localhost, 8080).
Time Elapsed 4408.464 ms
ok
```


The artificial delay radically increases the processing time, while the parsing overhead stays minimal.