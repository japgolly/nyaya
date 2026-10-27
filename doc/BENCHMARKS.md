# Running Benchmarks

For example, this runs the `GenBM.vector` benchmark with 3 warmup iterations, 3 real iterations, 1 fork and 1 thread.

```
benchmark/Jmh/run -wi 3 -i 3 -f 1 -t 1 .*GenBM.vector.*
```
