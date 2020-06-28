Tdigest
=======

OCaml implementation of the T-Digest algorithm.

The T-Digest is a data structure and algorithm for constructing an approximate distribution for a collection of real numbers presented as a stream.

The median of a list of medians is not necessarily equal to the median of the whole dataset. The median (p50), p95, and p99 are critical measures that are expensive to compute due to their requirement of having the entire **sorted** dataset present in one place.

The T-Digest can estimate percentiles or quantiles extremely accurately even at the tails, while using a fraction of the space.

Additionally, the T-Digest is concatenable, making it a good fit for distributed systems. The internal state of a T-Digest can be exported as a binary string, and the concatenation of any number of those strings can then be imported to form a new T-Digest.

Links:
- [A simple overview of the T-Digest](https://dataorigami.net/blogs/napkin-folding/19055451-percentile-and-quantile-estimation-of-big-data-the-t-digest)
- [A walkthrough of the algorithm by its creator](https://mapr.com/blog/better-anomaly-detection-t-digest-whiteboard-walkthrough/)
- [The white paper](https://github.com/tdunning/t-digest/blob/master/docs/t-digest-paper/histo.pdf)

This library started off as a port of [Will Welch's JavaScript implementation](https://github.com/welch/tdigest), down to the unit tests. However some modifications have been made to adapt it to OCaml, the most important one being immutability. As such, almost every function in the `Tdigest` module return a new `Tdigest.t`, including "reading" ones since they may trigger expensive intermediate computations worth caching.

## Usage

The API is well documented [here](https://github.com/SGrondin/tdigest/blob/master/src/tdigest.mli).

```sh
opam install tdigest
```
