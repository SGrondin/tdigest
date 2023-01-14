Tdigest
=======

OCaml implementation of the T-Digest algorithm.

```ocaml
let td =
  Tdigest.create ()
  |> Tdigest.add_list [ 10.0; 11.0; 12.0; 13.0 ]
in
Tdigest.p_ranks td [ 9.; 10.; 11.; 12.; 13.; 14. ] (* [ Some 0; Some 0.125; Some 0.375; Some 0.625; Some 0.875; Some 1 ] *)
```

The T-Digest is a data structure and algorithm for constructing an approximate distribution for a collection of real numbers presented as a stream.

The median of a list of medians is not necessarily equal to the median of the whole dataset. The median (p50), p95, and p99 are critical measures that are expensive to compute due to their requirement of having the entire **sorted** dataset present in one place.

The T-Digest can estimate percentiles or quantiles extremely accurately even at the tails, while using a fraction of the space.

Additionally, the T-Digest is concatenable, making it a good fit for distributed systems. The internal state of a T-Digest can be exported as a binary string, and the concatenation of any number of those strings can then be imported to form a new T-Digest.

```ocaml
let combined = Tdigest.merge [ td1; td2; td3 ] in
```

A T-Digest's state can be stored in a database `VARCHAR`/`TEXT` column and multiple such states can be merged by concatenating strings:
```sql
SELECT
  STRING_AGG(M.tdigest_state) AS concat_state
FROM my_table AS M
```
```ocaml
let combined = Tdigest.of_string concat_state in
```

Links:
- [A simple overview of the T-Digest](https://dataorigami.net/blogs/napkin-folding/19055451-percentile-and-quantile-estimation-of-big-data-the-t-digest)
- [A walkthrough of the algorithm by its creator](https://mapr.com/blog/better-anomaly-detection-t-digest-whiteboard-walkthrough/)
- [The white paper](https://github.com/tdunning/t-digest/blob/master/docs/t-digest-paper/histo.pdf)

This library started off as a port of [Will Welch's JavaScript implementation](https://github.com/welch/tdigest), down to the unit tests. However some modifications have been made to adapt it to OCaml, the most important one being immutability. As such, almost every function in the `Tdigest` module return a new `Tdigest.t`, including "reading" ones since they may trigger intermediate computations worth caching.

## Usage

The API is well documented [here](https://github.com/SGrondin/tdigest/blob/master/src/tdigest.mli).

```sh
opam install tdigest
```

## Performance

On a 2018 MacBook Pro, it can incorporate 1,000,000 random floating points in just 770ms.

Exporting and importing state (`to_string`/`of_string`) is essentially free.
