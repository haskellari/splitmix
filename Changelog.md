# 0.0.5

- Add `nextInteger`
- Use smaller range in `bitmaskWithRejection32` and `64`,
  when upper bound is 2^n - 1.
  This changes generated values when they were on the boundary.

# 0.0.4

- Add `bitmaskWithRejection32'` and `bitmaskWithRejection64'`
  which generate numbers in closed range `[0, n]`.
  Unticked variants generate in closed-open range `[0, n)`.

# 0.0.3

- Add `System.Random.SplitMix32` module
- Add `bitmaskWithRejection32` and `bitmaskWithRejection64` functions
- Add `nextWord32`, `nextTwoWord32` and `nextFloat`
- Add `random` flag, dropping dependency on `random`
  (breaks things, e.g. `QuickCheck`, when disabled).

# 0.0.2

- Support back to GHC-7.0
- Add `Read SMGen` instance

# 0.0.1

- Add `NFData SMGen` instance
- Fix a bug. http://www.pcg-random.org/posts/bugs-in-splitmix.html
  The generated numbers will be different for the same seeds!
