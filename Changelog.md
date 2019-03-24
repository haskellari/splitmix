# 0.0.2

- Support back to GHC-7.0
- Add `Read SMGen` instance

# 0.0.1

- Add `NFData SMGen` instance
- Fix a bug. http://www.pcg-random.org/posts/bugs-in-splitmix.html
  The generated numbers will be different for the same seeds!
