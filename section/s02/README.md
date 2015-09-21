# Section 02: Testing

Is week we will cover testing. here are some resources you might find
helpful:

- [`testthat` paper](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)
- [Testing examples](https://github.com/kbroman/Tools4RR/blob/master/09_TestingDebugging/Examples/ReadMe.md)
- [Testing (R packages)](http://r-pkgs.had.co.nz/tests.html)

I will be covering a subset of Karl Broman's slides:

- http://kbroman.org/Tools4RR/assets/lectures/09_testdebug.pdf

### Errata

- objects in `R` are [copy-on-modify](http://stackoverflow.com/questions/15759117/what-exactly-is-copy-on-modify-semantics-in-r-and-where-is-the-canonical-source), not immutable (no idea why I said that. I've been thinking about strings too much)
- I swapped the notation of `df[['idx']]` (which always returns a vector) and `df[c('idx')]` (which always returns a `data.frame`)
