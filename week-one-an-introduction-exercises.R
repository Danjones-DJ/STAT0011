### Two: Using Seq()
seq(from=3, to=15, by=3) # [1]  3  6  9 12 15
seq(3, 99, 4) # [1]  3  7 11 15 19 23 27 31 35 39 43 47 51 55 59 63 67 71 75 79 83 87 91 95 99

#### Find the sum of all even numbers between 2 and 100
sum( seq(2, 100, 2) ) # 2550

### Three: Generate 500 random numbers from Normal Dist, U=0, SD=1, Plot as histogram
GeneratedNums = rnorm(500, 0, 1)
hist(GeneratedNums, 25)

makeHist = function(num) {
  nums = rnorm(num, 0, 1) 
  bins = ceiling(num ** 0.5)
  hist(nums, bins)
}

makeHist(10000)

### Useful Funcs
# rexp(n, lambda) # n observations of Exp dist
# dexp(x, lambda) # evals Exp dist at point x
# pexp(x, lambda) # evals probability p(X < x)
# exp(-lambda*x) # e^kx

