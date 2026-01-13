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


### Four: function, alpha beta that return mean 0.6, sd 0.1

betaParams <- function(mu, sigma2) {
  alpha <- ( (1-mu)/sigma2 - 1/mu)*mu^2
  beta <- alpha*(1/mu - 1)
  return(c(alpha,beta))
}

mujohn <- 0.5
varjohn <- 0.03^2
temp  <- betaParams(mujohn,varjohn)
alphajohn <- temp[1]; betajohn <- temp[2]


betaParams <- function(mu, sigma2) {
  alpha <- ( (1-mu)/sigma2 - 1/mu)*mu^2
  beta <- alpha*(1/mu - 1)
  return(c(alpha,beta))
}

betaParams(0.6, 0.1)

plot()



# Realistic use of Data ---------------------------------------------------
earthquakedata = read.csv("Italy.csv")

# Times between successibe earthquakes
times = diff(earthquakedata[,1])
hist(times, breaks = 30)

plot(times)
mean(times) # [1] 36.93144
var(times) # [1] 3425.677
sd(times) # [1] 58.52928
max(times) # [1] 638
min(times) # [1] 1

# Fit an exp model
### Estimate Lambda
E.Lambda = 1/mean(times) # [1] 0.0270772

## Kolmogorov-Smirnoff Test to assess whether this model is a good fit
# ks.test(dataset_to_use, distname, distparams)
ks.test(times, 'pexp', E.Lambda) # p-value = 6.489e-09
# Low P val so reject Exponential assumption

# Calculate Log Likelihood
LogLik = sum(dexp(times, E.Lambda, log=TRUE))
LogLik


## Alt approahc dont do it :(
Lik = prod(dexp(times, E.Lambda, log=FALSE))
LogLik = log(Lik)





