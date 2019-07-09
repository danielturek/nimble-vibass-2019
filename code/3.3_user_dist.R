# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
read_chunk('chunks_litters.R')
dbetabin <- nimbleFunction(
    run = function(x = double(0), alpha = double(0), beta = double(0), size = double(0), 
        log = integer(0, default = 0)) {
        
        returnType(double(0))
        logProb <- lgamma(size+1) - lgamma(x+1) - lgamma(size - x + 1) +
            lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta) +
            lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta)
        if(log) return(logProb)
        else return(exp(logProb))
    })

rbetabin <- nimbleFunction(
    run = function(n = integer(0), alpha = double(0), beta = double(0), size = double(0)) {
        returnType(double(0))
        if(n != 1) print("rbetabin only allows n = 1; using n = 1.")
        p <- rbeta(1, alpha, beta)
        return(rbinom(1, size = size, prob = p))
    })
# not clear why dbetabin() not being put into global
# if this isn't done, registerDistributions fails to find dbetabin in knitr
assign('dbetabin', dbetabin, .GlobalEnv)
assign('rbetabin', rbetabin, .GlobalEnv)
littersMargCode <- nimbleCode({
  for (i in 1:G) {
     for (j in 1:N) {
     	 # (marginal) likelihood (data model)
        r[i,j] ~ dbetabin(a[i], b[i], n[i,j])
     }
     # prior for hyperparameters
     a[i] ~ dgamma(1, .001)
     b[i] ~ dgamma(1, .001)
   }
})
G <- 2
N <- 16
n <- matrix(c(13, 12, 12, 11, 9, 10, 
              9, 9, 8, 11, 8, 10, 13, 10, 12, 9, 10, 9, 10, 5, 9, 9, 13, 
              7, 5, 10, 7, 6, 10, 10, 10, 7), nrow = 2)
r <- matrix(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 10, 8, 9, 
     12, 9, 11, 8, 9, 8, 9, 4, 8, 7, 11, 4, 4, 5, 5, 3, 7, 3, 7, 0), 
     nrow = 2)
              
littersConsts <- list(G = G, N = N, n = n)
littersData <- list(r = r)
littersInits <- list( a = c(2, 2), b=c(2, 2) )
littersMargModel <- nimbleModel(littersMargCode, 
          data = littersData, constants = littersConsts, inits = littersInits)
cLittersMargModel <- compileNimble(littersMargModel)
littersMargConf <- configureMCMC(littersMargModel, print = TRUE)
hypers <- c('a[1]', 'b[1]', 'a[2]', 'b[2]')
for(h in hypers) {
      littersMargConf$removeSamplers(h)
      littersMargConf$addSampler(target = h, type = 'RW', control = list(log = TRUE))
}
littersMargConf$printSamplers()

littersMargMCMC <- buildMCMC(littersMargConf)
cLittersMargMCMC <- compileNimble(littersMargMCMC, project = littersMargModel)
niter <- 5000
nburn <- 1000
set.seed(1)
samplesMarg <- runMCMC(cLittersMargMCMC, niter = niter, nburnin = nburn,
        inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)
makePlot(samplesMarg)
## library(rstan)
## code <- "
## data {
##   int G; int N;
##   int r[G, N];
##   int n[G, N];
## }
## parameters {
##   real a[G];
##   real b[G];
## }
## model {
##     for (i in 1:G) {
##      for (j in 1:N) {
##         // likelihood (data model)
##         r[i,j] ~ beta_binomial(n[i,j], a[i], b[i]);
##      }
##      // prior for hyperparameters
##      a[i] ~ gamma(1, .001);
##      b[i] ~ gamma(1, .001);
##    }
## }
## "
## 
## verbose <- FALSE
## fit1 <- stan(model_code = code, iter = 250, warmup = 100, chains = 1,
##      data = littersData, init = list(littersInits),
##      control = list(adapt_delta = 0.9, max_treedepth = 12),
##      verbose = verbose)
## 
## out <- extract(fit1)
## 
## par(mfrow = c(2, 2), mai = c(.6, .5, .4, .1), mgp = c(1.8, 0.7, 0))
## ts.plot(out[['a']][ , 1], xlab = 'iteration',
##         ylab = expression(a[1]), main = expression(a[1]))
## ts.plot(out[['b']][ , 1], xlab = 'iteration',
##         ylab = expression(b[1]), main = expression(b[1]))
## ts.plot(out[['a']][ , 2], xlab = 'iteration',
##         ylab = expression(a[2]), main = expression(a[2]))
## ts.plot(out[['b']][ , 2], xlab = 'iteration',
##         ylab = expression(b[2]), main = expression(b[2]))
