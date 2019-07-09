# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
library(knitr)
read_chunk('chunks_litters.R')
# so attendees can run code below this without using code from other modules
if(FALSE) 
  if(!exists('littersModel') || !exists('cLittersModel')) source('chunks_litters.R')
littersConf <- configureMCMC(littersModel, monitors = c('a', 'b', 'p'))
littersMCMC <- buildMCMC(littersConf)
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel)
niter <- 5000
nburn <- 1000
set.seed(1)
samples <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
        inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)

makePlot(samples)
littersConf$printSamplers()
hypers <- c('a[1]', 'b[1]', 'a[2]', 'b[2]')
for(h in hypers) {
      littersConf$removeSamplers(h)
      littersConf$addSampler(target = h, type = 'slice')
}
littersConf$printSamplers()

littersMCMC <- buildMCMC(littersConf)
## we need 'resetFunctions' because we are rebuilding the MCMC for an existing model for
## which we've already done some compilation
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel, resetFunctions = TRUE)

set.seed(1)
samplesSlice <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
             inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)
library(coda, warn.conflicts = FALSE)
effectiveSize(samplesSlice)
makePlot(samplesSlice)
library(rjags)

cat("model {
  for (i in 1:G) {
     for (j in 1:N) {
        # likelihood (data model)
        r[i,j] ~ dbin(p[i,j], n[i,j])
        # latent process (random effects)
        p[i,j] ~ dbeta(a[i], b[i]) 
     }
     # prior for hyperparameters
     a[i] ~ dgamma(1, .001)
     b[i] ~ dgamma(1, .001)
   }
}", file = file.path(tempdir(), "tmp.bug"))

set.seed(2)  ## note: some other seeds result in slice sampler being stuck
inits <- littersInits
inits$p <- matrix(0.5, G, N)
## model <- jags.model(file = file.path(tempdir(), 'tmp.bug'),
##       data = list(G = G, N = N, n = n, r = r),
##       n.adapt = nburn, n.chains = 1, inits = inits)
## 
## samples <- jags.samples(model, variable.names = c('a','b'), n.iter = niter)
e <- try(dfdfdf, silent = TRUE)

while(inherits(e, 'try-error')) {
    e <- try({
        model <- jags.model(file = file.path(tempdir(), 'tmp.bug'),
              data = list(G = G, N = N, n = n, r = r),
              n.adapt = nburn, n.chains = 1, inits = inits)
              
        samples <- jags.samples(model, variable.names = c('a','b'), n.iter = niter)
	}, silent = TRUE)
}
par(mfrow = c(2, 2), mai = c(.6, .5, .4, .1), mgp = c(1.8, 0.7, 0))
ts.plot(samples[[1]][1, , 1], xlab = 'iteration',
        ylab = expression(a[1]), main = expression(a[1]))
ts.plot(samples[[2]][1, , 1], xlab = 'iteration',
        ylab = expression(b[1]), main = expression(b[1]))
ts.plot(samples[[1]][2, , 1], xlab = 'iteration',
        ylab = expression(a[2]), main = expression(a[2]))
ts.plot(samples[[2]][2, , 1], xlab = 'iteration',
        ylab = expression(b[2]), main = expression(b[2]))
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
##   real p[G, N];
## }
## model {
##     for (i in 1:G) {
##      for (j in 1:N) {
##         // likelihood (data model)
##         r[i,j] ~ binomial(n[i,j], p[i,j]);
##         // latent process (random effects)
##         p[i,j] ~ beta(a[i], b[i]);
##      }
##      // prior for hyperparameters
##      a[i] ~ gamma(1, .001);
##      b[i] ~ gamma(1, .001);
##    }
## }
## "
## 
## verbose <- FALSE
## init = littersInits
## init$p <- matrix(0.5, G, N)
## fit1 <- stan(model_code = code, iter = 250, warmup = 100, chains = 1,
##      data = littersData, init = list(init), verbose = verbose)
## fit2 <- stan(model_code = code, iter = 250, warmup = 100, chains = 1,
##      data = littersData, init = list(init), control = list(adapt_delta = 0.95),
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
