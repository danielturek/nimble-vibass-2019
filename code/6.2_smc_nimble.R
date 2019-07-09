library(methods) ## needed only when building documents outside of R
library(nimble)
stochVolCode <- nimbleCode({
  x[1] ~ dnorm(phi * x0, sd = sigma)
  y[1] ~ dnorm(0, var = betaSquared * exp(x[1]))
  for(t in 2:T){
        ## time-evolving volatility
        x[t] ~ dnorm(phi * x[t-1], sd = sigma)
        ## observations
        y[t] ~ dnorm(0, var = betaSquared * exp(x[t]))
  }
  x0 ~ dnorm(1, sd = sigma)
  phi <- 2 * phiStar - 1
  phiStar ~ dbeta(18, 1)
  sigma ~ T(dt(mu = 0, sigma = 1, df = 1), 0, )
  ## baseline volatility
  betaSquared <- beta^2
  beta ~ T(dt(mu = 0, sigma = 1, df = 1), 0, )
})
library('stochvol')
data('exrates')
y <- 100 * logret(exrates$USD[exrates$date > '2012-02-01'])
TT <- 44
stochVolModel <- nimbleModel(code = stochVolCode,
   constants = list(T = TT), data = list(y = y),
   inits = list(beta = .5992, phi = .9702,
   sigma = .178, x0 = 0))
CstochVolModel <- compileNimble(stochVolModel)
svBootFilter <- buildBootstrapFilter(stochVolModel, nodes = 'x',
                       control = list(saveAll = TRUE, thresh = 1.0))
cSvBootFilter <- compileNimble(svBootFilter, project = stochVolModel)
cSvBootFilter$run(10000)
samples <- as.matrix(cSvBootFilter$mvEWSamples) ## equally-weighted samples from filtering distribution
par(mfrow = c(1,2))
ts.plot(y, main = 'observations')
mn <- apply(samples, 2, mean)
qs <- apply(samples, 2, quantile, c(.025, .975))
ts.plot(mn, ylim = range(qs), main = 'estimated volatility')
lines(1:TT, qs[1, ], lty = 2)
lines(1:TT, qs[2, ], lty = 2)
##     bootStepFunctions <- nimbleFunctionList(bootStepVirtual)
##     for(iNode in seq_along(nodes)){
##        bootStepFunctions[[iNode]] <- bootFStep(model, mvEWSamples, mvWSamples,
##                                               nodes, iNode, names, saveAll,
##                                               smoothing, resamplingMethod,
##                                               silent)
##     }
