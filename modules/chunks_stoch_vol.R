## @knitr sv-code

stochVolCode <- nimbleCode({
  x[1] ~ dnorm(phi * x0, sd = sigma)
  y[1] ~ dnorm(0, var = betaSquared * exp(x[1]))
  for(t in 2:T){
        x[t] ~ dnorm(phi * x[t-1], sd = sigma)
        y[t] ~ dnorm(0, var = betaSquared * exp(x[t]))
  }
  x0 ~ dnorm(1, sd = sigma)
  phi <- 2 * phiStar - 1
  phiStar ~ dbeta(18, 1)
  sigma ~ T(dt(mu = 0, sigma = 1, df = 1), 0, )
  betaSquared <- beta^2
  beta ~ T(dt(mu = 0, sigma = 1, df = 1), 0, )
})

## @knitr sv-model

library('stochvol')
data('exrates')
y <- 100 * logret(exrates$USD[exrates$date > '2012-02-01'])
stochVolModel <- nimbleModel(code = stochVolCode,
   constants = list(T = 44), data = list(y = y),
   inits = list(beta = .5992, phi = .9702,
   sigma = .178, x0 = 0))
CstochVolModel <- compileNimble(stochVolModel)
