library(methods) ## needed only when building documents outside of R
library(nimble)
source("chunks_litters.R")
## 
## littersCode <- nimbleCode({
##   for (i in 1:G) {
##      for (j in 1:N) {
##      	# likelihood (data model)
##         r[i,j] ~ dbin(p[i,j], n[i,j])
## 	# latent process (random effects)
##         p[i,j] ~ dbeta(a[i], b[i])
##      }
##      # prior for hyperparameters
##      a[i] ~ dgamma(shape = 1, rate = .001)
##      b[i] ~ dgamma(shape = 1, scale = 1/.001)
##    }
## })
# so attendees can run code below this without using code from other modules
# if(!exists('littersModel') || !exists('cLittersModels')) source('chunks_litters.R')
littersModel$isData('r')
littersModel$isData('p')
littersModel$r
littersModel$p
littersModel$simulate('r')
littersModel$simulate('p')
littersModel$r
littersModel$p
littersModel$simulate('r', includeData = TRUE)
littersModel$r
nimbleOptions(verbose = FALSE)
m1 <- nimbleModel(
    nimbleCode({
        for(i in 1:5) {
            predicted[i] <- beta0 + beta1 * x[i]
        }
    }
    ))
m2 <- nimbleModel(
    nimbleCode({
        predicted[1:5] <- beta0 + beta1 * x[1:5]
    }
    ))
## m1 has 5 scalar nodes
m1$getNodeNames()
## m2 has 1 vector node
m2$getNodeNames()
code <- nimbleCode({
    sigma ~ dunif(0, 10)
    beta0 ~ dnorm(0, sd = 1000)
    beta1 ~ dnorm(0, sd = 1000)
    if(INCLUDE_X2) { beta2 ~ dnorm(0, sd = 1000) } else {}
    for(i in 1:10) {
        if(INCLUDE_X2) {
            y[i] ~ dnorm(beta0 + beta1 * x1[i] + beta2 * x2[i], sd = sigma)
        } else {
            y[i] ~ dnorm(beta0 + beta1 * x1[i], sd = sigma)
        }
    }
})

INCLUDE_X2 <- FALSE
m1 <- nimbleModel(code)
INCLUDE_X2 <- TRUE
m2 <- nimbleModel(code)
m1$getNodeNames()
m2$getNodeNames()

