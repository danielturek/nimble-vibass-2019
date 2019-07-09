# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
read_chunk("chunks_litters.R")
codeAFT <- nimbleCode({
    for(i in 1:n) {
        x[i] ~ dweib(alpha, lambda[i])
        is_cens[i] ~ dinterval(x[i], c[i])  ## right-censoring
        lambda[i] <- exp(eta + inprod(Z[i,1:p], delta[1:p]))
    }
    eta ~ dunif(b0, B0) ## base measure H_b
    alpha ~ dunif(a0, A0)
    delta[1:p] ~ dmnorm(mu[1:p], cov = Sigma[1:p, 1:p])
})
library(emplik, quietly = TRUE, warn.conflicts = FALSE)
data(myeloma)

n <- nrow(myeloma)
time <-  myeloma[ , 1]    ## survival or censoring time
vstatus <- myeloma[ , 2]  ##  0 = alive (i.e., censored)
alive <- vstatus == 0
cens_time <- rep(NA, n)
cens_time[alive] <- time[alive]
cens_time[!alive] <- Inf
time[alive] <- NA
## covariates:
logBUN <- myeloma[ , 3]
HGB <- myeloma[ , 4]
logBUN <- (logBUN - mean(logBUN)) / sd(logBUN)
HGB <- (HGB - mean(HGB)) / sd(HGB)

nSub = 15
constants = list(b0 = -10, B0 = 10, a0 = 0.1, A0 = 10, p = 2, n = n,
                 c = cens_time, Z = cbind(logBUN, HGB), nSub = nSub,
                 mu = rep(0, 2), Sigma = diag(2))
data = list(is_cens = as.numeric(alive), x = time)
xInit <- rep(NA, n)
xInit[alive] <- cens_time[alive] + 10
inits = list(alpha = 1, delta = c(0, 0), eta = 1, x = xInit)
model <- nimbleModel(codeAFT, constants = constants, data = data, inits = inits)
nodes <- model$getNodeNames()
nodes[c(1:7, 70:73, 135:138, 200:203)]
top <- model$getNodeNames(topOnly = TRUE)
top
etaDeps <- model$getDependencies('eta')
etaDeps[c(1:4, 67:69, 132:134)]
model$getNodeNames(dataOnly = TRUE)
model$isData('x')
## parameters (including imputed data)
model$getNodeNames(stochOnly = TRUE, includeData = FALSE)
args(model$getDependencies)
args(model$getNodeNames)

latents <- model$getNodeNames(latentOnly = TRUE, stochOnly = TRUE, includeData = FALSE)
latents
model$getDependencies(latents, determOnly = TRUE)
model$getNodeNames(dataOnly = TRUE)
model$getVarNames()
model$getVarInfo('delta')
m1 <- nimbleModel(
    nimbleCode({
        tau ~ dunif(0, 100)
        x ~ dnorm(0, tau) #by default, tau is a precision
    }))
plot(m1$getGraph())
m1$getNodeNames()
## nimbleCode({
##     tau ~ dunif(0, 100)
##     lifted_d1_over_sqrt_oPtau_cP <- 1/sqrt(tau)
##     x ~ dnorm(0, sd = lifted_d1_over_sqrt_oPtau_cP) # override and make 2nd arg the SD
## })
m1$tau <- 3
m1$x <- 1
m1$calculate(c('tau','x')) ## Wrong: the lifted node is being neglected
m1$getDependencies('tau')
m1$calculate( m1$getDependencies('tau') )
