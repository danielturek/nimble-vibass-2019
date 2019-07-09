# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
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
codeAFT <- nimbleCode({
    for(i in 1:n) {
        x[i] ~ dweib(alpha, lambda[i])
        is_cens[i] ~ dinterval(x[i], c[i])  ## right-censoring
        lambda[i] <- exp(eta[i] + Z[i,1]*delta[1] + Z[i,2]*delta[2])
        eta[i] <- etaTilde[xi[i]]  ## mix over eta; mu = exp(eta)
    }
    xi[1:n] ~ dCRP(conc, size = n) ## CRP for mixture components
    conc ~ dgamma(1, 1)
    for(i in 1:nSub)    ## cap the number of clusters for faster computation
        etaTilde[i] ~ dunif(b0, B0) ## base measure G_0
    alpha ~ dunif(a0, A0)
    for(j in 1:p)
        delta[j] ~ dflat()
})
nSub = 15
constants = list(b0 = -10, B0 = 10, a0 = 0.1, A0 = 10, p = 2, n = n,
                 c = cens_time, Z = cbind(logBUN, HGB), nSub = nSub)
data = list(is_cens = as.numeric(alive), x = time)
xInit <- rep(NA, n)
xInit[alive] <- cens_time[alive] + 10
inits = list(alpha = 1, delta = c(0, 0), conc = 1,
             etaTilde = runif(nSub, constants$b0, constants$B0),
             xi = sample(1:3, n, replace = TRUE), x = xInit)
model <- nimbleModel(codeAFT, constants = constants, data = data, inits = inits)
cmodel = compileNimble(model)
conf <- configureMCMC(model, thin = 10, monitors = c('alpha', 'delta', 'xi'))
conf$removeSamplers(c('alpha', 'delta', 'etaTilde'))
conf$addSampler('alpha','slice')
for(node in model$expandNodeNames('delta'))
    conf$addSampler(node,'slice')
for(node in model$expandNodeNames('etaTilde'))
    conf$addSampler(node,'slice')
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model)
resultsAFT <- runMCMC(cmcmc, niter = 21000, nburnin = 1000) 

xiCols <- grep('xi', colnames(resultsAFT))
nComponents <- apply(resultsAFT[ , xiCols], 1, function(x) length(unique(x)))

par(mfrow = c(1,2))
ts.plot(resultsAFT[ , 'alpha'], xlab = 'iteration', ylab = expression(alpha),
                    main = expression(alpha))
ts.plot(nComponents, xlab = 'iterations', ylab = 'number of clusters',
               main = 'number of clusters')
beta <- -resultsAFT[ , grep('delta', colnames(resultsAFT))] / resultsAFT[ , 'alpha']
par(mfrow = c(1,2))
ts.plot(beta[ , 1], xlab = 'iteration', ylab = expression(beta[1]),
                    main = expression(beta[1]))
ts.plot(beta[ , 2], xlab = 'iteration', ylab = expression(beta[2]),
                    main = expression(beta[2]))
codeAFTstick <- nimbleCode({
    for(i in 1:n) {
        x[i] ~ dweib(alpha, lambda[i])
        is_cens[i] ~ dinterval(x[i], c[i])    ## right-censoring
        lambda[i] <- exp(eta[i] + Z[i,1]*delta[1] + Z[i,2]*delta[2])
        eta[i] <- etaTilde[xi[i]]
        xi[i] ~ dcat(prob[1:nSub])            ## finite mixture
    }
    prob[1:nSub] <- stick_breaking(z[1:(nSub-1)])  ## stick-breaking form of CRP
    for(i in 1:(nSub-1))
        z[i] ~ dbeta(1, conc)

    for(i in 1:nSub) 
        etaTilde[i] ~ dunif(b0, B0)           ## base measure G_0
    conc ~ dgamma(1, 1) 
    alpha ~ dunif(a0, A0)
    for(j in 1:p)
        delta[j] ~ dflat()
})
