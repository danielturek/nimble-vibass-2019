# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
dat <- read.csv('avandia.csv')
head(dat)
dat <- read.csv('avandia.csv')
dat <- dat[-49, ]

x <- dat$controlMI
n <- dat$nControl
y <- dat$avandiaMI
m <- dat$nAvandia

nStudies <- nrow(dat)
data <- list(x = x, y = y)
constants = list(n = n, m = m, nStudies = nStudies)

codeParam <- nimbleCode({
    for(i in 1:nStudies) {
        y[i] ~ dbin(size = m[i], prob = q[i]) # avandia MIs
        x[i] ~ dbin(size = n[i], prob = p[i]) # control MIs
        q[i] <- expit(theta + gamma[i])       # Avandia log-odds
        p[i] <- expit(gamma[i])               # control log-odds
        gamma[i] ~ dnorm(mu, sd = tau)        # study effects
    }
    theta ~ dflat()        # effect of Avandia
    # random effects hyperparameters
    mu ~ dflat()
    tau ~ dunif(0, 100)
})
inits = list(theta = 0, mu = 0, tau = 1, gamma = rnorm(nStudies))

samples <- nimbleMCMC(code = codeParam, data = data, inits = inits,
                      constants = constants, monitors = c("mu", "tau", "theta", "gamma"),
                      thin = 10, niter = 21000, nburnin = 1000, nchains = 1, setSeed = TRUE)
gammaCols <- grep('gamma', colnames(samples))

par(mfrow = c(1, 4))
ts.plot(samples[ , 'theta'], xlab = 'iteration', ylab = expression(theta))
hist(samples[ , 'theta'], xlab = expression(theta), main = 'effect of Avandia')
gammaMn <- colMeans(samples[ , gammaCols])
hist(gammaMn, xlab = 'posterior means of random effects', main = 'random effects distribution')
hist(samples[1000, gammaCols], xlab = 'single draw of random effects',
                   main = 'random effects distribution')
codeBNP <- nimbleCode({
    for(i in 1:nStudies) {
        y[i] ~ dbin(size = m[i], prob = q[i]) # avandia MIs
        x[i] ~ dbin(size = n[i], prob = p[i]) # control MIs
        q[i] <- expit(theta + gamma[i])       # Avandia log-odds
        p[i] <- expit(gamma[i])               # control log-odds
        gamma[i] ~ dnorm(muTilde[xi[i]],      # random effects (from mixture)
                     var = tauTilde[xi[i]])   # with mean/var from one component of mixture
    }
    # mixture component parameters drawn from base measures
    for(i in 1:nStudies) {
        muTilde[i] ~ dnorm(mu0, sd = sd0)
        tauTilde[i] ~ dinvgamma(a0, b0)
    }
    # CRP for clustering studies to mixture components
    xi[1:nStudies] ~ dCRP(alpha, size = nStudies)
    # hyperparameters
    alpha ~ dgamma(1, 1)      
    mu0 ~ dflat()
    sd0 ~ dunif(0, 100)
    a0 ~ dunif(0, 100)
    b0 ~ dunif(0, 100)
    theta ~ dflat()          # effect of Avandia
})
inits <- list(gamma = rnorm(nStudies), xi = sample(1:2, nStudies, replace = TRUE),
              alpha = 1, mu0 = 0, sd0 = 1, a0 = 1, b0 = 1, theta = 0,
              muTilde = rnorm(nStudies), tauTilde = rep(1, nStudies))

samplesBNP <- nimbleMCMC(code = codeBNP, data = data, inits = inits,
               constants = constants,
               monitors = c("theta", "gamma", "alpha", "xi", "mu0", "sd0", "a0", "b0"),
               thin = 10, niter = 21000, nburnin = 1000, nchains = 1, setSeed = TRUE)

gammaCols <- grep('gamma', colnames(samplesBNP))
xiCols <- grep('xi', colnames(samplesBNP))

par(mfrow = c(1,5))
ts.plot(samplesBNP[ , 'theta'], xlab = 'iteration', ylab = expression(theta))
hist(samplesBNP[ , 'theta'], xlab = expression(theta), main = 'effect of Avandia')
gammaMn <- colMeans(samplesBNP[ , gammaCols])
hist(gammaMn, xlab = 'posterior means of random effects',
              main = 'random effects distribution')
hist(samplesBNP[1000, gammaCols], xlab = 'single draw of random effects',
                   main = 'random effects distribution')

# How many mixture components are inferred?
xiRes <- samplesBNP[ , xiCols]
nGrps <- apply(xiRes, 1, function(x) length(unique(x)))
ts.plot(nGrps, xlab = 'iteration', ylab = 'number of components')
model <- nimbleModel(codeBNP, constants = constants, data = data, inits = inits)
conf = configureMCMC(model, print = TRUE)
