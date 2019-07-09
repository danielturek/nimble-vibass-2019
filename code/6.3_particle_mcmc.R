# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
read_chunk('chunks_stoch_vol.R')
stochVolConf <- configureMCMC(stochVolModel, nodes = NULL,
    monitors = c('beta', 'phi', 'sigma' , 'x'))
stochVolConf$addSampler(target = c('beta', 'phiStar', 'sigma' , 'x0'),
                               type = 'RW_PF_block', control = list(propCov = .1 * diag(4),
                               pfType = 'auxiliary', pfControl = list(thresh = 1),
                               adaptive = TRUE, pfNparticles = 200,
                               latents = 'x', pfResample = TRUE))
                               
stochVolMCMC <- buildMCMC(stochVolConf)
cMCMC <- compileNimble(stochVolMCMC, project = stochVolModel, resetFunctions = TRUE)
samples <- runMCMC(cMCMC, niter = 5000)
par(mfrow = c(2, 3))
hist(samples[ , 'beta'])
hist(samples[ , 'phi'])
hist(samples[ , 'sigma'])
ts.plot(samples[ , 'beta'])
ts.plot(samples[ , 'phi'])
ts.plot(samples[ , 'sigma'])
