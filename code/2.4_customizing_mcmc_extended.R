# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
library(coda, warn.conflicts = FALSE)
read_chunk('chunks_litters.R')
# so attendees can run code below this without using code from other modules
if(FALSE)
   if(!exists('littersModel') || !exists('cLittersModels')) source('chunks_litters.R')
niter <- 5000
nburn <- 1000

littersConf <- configureMCMC(littersModel, monitors = c('a', 'b', 'p'))
hypers <- littersModel$getNodeNames(topOnly = TRUE)
print(hypers)
for(h in hypers) {
      littersConf$removeSamplers(h)
}
littersConf$addSampler(target = c('a[1]','b[1]'), type = 'RW_block', 
                              control = list(adaptInterval = 100))
littersConf$addSampler(target = c('a[2]','b[2]'), type = 'RW_block', 
                              control = list(adaptInterval = 100))

littersMCMC <- buildMCMC(littersConf)
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel, resetFunctions = TRUE)

set.seed(1)
samplesBlock <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
             inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)
effectiveSize(samplesBlock)
makePlot(samplesBlock)
littersConf$removeSamplers(c('a', 'b', 'p'))
group1nodes <- littersModel$getDependencies(c('a[1]', 'b[1]'), stochOnly = TRUE)
group2nodes <- littersModel$getDependencies(c('a[2]', 'b[2]'), stochOnly = TRUE)
group1nodes
propCov <- diag(c(.5, .5, rep(.01, 16)))
littersConf$addSampler(group1nodes, 'RW_block', control =
                       list(adaptInterval = 100, propCov = propCov))
littersConf$addSampler(group2nodes, 'RW_block', control =
                       list(adaptInterval = 100, propCov = propCov))

littersMCMC <- buildMCMC(littersConf)
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel, resetFunctions = TRUE)

set.seed(1)
samplesSuperblock <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
                  inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)
effectiveSize(samplesSuperblock)
makePlot(samplesSuperblock)
littersConf$removeSamplers(c('a', 'b', 'p'))
littersConf$addSampler(c('a[1]', 'b[1]'), 'crossLevel')
littersConf$addSampler(c('a[2]', 'b[2]'), 'crossLevel')

littersMCMC <- buildMCMC(littersConf)
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel, resetFunctions = TRUE)

set.seed(1)
samplesCross <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
             inits = littersInits, nchains = 1, samplesAsCodaMCMC = TRUE)
effectiveSize(samplesCross)
makePlot(samplesCross)
