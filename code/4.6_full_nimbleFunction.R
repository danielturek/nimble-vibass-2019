# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
read_chunk('chunks_litters_marginal.R')
margSampler <- nimbleFunction(
            setup = function(fullModel, samplesMarg, target) {
                  ## samplesMarg is a matrix of samples from the marginalized model
                  ## target contains the nodes that have been integrated over that we want to sample from

                  ## setup MCMC only for integrated-over nodes
                  conf <- configureMCMC(fullModel, nodes = target, monitors = target)
                  
                  ## check everything is conjugate
                  samplers <- sapply(conf$getSamplers(),
                     function(x) x$name)
                  if(length(samplers) != length(grep("conjugate", samplers)))
                     stop("Not all samplers are conjugate")

                  ## note: haven't thought through if this is ok for multiple levels of marginalization...

                  ## check samples have all the nodes in marginalized model
                      allNodes <- fullModel$getNodeNames(stochOnly = TRUE, includeData = FALSE)
                      targetNodes <- fullModel$expandNodeNames(target)
                      margNodes <- allNodes[!allNodes %in% targetNodes]
                      neededVars <- fullModel$getVarNames(nodes = margNodes)
                      margMCMCvars <- fullModel$getVarNames(nodes = dimnames(samplesMarg)[[2]])
                      if(any(!neededVars %in% margMCMCvars))
                         stop("Some needed variables not present in samplesMarg for marginalized model")
                  
                  ## create MCMC object and modelValues for full model
                  mcmc <- buildMCMC(conf)
                  mvSamplesConf  <- conf$getMvSamplesConf(1)  ## modelValues 'configuration' ('1' is the first primary set of samples)
                  newMV <- modelValues(mvSamplesConf, m = 1)  ## default storage (m=1 row) for new samples
            },
            run = function(samplesMarg = double(2)) {
                  ## dynamically determine how many samples we will get
                  ## ('samplesMarg' might have been updated since setup code was run)
                  nIts <- dim(samplesMarg)[1]
                  resize(newMV, nIts)

                  ## sample integrated-over nodes once per thinned iteration of original MCMC
                  for(i in 1:nIts) {
                      values(fullModel, margMCMCvars) <<- samplesMarg[i, ]
                      mcmc$run(1, reset = FALSE, progressBar = FALSE)
                      copy(fullModel, newMV, nodes = targetNodes, row = i)
                  }
            })
# so attendees can run code below this without using code from other modules
if(!exists('littersModel') || !exists('cLittersModel')) source('chunks_litters.R')
if(!exists('littersMargModel') || !exists('cLittersMargModel')) source('chunks_litters_marginal.R')
littersMargModel <- nimbleModel(littersMargCode, 
          data = littersData, constants = littersConsts, inits = littersInits)
cLittersMargModel <- compileNimble(littersMargModel)
thin = 10
littersMargConf <- configureMCMC(littersMargModel, print = TRUE, thin = thin)
littersMargMCMC <- buildMCMC(littersMargConf)

littersModel <- nimbleModel(littersCode, 
          data = littersData, constants = littersConsts, inits = littersInits)

## We can set up the new sampler without actually running the MCMC
rMargSampler <- margSampler(littersModel, as.matrix(littersMargMCMC$mvSamples), 'p')

cLittersModel <- compileNimble(littersModel)
cLittersMargMCMC <- compileNimble(littersMargMCMC, project = littersMargModel)
niter <- 5000
nburnin <- 1000
samplesMarginal <- runMCMC(cLittersMargMCMC, niter, nburnin)

cMargSampler <- compileNimble(rMargSampler, project = littersModel)
cMargSampler$run(samplesMarginal)
fullSamples <- as.matrix(cMargSampler$newMV)

dim(fullSamples)
dimnames(fullSamples)[[2]]
ts.plot(fullSamples[ , 1])

## function to do A %*% x, with A a matrix and x a vector
matrixMult <- nimbleFunction(
    run = function(A = double(2), x = double(1)) {
        y <- A %*% x
        return(y)
        returnType(double(1))
    }
)
A <- matrix(1:4, nrow = 2)
x <- c(10, 20)
A %*% x
matrixMult(A, x)
## CmatrixMult <- try(compileNimble(matrixMult))
## CmatrixMult
## matrixMult2 <- nimbleFunction(
##     run = function(A = double(2), x = double(1)) {
##         ## Say for some reason we already have used y as a vector
##         y <- rnorm(10, mean = 0, sd = 1)
##         ## Now we try to use y again
##         y <- A %*% x
##         return(y)
##         returnType(double(1))
##     }
## )
## matrixMult2(A, x)
## CmatrixMult2 <- try(compileNimble(matrixMult2))
## CmatrixMult2
