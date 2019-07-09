# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
library(nimble)
littersCode <- nimbleCode({
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
})
## data and constants as R objects
G <- 2
N <- 16
n <- matrix(c(13, 12, 12, 11, 9, 10, 
              9, 9, 8, 11, 8, 10, 13, 10, 12, 9, 10, 9, 10, 5, 9, 9, 13, 
              7, 5, 10, 7, 6, 10, 10, 10, 7), nrow = 2)
r <- matrix(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 10, 8, 9, 
     12, 9, 11, 8, 9, 8, 9, 4, 8, 7, 11, 4, 4, 5, 5, 3, 7, 3, 7, 0), 
     nrow = 2)
              
littersConsts <- list(G = G, N = N, n = n)
littersData <- list(r = r)
littersInits <- list( a = c(2, 2), b=c(2, 2) )

## create the NIMBLE model object
littersModel <- nimbleModel(littersCode, 
          data = littersData, constants = littersConsts, inits = littersInits)
cLittersModel <- compileNimble(littersModel)
cLittersModel$p
cLittersModel$calculate('a')   # log-prior density
cLittersModel$getLogProb('a')

cLittersModel$a <- c(3, 3)
cLittersModel$getLogProb('a')
cLittersModel$calculate('a')   # log-prior density

set.seed(1)  # so the calculations are reproducible
littersModel$simulate('p')  # simulate from prior
littersModel$p
littersModel$getLogProb('p')  # log prob not yet updated!
littersModel$calculate('p')   # update it
littersModel$getLogProb('p')  # now we're good
