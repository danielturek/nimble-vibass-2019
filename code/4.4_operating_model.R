# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
read_chunk("chunks_litters.R")
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
littersModel$r  
littersModel$a[1]
littersModel$p
littersModel$a[1] <- 5
littersModel$a
set.seed(1)  # so the calculations are reproducible
littersModel$simulate('p')  # simulate from prior
littersModel$p
littersModel$getLogProb('p')  # log prob not yet updated!
littersModel$calculate('p')   # update it
littersModel$getLogProb('p')  # now we're good
littersModel$getLogProb('p')
littersModel$a[1] <- 1
littersModel$b[1] <- 3
littersModel$getLogProb('p')  # recall why this hasn't changed yet
## littersModel$calculate('p')  ## DON'T DO THIS (though it's ok to do here...)
littersModel$calculate(littersModel$getDependencies(c('a[1]','b[1]')))
## or we could just update the entire model to be safe:
## littersModel$calculate()
littersModel$getLogProb('p')
