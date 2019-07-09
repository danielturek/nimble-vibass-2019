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
     # such gamma priors are not generally recommended, but
     # these are the priors from the original example
     a[i] ~ dgamma(1, .001)
     b[i] ~ dgamma(1, .001)
   }
})
