# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found
library(nimble)
read_chunk("chunks_litters.R")
# so attendees can run code below this without using code from other modules
if(FALSE) 
   if(!exists('littersModel') || !exists('cLittersModels')) source('chunks_litters.R')
littersConf <- configureMCMC(littersModel, print = TRUE)
littersConf$addMonitors(c('a', 'b', 'p'))
littersMCMC <- buildMCMC(littersConf)
cLittersMCMC <- compileNimble(littersMCMC, project = littersModel)
niter <- 1000
nburn <- 100
set.seed(1)
inits <- function() {
      a <- runif(G, 1, 20)
      b <- runif(G, 1, 20)
      p <- rbind(rbeta(N, a[1], b[1]), rbeta(N, a[2], b[2]))
      return(list(a = a, b = b, p = p))
}             
print(system.time(samples <- runMCMC(cLittersMCMC, niter = niter, nburnin = nburn,
                          inits = inits, nchains = 3, samplesAsCodaMCMC = TRUE)))
library(basicMCMCplots)

basicMCMCplots::chainsPlot(samples,
                           var = c("a", "b"),
                           cex = 1.5)
library(coda, warn.conflicts = FALSE)

samples1 <- samples[[1]]

crosscorr(samples1[ , c('a[1]', 'b[1]', 'a[2]', 'b[2]')])

effectiveSize(samples1)  ## ESS
par(mfrow = c(1,1))
gelman.diag(samples)

## and here's a graphical representation of the information
basicMCMCplots::chainsPlot(samples,
                           var = c("a", "b"),
                           cex = 1.5,
                           densityplot = FALSE,
                           legend.location = 'topleft')
