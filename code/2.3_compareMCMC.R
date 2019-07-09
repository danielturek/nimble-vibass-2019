library(methods) ## needed only when building documents outside of R
library(nimble)
library(compareMCMCs)
## library(mcmcplots)
read_chunk('chunks_litters.R')
source("chunks_litters.R")
## library(devtools)
## 
## devtools::install_github("nimble-dev/compareMCMCs", subdir = "compareMCMCs")
# so attendees can run code below this without using code from other modules
# if(!exists('littersModel') || !exists('cLittersModels')) source('chunks_litters.R')
littersInfo <- list(code = littersCode,
                   constants = littersConsts,
                   data = littersData, inits = littersInits)
## littersComparisons <- compareMCMCs(
##     littersInfo,
##     MCMCs = c("jags", "nimble", "nimble_slice"),
##     nimbleMCMCdefs = list(nimble_slice =
##         function(model) {
##             configureMCMC(model, onlySlice = TRUE)
##         }),
##     MCMCcontrol = list(niter = 10000,
##                        burnin = 1000),
##     seed = 1)
## make_MCMC_comparison_pages(littersComparisons,
##                            dir = "litters_comparison_results",
##                            modelName = "littersComparisons")
