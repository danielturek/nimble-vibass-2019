# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found
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
codeAFTsel <- nimbleCode({
    for(i in 1:n) {
        x[i] ~ dweib(alpha, lambda[i])
        is_cens[i] ~ dinterval(x[i], c[i])  ## right-censoring
        lambda[i] <- exp(eta[i] + Z[i,1]*delta[1] + indic*Z[i,2]*delta[2])
        eta[i] <- etaTilde[xi[i]]  ## mix over eta; mu = exp(eta)
    }
    indic ~ dbern(0.5)             ## turns Z2 on and off
    xi[1:n] ~ dCRP(conc, size = n) ## CRP for mixture components
    conc ~ dgamma(1, 1)
    for(i in 1:nSub)
        etaTilde[i] ~ dunif(b0, B0) ## base measure G_0
    alpha ~ dunif(a0, A0)
    for(j in 1:p)
        delta[j] ~ dflat()
})
##  setup = function( model, mvSaved, target, control ) {
##    # target = 'indic'
##    coefNode <- control$coef   # e.g., 'delta[2]'
##    scale    <- control$scale  # could be made adaptive
##    # with variable
##    calcNodes_full <- model$getDependencies(c(coefNode, target))
##    # without variable
##    calcNodes_reduced <- model$getDependencies(target)
##  }
## run = function( ) {  # Reversible-jump updates:
##    currentIndicator <- model[[target]]   # get current z2
##    if(currentIndicator == 0) { ## propose adding z2 to model
##      currentLogProb <- model$getLogProb(calcNodes_reduced)
##      proposalCoef <- rnorm(1, 0, sd = scale)
##      model[[target]] <<- 1      # put proposal values in model
##      model[[coefNode]] <<- proposalCoef
##      logProbForwardProposal <-
##                 dnorm(proposalCoef, 0, sd = scale, log = TRUE)
##      proposalLogProb <- model$calculate(calcNodes_full)
##      logAcceptProb <- proposalLogProb -
##                         currentLogProb -
##                         logProbForwardProposal
##    } else {                   ## propose removing z2
##      currentLogProb <- model$getLogProb(calcNodes_full)
##      currentCoef <- model[[coefNode]]      # get current beta2
##      logProbReverseProposal <-
##                  dnorm(currentCoef, 0, sd = scale, log = TRUE)
##      model[[target]] <<- 0      # put proposal values in model
##      model[[coefNode]] <<- 0
##      model$calculate(calcNodes_full) # calculate proposal log probabilities
##      logAcceptProb <- model$getLogProb(calcNodes_reduced) -
##                         currentLogProb +
##                         logProbReverseProposal
##    }
##    ## additional book-keeping code to go here
## }
RJ_var_sel <- nimbleFunction(
 contains = sampler_BASE,
 setup = function( model, mvSaved, target, control ) {
   # target = 'indic'
   coefNode <- control$coef   
   scale    <- control$scale  # could be made adaptive
   # with variable
   calcNodes_full <- model$getDependencies(c(coefNode, target))
   # without variable (don't calculate prior for coefNode since not in model)
   calcNodes_reduced <- model$getDependencies(target)
 },
run = function( ) {  # Reversible-jump updates:
   currentIndicator <- model[[target]]   
   if(currentIndicator == 0) { ## propose adding z2 to model
     currentLogProb <- model$getLogProb(calcNodes_reduced)
     proposalCoef <- rnorm(1, 0, sd = scale)
     model[[target]] <<- 1
     model[[coefNode]] <<- proposalCoef
     logProbForwardProposal <- 
                dnorm(proposalCoef, 0, sd = scale, log = TRUE)
     proposalLogProb <- model$calculate(calcNodes_full)
     logAcceptProb <- proposalLogProb - 
                        currentLogProb - 
                        logProbForwardProposal
   } else {                   ## propose removing z2
     currentLogProb <- model$getLogProb(calcNodes_full)
     currentCoef <- model[[coefNode]]      
     logProbReverseProposal <- 
                 dnorm(currentCoef, 0, sd = scale, log = TRUE)   
     model[[target]] <<- 0      
     model[[coefNode]] <<- 0
     model$calculate(calcNodes_full) # calculate proposal log probabilities
     logAcceptProb <- model$getLogProb(calcNodes_reduced) -
                        currentLogProb + 
                        logProbReverseProposal
    }
    accept <- decide(logAcceptProb)
    if(accept) {
      copy(from = model, to = mvSaved, row = 1, 
         nodes = calcNodes_full, logProb = TRUE)
    } else {
      copy(from = mvSaved, to = model, row = 1, 
         nodes = calcNodes_full, logProb = TRUE)
    }
 },
 methods = list(reset = function() {
 })
) 

slice_var_sel_wrapper <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        regular_slice_sampler <- sampler_slice(model, mvSaved, target = target, 
                                         control = control$sliceControl)
        indicatorNode <- control$indicator
    },
    run = function() {
        if(model[[indicatorNode]] == 1) regular_slice_sampler$run()
    },
    methods = list(
        reset = function() {regular_slice_sampler$reset()}
    ))
# not clear why RW_reflect() not being put into global
# if this isn't done, configureMCMC fails to find sampler_RW_reflect in knitr
assign('slice_var_sel_wrapper', slice_var_sel_wrapper, .GlobalEnv)
assign('RJ_var_sel', RJ_var_sel, .GlobalEnv)
nSub = 15
constants = list(b0 = -10, B0 = 10, a0 = 0.1, A0 = 10, p = 2, n = n,
                 c = cens_time, Z = cbind(logBUN, HGB), nSub = nSub)
data = list(is_cens = as.numeric(alive), x = time)
xInit <- rep(NA, n)
xInit[alive] <- cens_time[alive] + 10
inits = list(alpha = 1, delta = c(0, 0), conc = 1,
             etaTilde = runif(nSub, constants$b0, constants$B0),
             xi = sample(1:3, n, replace = TRUE), x = xInit, indic = 1)
model <- nimbleModel(codeAFTsel, constants = constants, data = data, inits = inits)
cmodel = compileNimble(model)
conf <- configureMCMC(model, thin = 10, monitors = c('alpha', 'delta', 'indic'))
conf$removeSamplers(c('alpha', 'delta', 'etaTilde', 'indic'))

## for example, we'll use all slice samplers
conf$addSampler('alpha', 'slice')
conf$addSampler('delta[1]', 'slice')
for(node in model$expandNodeNames('etaTilde'))
    conf$addSampler(node,'slice')

## special changes for variable selection:
conf$addSampler('indic', 'RJ_var_sel', control = list(coef = 'delta[2]', scale = 0.5))
conf$addSampler('delta[2]', 'slice_var_sel_wrapper', control = list(indicator = 'indic'))

mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model)
resultsAFT <- runMCMC(cmcmc, niter = 21000, nburnin = 1000)
par(mfrow = c(1,2))
ts.plot(resultsAFT[ , 'indic'], xlab = 'iteration', ylab = 'z2 indicator',
                    main = 'z2 indicator')
ts.plot(resultsAFT[ , 'delta[2]'], xlab = 'iterations', ylab = 'delta[2]',
               main = 'hemoglobin coefficient')

## posterior probability of inclusion    
mean(resultsAFT[ , 'indic'])  
