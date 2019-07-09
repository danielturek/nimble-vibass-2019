# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found 
library(nimble)
RW_reflect <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        dist <- model$getDistribution(target)
        targetComponents <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        if(length(targetComponents) > 1)
                stop("RW_reflect: cannot use univariate RW sampler on multivariate target, try RW_block sampler.")
        rg <- getDistributionInfo(dist)$range
        if(rg[[1]] > -Inf || rg[[2]] < Inf)
                  reflect <- TRUE else reflect <- FALSE

        calcNodes  <- model$getDependencies(target)
    },
    
    run = function() {
        propValue <- rnorm(1, mean = model[[target]], sd = scale)

        if(reflect) {
            lower <- model$getBound(target, 'lower')
            upper <- model$getBound(target, 'upper')
             if(propValue < lower) propValue <- 2*lower - propValue
             if(propValue > upper) propValue <- 2*upper - propValue
        }
 
        model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes)
        jump <- decide(logMHR)
        if(jump)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
    },
    methods = list(
            reset = function () {}
            )
)
RW_reflect <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ## control list extraction
        logScale      <- if(!is.null(control$log))           control$log           else FALSE
        reflective    <- if(!is.null(control$reflective))    control$reflective    else FALSE
        adaptive      <- if(!is.null(control$adaptive))      control$adaptive      else TRUE
        adaptInterval <- if(!is.null(control$adaptInterval)) control$adaptInterval else 200
        scale         <- if(!is.null(control$scale))         control$scale         else 1
        ###  node list generation  ###
        targetAsScalar <- model$expandNodeNames(target, 
                       returnScalarComponents = TRUE)
        if(length(targetAsScalar) > 1)     
                       stop('more than one target; cannot use RW sampler, try RW_block sampler')
        if(model$isDiscrete(target))
                       stop('cannot use RW sampler on discrete-valued target; try slice sampler')

        ### ADDED code ############################################
        dist <- model$getDistribution(target)
        rg <- getDistributionInfo(dist)$range
        if(rg[[1]] > -Inf || rg[[2]] < Inf)
                  reflect <- TRUE else reflect <- FALSE
        ###########################################################

        calcNodes  <- model$getDependencies(target)
        ###  numeric value generation  ###
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        scaleHistory          <- c(0, 0)
        acceptanceRateHistory <- c(0, 0)
        optimalAR <- 0.44
        gamma1    <- 0
    },
    
    run = function() {
        propValue <- rnorm(1, mean = model[[target]], sd = scale)

        ### ADDED code ############################################
        if(reflect) {
            lower <- model$getBound(target, 'lower')
            upper <- model$getBound(target, 'upper')
             if(propValue < lower) propValue <- 2*lower - propValue
             if(propValue > upper) propValue <- 2*upper - propValue
        }
        ###########################################################

        model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes)
        jump <- decide(logMHR)
        if(jump)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        if(adaptive)     adaptiveProcedure(jump)
    },
    
    methods = list(
        
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                setSize(scaleHistory,          timesAdapted)
                setSize(acceptanceRateHistory, timesAdapted)
                scaleHistory[timesAdapted] <<- scale
                acceptanceRateHistory[timesAdapted] <<- acceptanceRate
                gamma1 <<- 1/((timesAdapted + 3)^0.8)
                gamma2 <- 10 * gamma1
                adaptFactor <- exp(gamma2 * (acceptanceRate - optimalAR))
                scale <<- scale * adaptFactor
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        
        reset = function() {
            scale <<- scaleOriginal
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            scaleHistory          <<- scaleHistory          * 0
            acceptanceRateHistory <<- acceptanceRateHistory * 0
            gamma1 <<- 0
        }
    ), where = getLoadingNamespace()
)

model <- readBUGSmodel('blocker', dir = system.file('classic-bugs',
      'vol1','blocker', package = 'nimble'))
model$tau
model$tau <- 0.01
conf <- configureMCMC(model)
conf$removeSamplers('tau')
# as baseline, use standard Metropolis for tau
conf$addSampler('tau', type = 'RW')
mcmc <- buildMCMC(conf)
niter <- 25000
cmodel <- compileNimble(model)
cmcmc <- compileNimble(mcmc, project = model)
set.seed(1)
smp1 <- runMCMC(cmcmc, niter = 25000)
# not clear why RW_reflect() not being put into global
# if this isn't done, configureMCMC fails to find sampler_RW_reflect in knitr
assign('RW_reflect', RW_reflect, .GlobalEnv)
conf$removeSamplers('tau')
# for comparison, consider the reflection sampler
conf$addSampler('tau', type = 'RW_reflect')
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model, resetFunctions = TRUE)

nimCopy(model, cmodel)  # initialize 2nd MCMC same as 1st MCMC
set.seed(1)
smp2 <- runMCMC(cmcmc, niter = 25000)

nplot <- 300
burnin <- 1000
plot(seq_len(nplot), smp1[seq_len(nplot), 'tau'], type = 'l', 
                     xlab = 'iteration', ylab=expression(tau))
lines(seq_len(nplot), smp2[seq_len(nplot), 'tau'], col = 'red')
library(coda, quietly = TRUE)
