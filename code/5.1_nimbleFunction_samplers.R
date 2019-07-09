# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found - weird
library(nimble)
## sampler_RW <- nimbleFunction(
##     contains = sampler_BASE,  # some object-oriented syntax for MCMC samplers
##     setup = function(model, mvSaved, target, control) {
##         # All samplers must take the above arguments to work with NIMBLE's MCMC engine
##         targetAsScalar <- model$expandNodeNames(target,
##                                                 returnScalarComponents = TRUE)
##         if(length(targetAsScalar) > 1)
##            stop('cannot use RW sampler on more than one target; try RW_block sampler')
##         if(model$isDiscrete(target))
##            stop('cannot use RW sampler on discrete-valued target; try slice sampler')
##         ###  control list extraction  ###
##         scale <- control$scale
##         ###  node list generation  ###
##         calcNodes  <- model$getDependencies(target)
##     },
## 
##     run = function() {
##         # this assumes logProb values are up-to-date, which is how all samplers should be set up
##         logProbCurrent <- model$getLogProb(calcNodes)
##         # 'target', 'model' is available because it was a setup code argument
##         currentValue <- model[[target]]
##         # 'scale', 'calcNodes' available because created in setup code
##         propValue <- rnorm(1, mean = currentValue, sd = scale)
##         model[[target]] <<- propValue
##         model$calculate(calcNodes)
##         logMHR <- model$getLogProb(calcNodes) - logProbCurrent
##         jump <- decide(logMHR)
##         # make sure state of model is up-to-date and mvSaved has current iteration
##         if(jump) nimCopy(from = model, to = mvSaved, row = 1,
##                          nodes = calcNodes, logProb = TRUE)
##         else     nimCopy(from = mvSaved, to = model, row = 1,
##                          nodes = calcNodes, logProb = TRUE)
##     }
## )
sampler_RW <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ## control list extraction
        logScale      <- if(!is.null(control$log))           control$log           else FALSE
        reflective    <- if(!is.null(control$reflective))    control$reflective    else FALSE
        adaptive      <- if(!is.null(control$adaptive))      control$adaptive      else TRUE
        adaptInterval <- if(!is.null(control$adaptInterval)) control$adaptInterval else 200
        scale         <- if(!is.null(control$scale))         control$scale         else 1
        ## node list generation
        targetAsScalar <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        calcNodes <- model$getDependencies(target)
        ## numeric value generation
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        scaleHistory  <- c(0, 0)   ## scaleHistory
        acceptanceHistory  <- c(0, 0)   ## scaleHistory
        if(nimbleOptions('saveMCMChistory')) {
            saveMCMChistory <- TRUE
        } else saveMCMChistory <- FALSE
        optimalAR     <- 0.44
        gamma1        <- 0
        ## checks
        if(length(targetAsScalar) > 1)   stop('cannot use RW sampler on more than one target; try RW_block sampler')
        if(model$isDiscrete(target))     stop('cannot use RW sampler on discrete-valued target; try slice sampler')
    },
    
    run = function() {
        currentValue <- model[[target]]
        propLogScale <- 0
        if(logScale) { propLogScale <- rnorm(1, mean = 0, sd = scale)
                       propValue <- currentValue * exp(propLogScale)
                   } else         propValue <- rnorm(1, mean = currentValue,  sd = scale)
        model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes) + propLogScale
        jump <- decide(logMHR)
        if(jump) nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, logProb = TRUE)
        else     nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, logProb = TRUE)
        ## call additional method (defined below)
        if(adaptive)     adaptiveProcedure(jump)
    },

    ## additional methods, callable by nimbleFunction$method_name, or by method_name within a given nimbleFunction's run function or methods
    methods = list(
        
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                if(saveMCMChistory) {
                    setSize(scaleHistory, timesAdapted)         
                    scaleHistory[timesAdapted] <<- scale        
                    setSize(acceptanceHistory, timesAdapted)    
                    acceptanceHistory[timesAdapted] <<- acceptanceRate  
                }
                # Shaby & Wells (2011) adaptation scheme
                gamma1 <<- 1/((timesAdapted + 3)^0.8)
                gamma2 <- 10 * gamma1
                adaptFactor <- exp(gamma2 * (acceptanceRate - optimalAR))
                scale <<- scale * adaptFactor
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        
        getScaleHistory = function() {  ## scaleHistory
            returnType(double(1))
            if(saveMCMChistory) {
                return(scaleHistory)
            } else {
                print("Please set 'nimbleOptions(saveMCMChistory = TRUE)' before building the MCMC")
                return(numeric(1, 0))
            }
        },          

        getAcceptanceHistory = function() {  ## scaleHistory
            returnType(double(1))
            if(saveMCMChistory) {
                return(acceptanceHistory)
            } else {
                print("Please set 'nimbleOptions(saveMCMChistory = TRUE)' before building the MCMC")
                return(numeric(1, 0))
            }
        },          

        reset = function() {
            scale <<- scaleOriginal
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            if(saveMCMChistory) {
                scaleHistory  <<- c(0, 0)    ## scaleHistory
                acceptanceHistory  <<- c(0, 0)
            }
            gamma1 <<- 0
        }
    )
)

