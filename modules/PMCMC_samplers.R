####################################################################
### virtual nimbleFunction template, included for ALL samplers #####
####################################################################

#' @rdname samplers
#' @export
sampler_BASE <- nimbleFunctionVirtual(
    methods = list(
        reset = function() { }
    )
)

 #######################################################################################
### RW_PF, does a univariate RW, but using a particle filter likelihood function ######
#######################################################################################

#' @rdname samplers
#' @export
sampler_RW_PF <- nimbleFunction(
    name = 'sampler_RW_PF',
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ## control list extraction
        adaptive       <- if(!is.null(control$adaptive))             control$adaptive             else TRUE
        adaptInterval  <- if(!is.null(control$adaptInterval))        control$adaptInterval        else 200
        scale          <- if(!is.null(control$scale))                control$scale                else 1
        m              <- if(!is.null(control$pfNparticles))         control$pfNparticles         else 1000
        resample       <- if(!is.null(control$pfResample))           control$pfResample           else FALSE
        filterType     <- if(!is.null(control$pfType))               control$pfType               else 'bootstrap'
        lookahead      <- if(!is.null(control$pfLookahead))          control$pfLookahead          else 'simulate'
        optimizeM      <- if(!is.null(control$pfOptimizeNparticles)) control$pfOptimizeNparticles else FALSE
        latents        <- if(!is.null(control$latents))              control$latents              else stop('RW_PF sampler missing required control argument: latents')
        ## node list generation
        targetAsScalar <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        calcNodes <- model$getDependencies(target)
        latentSamp <- FALSE
        MCMCmonitors <- tryCatch(parent.frame(2)$conf$monitors, error = function(e) e)
        if(identical(MCMCmonitors, TRUE))
            latentSamp <- TRUE
        else if(any(model$expandNodeNames(latents) %in% model$expandNodeNames(MCMCmonitors)))
            latentSamp <- TRUE
        latentDep <- model$getDependencies(latents)
        topParams <- model$getNodeNames(stochOnly=TRUE, includeData=FALSE, topOnly=TRUE)
        ## numeric value generation
        optimizeM       <- as.integer(optimizeM)
        scaleOriginal   <- scale
        timesRan        <- 0
        timesAccepted   <- 0
        timesAdapted    <- 0
        prevLL          <- 0
        nVarEsts        <- 0
        itCount         <- 0
        optimalAR       <- 0.44
        gamma1          <- 0
        storeParticleLP <- -Inf
        storeLLVar      <- 0
        nVarReps        <- 7    # number of LL estimates to compute to get each LL variance estimate for m optimization
        mBurnIn         <- 15   # number of LL variance estimates to compute before deciding optimal m
        d               <- length(targetAsScalar)
        if(optimizeM) m <- 3000
        ## nested function and function list definitions
        my_setAndCalculate <- setAndCalculateOne(model, target)
        my_decideAndJump <- decideAndJump(model, mvSaved, calcNodes)
        if(latentSamp == TRUE) { 
          saveAllVal <- TRUE
          smoothingVal <- TRUE
        } else {
          saveAllVal <- FALSE
          smoothingVal <- FALSE
        }
        if(filterType == 'auxiliary') {
            my_particleFilter <- buildAuxiliaryFilter(model, latents, control = list(saveAll = saveAllVal, smoothing = smoothingVal, lookahead = lookahead, initModel = FALSE))
        }
        else if(filterType == 'bootstrap') {
            my_particleFilter <- buildBootstrapFilter(model, latents, control = list(saveAll = saveAllVal, smoothing = smoothingVal, initModel = FALSE))
        }
        else   stop('filter type must be either bootstrap or auxiliary')
        particleMV <- my_particleFilter$mvEWSamples
        ## checks
        if(any(target%in%model$expandNodeNames(latents)))   stop('PMCMC \'target\' argument cannot include latent states')
        if(length(targetAsScalar) > 1)                      stop('more than one top-level target; cannot use RW_PF sampler, try RW_PF_block sampler')
    },
    run = function() {
        if(resample) {
            modelLP0 <- my_particleFilter$run(m)
            modelLP0 <- modelLP0 + getLogProb(model, target)
        }
        else   modelLP0 <- storeParticleLP + getLogProb(model, target)
        propValue <- rnorm(1, mean = model[[target]], sd = scale)
        my_setAndCalculate$run(propValue)
        particleLP <- my_particleFilter$run(m)
        modelLP1 <- particleLP + getLogProb(model, target)
        jump <- my_decideAndJump$run(modelLP1, modelLP0, 0, 0)
        if(jump & latentSamp){
            ## if we jump, randomly sample latent nodes from pf output and put into model so that they can be monitored
            index <- ceiling(runif(1, 0, m))
            copy(particleMV, model, latents, latents, index)
            calculate(model, latentDep)
            copy(from = model, to = mvSaved, nodes = latentDep, row = 1, logProb = TRUE)
        }
        else if(!jump & latentSamp){
            ## if we don't jump, replace model latent nodes with saved latent nodes
            copy(from = mvSaved, to = model, nodes = latentDep, row = 1, logProb = TRUE)
        }
        if(jump & !resample)  storeParticleLP <<- particleLP
        if(jump & optimizeM) optimM()
        if(adaptive)     adaptiveProcedure(jump)
    },
    methods = list(
        optimM = function() {
            tempM <- 15000
            declare(LLEst, double(1, nVarReps))
            if(nVarEsts < mBurnIn) {  # checks whether we have enough var estimates to get good approximation
                for(i in 1:nVarReps)
                    LLEst[i] <- my_particleFilter$run(tempM)
                ## next, store average of var estimates
                if(nVarEsts == 1)
                    storeLLVar <<- var(LLEst)/mBurnIn
                else {
                    LLVar <- storeLLVar
                    LLVar <- LLVar + var(LLEst)/mBurnIn
                    storeLLVar<<- LLVar
                }
                nVarEsts <<- nVarEsts + 1
            }
            else {  # once enough var estimates have been taken, use their average to compute m
                m <<- m*storeLLVar/(0.92^2)
                m <<- ceiling(m)
                if(!resample) {  #reset LL with new m value after burn-in period
                  storeParticleLP <<- my_particleFilter$run(m)
                }
                optimizeM <<- 0
            }
        },
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
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
            storeParticleLP <<- -Inf
            gamma1 <<- 0
        }
    ), where = getLoadingNamespace()
)



#######################################################################################
### RW_PF_block, does a block RW, but using a particle filter likelihood function #####
#######################################################################################

#' @rdname samplers
#' @export
sampler_RW_PF_block <- nimbleFunction(
    name = 'sampler_RW_PF_block',
    contains = sampler_BASE,
    setup = function(model, mvSaved, target,  control) {
        ## control list extraction
        adaptive       <- if(!is.null(control$adaptive))             control$adaptive             else TRUE
        adaptScaleOnly <- if(!is.null(control$adaptScaleOnly))       control$adaptScaleOnly       else FALSE
        adaptInterval  <- if(!is.null(control$adaptInterval))        control$adaptInterval        else 200
        scale          <- if(!is.null(control$scale))                control$scale                else 1
        propCov        <- if(!is.null(control$propCov))              control$propCov              else 'identity'
        m              <- if(!is.null(control$pfNparticles))         control$pfNparticles         else 1000
        resample       <- if(!is.null(control$pfResample))           control$pfResample           else FALSE
        filterType     <- if(!is.null(control$pfType))               control$pfType               else 'bootstrap'
        lookahead      <- if(!is.null(control$pfLookahead))          control$pfLookahead          else 'simulate'
        optimizeM      <- if(!is.null(control$pfOptimizeNparticles)) control$pfOptimizeNparticles else FALSE
        latents        <- if(!is.null(control$latents))              control$latents              else stop('RW_PF sampler missing required control argument: latents')
        ## node list generation
        targetAsScalar <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        calcNodes <- model$getDependencies(target)
        latentSamp <- FALSE
        MCMCmonitors <- tryCatch(parent.frame(2)$conf$monitors, error = function(e) e)
        if(identical(MCMCmonitors, TRUE))
            latentSamp <- TRUE
        else if(any(model$expandNodeNames(latents) %in% model$expandNodeNames(MCMCmonitors)))
            latentSamp <- TRUE
        latentDep <- model$getDependencies(latents)
        topParams <- model$getNodeNames(stochOnly=TRUE, includeData=FALSE, topOnly=TRUE)
        target <- model$expandNodeNames(target)
        ## numeric value generation
        optimizeM     <- as.integer(optimizeM)
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        prevLL        <- 0
        nVarEsts      <- 0
        itCount       <- 0
        d <- length(targetAsScalar)
        if(is.character(propCov) && propCov == 'identity')     propCov <- diag(d)
        propCovOriginal <- propCov
        chol_propCov <- chol(propCov)
        chol_propCov_scale <- scale * chol_propCov
        empirSamp <- matrix(0, nrow=adaptInterval, ncol=d)
        storeParticleLP <- -Inf
        storeLLVar  <- 0
        nVarReps <- 7    # number of LL estimates to compute to get each LL variance estimate for m optimization
        mBurnIn  <- 15   # number of LL variance estimates to compute before deciding optimal m
        if(optimizeM)   m <- 3000
        ## nested function and function list definitions
        my_setAndCalculate <- setAndCalculate(model, target)
        my_decideAndJump <- decideAndJump(model, mvSaved, calcNodes)
        my_calcAdaptationFactor <- calcAdaptationFactor(d)
        if(latentSamp == TRUE) { saveAllVal <- TRUE
                                 smoothingVal <- TRUE
                             } else {
                                 saveAllVal <- FALSE
                                 smoothingVal <- FALSE
                             }
        if(filterType == 'auxiliary') {
            my_particleFilter <- buildAuxiliaryFilter(model, latents, control = list(saveAll = saveAllVal, smoothing = smoothingVal, lookahead = lookahead, initModel = FALSE))
        }
        else if(filterType == 'bootstrap') {
            my_particleFilter <- buildBootstrapFilter(model, latents, control = list(saveAll = saveAllVal, smoothing = smoothingVal, initModel = FALSE))
        }
        else   stop('filter type must be either bootstrap or auxiliary')
        particleMV <- my_particleFilter$mvEWSamples
        ## checks
        if(class(propCov) != 'matrix')        stop('propCov must be a matrix\n')
        if(class(propCov[1,1]) != 'numeric')  stop('propCov matrix must be numeric\n')
        if(!all(dim(propCov) == d))           stop('propCov matrix must have dimension ', d, 'x', d, '\n')
        if(!isSymmetric(propCov))             stop('propCov matrix must be symmetric')
        if(length(targetAsScalar) < 2)        stop('less than two top-level targets; cannot use RW_PF_block sampler, try RW_PF sampler')
        if(any(target%in%model$expandNodeNames(latents)))   stop('PMCMC \'target\' argument cannot include latent states')
    },
    run = function() {
        if(resample) {
            modelLP0 <- my_particleFilter$run(m)
            modelLP0 <- modelLP0 + getLogProb(model, target)
        }
        else   modelLP0 <- storeParticleLP + getLogProb(model, target)
        propValueVector <- generateProposalVector()
        my_setAndCalculate$run(propValueVector)
        particleLP <- my_particleFilter$run(m)
        modelLP1 <- particleLP + getLogProb(model, target)
        jump <- my_decideAndJump$run(modelLP1, modelLP0, 0, 0)
        if(jump & latentSamp) {
            ## if we jump, randomly sample latent nodes from pf output and put
            ## into model so that they can be monitored
            index <- ceiling(runif(1, 0, m))
            copy(particleMV, model, latents, latents, index)
            calculate(model, latentDep)
            copy(from = model, to = mvSaved, nodes = latentDep, row = 1, logProb = TRUE)
        }
        else if(!jump & latentSamp) {
            ## if we don't jump, replace model latent nodes with saved latent nodes
            copy(from = mvSaved, to = model, nodes = latentDep, row = 1, logProb = TRUE)
        }
        if(jump & !resample)  storeParticleLP <<- particleLP
        if(jump & optimizeM) optimM()
        if(adaptive)     adaptiveProcedure(jump)
    },
    methods = list(
        optimM = function() {
            tempM <- 15000
            declare(LLEst, double(1, nVarReps))
            if(nVarEsts < mBurnIn) {  # checks whether we have enough var estimates to get good approximation
                for(i in 1:nVarReps)
                    LLEst[i] <- my_particleFilter$run(tempM)
                ## next, store average of var estimates
                if(nVarEsts == 1)
                    storeLLVar <<- var(LLEst)/mBurnIn
                else {
                    LLVar <- storeLLVar
                    LLVar <- LLVar + var(LLEst)/mBurnIn
                    storeLLVar <<- LLVar
                }
                nVarEsts <<- nVarEsts + 1
            }
            else {  # once enough var estimates have been taken, use their average to compute m
                m <<- m*storeLLVar/(0.92^2)
                m <<- ceiling(m)
                if(!resample){  #reset LL with new m value after burn-in period
                  storeParticleLP <<- my_particleFilter$run(m)
                }
                optimizeM <<- 0
            }
        },
        generateProposalVector = function() {
            propValueVector <- rmnorm_chol(1, values(model,target), chol_propCov_scale, 0)  ## last argument specifies prec_param = FALSE
            returnType(double(1))
            return(propValueVector)
        },
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(!adaptScaleOnly)     empirSamp[timesRan, 1:d] <<- values(model, target)
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                adaptFactor <- my_calcAdaptationFactor$run(acceptanceRate)
                scale <<- scale * adaptFactor
                ## calculate empirical covariance, and adapt proposal covariance
                if(!adaptScaleOnly) {
                    gamma1 <- my_calcAdaptationFactor$gamma1
                    for(i in 1:d)     empirSamp[, i] <<- empirSamp[, i] - mean(empirSamp[, i])
                    empirCov <- (t(empirSamp) %*% empirSamp) / (timesRan-1)
                    propCov <<- propCov + gamma1 * (empirCov - propCov)
                    chol_propCov <<- chol(propCov)
                }
                chol_propCov_scale <<- chol_propCov * scale
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        reset = function() {
            scale   <<- scaleOriginal
            propCov <<- propCovOriginal
            chol_propCov <<- chol(propCov)
            storeParticleLP <<- -Inf
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            my_calcAdaptationFactor$reset()
        }
    ), where = getLoadingNamespace()
)

