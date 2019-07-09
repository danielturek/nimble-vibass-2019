# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found
library(nimble)
read_chunk('chunks_litters_marginal.R')
objective <- nimbleFunction(
    setup = function(model) {
          # ordinarily we would do stuff here, but in this case
          # we only need make the nimbleFunction aware of the model
          },
    run = function(par = double(1)) {
        returnType(double(0))
        model[['a']] <<- exp(par[1:2])
        model[['b']] <<- exp(par[3:4])
        ans <- model$calculate()
        return(ans)
    }
)
# so attendees can run code below this without using code from other modules
# if(!exists('littersMargModel') || !exists('cLittersMargModel')) source('chunks_litters_marginal.R')
# not clear why dbetabin() not being put into global
# if this isn't done, registerDistributions fails to find dbetabin in knitr
assign('dbetabin', dbetabin, .GlobalEnv)
assign('rbetabin', rbetabin, .GlobalEnv)
rObjective <- objective(littersMargModel)
## cObjective <- compileNimble(rObjective, project = littersMargModel)  ## remember to compile model first
cLittersMargModel <- compileNimble(littersMargModel)
cObjective <- compileNimble(rObjective, project = littersMargModel)
system.time(optR <- optim(log(rep(1,4)), rObjective$run, control = list(fnscale = -1)))
system.time(optC <- optim(log(rep(1,4)), cObjective$run, control = list(fnscale = -1)))
optR
optC
exp(optC$par)
objective <- nimbleFunction(
    setup = function(model, target) {
          ## we'll start putting stuff here soon, I promise!
          },
    run = function(par = double(1)) {
        returnType(double(0))
        values(model, target) <<- exp(par)
        ans <- model$calculate()
        return(ans)
    }
)
objective <- nimbleFunction(
    setup = function(model, target) {
          calcNodes <- model$getDependencies(target)
          },
    run = function(par = double(1)) {
        returnType(double(0))
        values(model, target) <<- exp(par)
        ans <- model$calculate(calcNodes)
        return(ans)
    }
)
rObjective <- objective(littersMargModel, c('a', 'b'))  ## or c('a[1]','a[2]','b[1]','b[2]')
cObjective <- compileNimble(rObjective, project = littersMargModel)
optC <- optim(log(rep(1,4)), cObjective$run, control = list(fnscale = -1))
optC
