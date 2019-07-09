## set.seed(0)
## n <- 1e6
## path <- rep(0, n)
## rho1 <- .8
## rho2 <- .1
## path[1:2] <- rnorm(2)
## print(system.time(
## for(i in 3:n)
##       path[i] <- rho1*path[i-1] + rho2*path[i-2] + rnorm(1)
## ))
## ts.plot(path[1:5000])
## mc <- nimbleFunction(
##    run = function( ... ) ) {
##        returnType( ... )
##        ...
##        return(...)
## })
## cmc <- compileNimble(mc)
## set.seed(1)
## system.time(path <- cmc(n, rho1, rho2))
