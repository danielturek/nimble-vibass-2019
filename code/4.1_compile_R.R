# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
library(methods)  # otherwise new() not being found
library(nimble)
## out <- exp(cos(sin(x)) + x^3)
nimMath1 <- nimbleFunction(
       run = function(x = double(1)) {
           returnType(double(1))
           n <- length(x)
           # some functions, like numeric, mimic R
	   # but also may have additional/different features
           out <- numeric(n, init = FALSE)
           # core computation
           for( i in 1:n) 
                out[i] <- exp(cos(sin(x[i])) + x[i]^3)
           return(out)
})
cNimMath1 <- compileNimble(nimMath1)
nimMath2 <- nimbleFunction(
       run = function(x = double(1)) {
           returnType(double(1))
           out <- exp(cos(sin(x)) + x^3)
           return(out)
})
cNimMath2 <- compileNimble(nimMath2)
x <- rnorm(1e6)
library(rbenchmark)
benchmark(out0 <- exp(cos(sin(x)) + x^3),
               out1 <- cNimMath1(x),
               out2 <- cNimMath2(x),
                columns = c('test','replications','elapsed'),
               replications = 10)
set.seed(1)
M <- 1000000
alphas <- c(-3, -0.5, -0.25, .1, .15, .29, .4, .45)  ## i.e., Xbeta from previous slide
K <- length(alphas)
system.time({
        # generate W_k ~ N(alpha_k, 1)
        rands <- matrix(rnorm(M*K), nrow = K, ncol = M)
        props <- rep(0, K)
        tmp <- alphas + rands # exploit vectorization
        # now tally the results
        id <- apply(tmp, 2, which.max)
        tbl <- table(id)
        props[as.integer(names(tbl))] <- tbl / M
        props
})
mprobit <- nimbleFunction(
         run = function(alphas = double(1), M = double(0)) {
             returnType(double(1))
             K <- length(alphas)
             props <- numeric(K, value = 0)
             w <- numeric(K, init = FALSE)
             for(m in 1:M) {
                   for(k in 1:K) 
                        w[k] <- alphas[k] + rnorm(1) 
                   maxind <- 1
                   max <- w[1]
                   for(k in 2:K) {
                        if(w[k] > max){
                                maxind <- k
                                max <- w[k]          
                        }
                   }
                   props[maxind] <- props[maxind] + 1
             }
             props <- props/M
             return(props)
         }
)
mprobitVec <- nimbleFunction(
         run = function(alphas = double(1), M = double(0)) {
             returnType(double(1))
             K <- length(alphas)
             props <- numeric(K, value = 0)
             for(m in 1:M) {
                   w <- alphas + rnorm(K)
                   mx = max(w)
                   maxind = which(w == mx)                   
                   props[maxind] <- props[maxind] + 1
             }
             props <- props/M
             return(props)
         }
)
cmprobit = compileNimble(mprobit)
cmprobitVec = compileNimble(mprobitVec)
set.seed(1)
system.time(
props2 <- cmprobit(alphas, M)
)
system.time(
props3 <- cmprobitVec(alphas, M)
)
