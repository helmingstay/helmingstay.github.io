library(RcppArmadillo)
library(Rcpp)
sourceCpp("helpers.cpp")

## helper functions
gen.lengths <- function(particles, ntrials=3, prob=0.5) {
    ## a vector of random draws
    pmax(1, rnbinom(particles, ntrials, prob))
}
gen.dat <- function(nelem, rate=1) {
    ## a list of vectors, vector i has length nelem[i]
    ## each vector is filled with random draws
    lapply(nelem, rexp, rate=rate)
}

## naive method
appendL <- function(new.dat, new.lengths, dat, dat.lengths) {
    ## grow dat by appending to list element i
    ## memory will be reallocated at each call
    dat <- mapply( append, dat, new.dat )
    ## update lengths
    dat.lengths <- dat.lengths + new.lengths
    return(list(dat=dat, len=dat.lengths))
}

## dynamically append to preallocated matrix
## maintain a vector of the number of "filled" elements in each row
## emit error if overfilled
## R solution
appendR <- function(new.dat, new.lengths, dat, dat.lengths) {
    ## grow pre-allocated dat by inserting data in the correct place
    for (irow in 1:length(new.dat)) {
        ## insert one vector at a time
        ## col indices for where to insert new.dat
        cols.ii <- (dat.lengths[irow]+1):(dat.lengths[irow]+new.lengths[irow])
        dat[irow, cols.ii] = new.dat[[irow]]
    }
    ## update lengths
    dat.lengths <- dat.lengths + new.lengths
    return(list(dat=dat, len=dat.lengths))
}

## unit test
test.correct.append <- function(nrep, particles=1e3, max.cols=1e3, do.c=F) {
    ## list of empty vectors, fill with append
    dat.list <- lapply(1:particles, function(x) numeric())
    ## preallocated matrix, fill rows from left to right
    dat.r <- dat.c <- matrix(numeric(), nrow=particles, ncol=max.cols)
    ## length of each element/row
    N.list <- N.r <- N.c <- rep(0, particles)
    ## repeat process, "appending" as we go
    for (ii in 1:nrep) {
        N.new <- gen.lengths(particles)
        dat.new <- gen.dat(N.new)
        ## in R, list of vectors
        tmp <- appendL(dat.new, N.new, dat.list, N.list)
        ## unpack, update
        dat.list <- tmp$dat
        N.list <- tmp$len
        ## in R, preallocate
        tmp <- appendR(dat.new, N.new, dat.r, N.r)
        ## unpack, update
        dat.r <- tmp$dat
        N.r <- tmp$len
        ## as above for C, modify dat.c and N.c in place
        appendRcpp(dat.new, N.new, dat.c, N.c)
    }
    ## pull pre-allocated data back into list
    dat.r.list <- apply(dat.r, 1, function(x) { x <- na.omit(x); attributes(x) <- NULL; x } )
    ## check that everything is  
    identical(dat.r, dat.c) && identical(N.r, N.c) &&
    identical(dat.list, dat.r.list) && identical(N.list, N.r)
}

## timing function, test each method
test.time.append <- function(nrep, particles=1e2, max.cols=1e3, append.fun, do.update=T, seed=2) {
    ## object to modify
    N.test <- rep(0, particles)
    dat.test <- matrix(numeric(), nrow=particles, ncol=max.cols)
    ## speed is affected by size, 
    ## so ensure that each run the same elements
    set.seed(seed)
    for (irep in 1:nrep) {
        ## generate draws
        N.new <- gen.lengths(particles)
        dat.new <- gen.dat(N.new)
        ## bind in using given method
        tmp <- append.fun(dat.new, N.new, dat.test, N.test)
        if(do.update) {
            ## skip update for C
            dat.test <- tmp$dat
            N.test <- tmp$len
        }
    }
}


## run unit test, verify both functions return identical results
is.identical <- test.correct.append(1e1, max.cols=1e3)
print(is.identical)

library(rbenchmark)
## test timings of each solution
test.nreps <- 10
test.ncols <- 1e3
timings = benchmark(
    r=test.time.append(test.nreps, max.cols=test.ncols, append.fun=appendR),
    c=test.time.append(test.nreps, max.cols=test.ncols, do.update=F, append.fun=appendRcpp),
    list=test.time.append(test.nreps, max.cols=test.ncols, append.fun=appendL),
    replications=10
)

## test fast solutions with larger data structures
test.nreps <- 1e2
test.ncols <- 1e4
timings.fast = benchmark(
    r=test.time.append(test.nreps, max.cols=test.ncols, append.fun=appendR),
    c=test.time.append(test.nreps, max.cols=test.ncols, do.update=F, append.fun=appendRcpp),
    replications=1e1
)
