varLapply <- function(X, FUN,               # same syntax as base::lapply
                      use_parallel = TRUE,  # use parLapply by default
                      number_of_nodes = detectCores()-1,  # keep 1 core free by default
                      ...) {
    if (!use_parallel) {
        lapply(X, FUN, ...)
    } else {
        require(parallel)
        temp_cluster <- makeForkCluster(nnodes = max(number_of_nodes,1))
        clusterExport(cl = cluster, varlist = c(), envir = parent.frame())
        output <- parLapply(cl = cluster, X = X, fun = FUN, ...)
        stopCluster(temp_cluster)
        return(output)
    }
}

library(plyr); library(dplyr); library(magrittr); library(ggplot2)
library(parallel)
library(numbers)  # for the isPrime function

prime_test <- function(N,   # the integers we test for primality
                       K,   # the maximum number of times each loop tests N
                       P) { # the number of times each test is repeated for averaging
    L <- length(N)
    S <- L*K*P*2
    tests <- data.frame(matrix(nrow = S, ncol = 5))
    colnames(tests) <- c("n", "k", "p", "b", "time")
    i <- 1
    for (n in 1:L) {
        for (k in 1:K) {
            for (p in 1:P) {
                for (b in c(FALSE, TRUE)) {
                    tests[i,] <- c(N[n], k, p, b, 0.0)
                    i <- i+1
                }}}}
    # each row of tests corresponds to a test unit where we run
    # isPrime(n) k times using varLapply(..., use_parallel = b);
    # output of system.time (elapsed) is stored in the time column;
    # each case is repeated P times for averaging;
    # we sample test units in random order to reduce bias potential
    for (j in sample(1:S)) { 
        tests[j,"time"] <- system.time(varLapply(X = rep(tests[i,"n"], tests[i,"k"]),
                                                 FUN = isPrime,
                                                 use_parallel = tests[i,"b"])
        )[3]
        gc()
    }
    return(tests)
}

large_test <- prime_test(N = 10:15, K = 10, P = 2)
large_test %>% head