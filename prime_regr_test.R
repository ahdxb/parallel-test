library(dplyr); library(magrittr); library(ggplot2)
library(parallel)
library(numbers)

varLapply <- function(X, FUN,               # same syntax as base::lapply
                      use_parallel = TRUE,  # use parLapply by default
                      number_of_nodes = detectCores()-1,  # keep 1 core free by default
                      par_apply_function = "parLapply",   # allows to switch to mclapply
                      ...) {
    if (!use_parallel) {
        lapply(X, FUN, ...)
    } else {
        require(parallel)
        if (par_apply_function == "parLapply") {
            tmp_cluster <- makeForkCluster(nnodes = max(number_of_nodes,1))
            clusterExport(cl = tmp_cluster, varlist = c(), envir = parent.frame())
            output <- parLapply(cl = tmp_cluster, X = X, fun = FUN, ...)
            stopCluster(tmp_cluster)
            return(output)
        } else if (par_apply_function == "mclapply") {
            mclapply(X = X, FUN = FUN, mc.cores = number_of_nodes)
        }
    }
}

prime_test <- function(N,   # a vector, the integers we test for primality
                       K,   # a vector, the lengths of the [l/parL/mcl]apply loops
                       P) { # an integer, the number of times each test is repeated for averaging
    N.l <- length(N)
    K.l <- length(K)
    S.l <- N.l * K.l * P * 2
    tests <- data.frame(matrix(NA, nrow = S.l, ncol = 5))
    colnames(tests) <- c("n", # the integer tested for primality
                         "k", # the number of times n is tested
                         "p", # the number of times each test is repeated
                         "b", # if (b), uses a parallel version of lapply
                         "time") # the measured test time (elapsed in system.time)
    i <- 1
    for (n in 1:N.l) {
        for (k in 1:K.l) {
            for (p in 1:P) {
                for (b in c(FALSE, TRUE)) {
                    tests[i,] <- c(N[n], K[k], p, b, 0.0)
                    i <- i+1
                }}}}
    # each row of tests corresponds to a test unit where we run
    # isPrime(n) k times using varLapply(..., use_parallel = b);
    # output of system.time (elapsed) is stored in the time column;
    # each case is repeated P times for averaging;
    # we sample test units in random order to reduce bias potential
    for (j in sample(1:S.l)) { 
        tests[j,"time"] <- system.time(varLapply(X = rep(tests[j,"n"], tests[j,"k"]),
                                                 FUN = isPrime,
                                                 use_parallel = tests[j,"b"], 
                                                 par_apply_function = "mclapply")
        )[3]
        gc()
    }
    return(tests)
}

set.seed(123456)
rdm_primes <- Primes(10^1,10^8) %>% sample(size = 100)
loop_sizes <- c(100)
test_2     <- prime_test(N = rdm_primes, K = loop_sizes, P = 4)

load("./test_1.Rdata")

