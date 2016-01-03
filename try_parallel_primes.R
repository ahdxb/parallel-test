library(plyr); library(dplyr); library(magrittr)
library(parallel)
library(numbers)
library(ggplot2)

varLapply <- function(X, FUN, parallel = TRUE, ...) {
    if (!parallel) {
        lapply(X, FUN, ...)
    } else {
        require(parallel)
        cluster <- makeForkCluster(nnodes = max(detectCores()-1,1))
        clusterExport(cl = cluster, varlist = c(), envir = parent.frame())
        result <- parLapply(cl = cluster, X = X, fun = FUN, ...)
        stopCluster(cluster)
        return(result)
    }
}

N <- 10^3   # lower end of the range of integers to be tested
K <- 100    # number of, and max size of, ranges to be tested
M <- 10     # range step for integer testing
P <- 3      # number of identical repeats (for averaging)
empty_block <- rep(NA,2*P*K)
times <- data.frame(p = empty_block, k = empty_block, b = empty_block, 
                    size = empty_block, case = empty_block, time = empty_block, iter = empty_block)
for (p in 1:P) {
    for (k in 1:K) {
        for (b in c(TRUE,FALSE)) {
            index <- 2*K*(p-1) + b*K + k
            times[index, c("p","k","b")] <- c(p,k,b)
        }
    }
}
for (n in sample(1:(2*P*K))) {
    p <- times[n,"p"]
    k <- times[n,"k"]
    b <- (times[n,"b"] == TRUE)
    index <- 2*K*(p-1) + b*K + k
    times[index,"size"] <- M*k
    times[index,"case"] <- b
    times[index,"iter"] <- p
    times[index,"time"] <- system.time(varLapply(X = N:(N+M*k),
                                                 FUN = isPrime,
                                                 parallel = b))[3]
    gc()
}
times %<>% select(-(p:b))
chart_filename <- paste0("test-parallel-primes--",
                         "N-", N, "--",
                         "K-", K, "--",
                         "M-", M, "--",
                         "P-", P,
                         ".pdf")
ggplot(data = times, aes(x = size, y = time, colour = case) ) + 
    geom_point() + geom_smooth(method = "lm") +
    ggsave(filename = chart_filename)
    
fit_model <- lm(time ~ size * case, data = times)
data_filename <- paste0("test-parallel-primes--",
                        "N-", N, "--",
                        "K-", K, "--",
                        "M-", M, "--",
                        "P-", P,
                        ".Rdata")
save(times, fit_model, file = data_filename)
    
    
