library(plyr); library(dplyr); library(magrittr)
library(caret)
library(parallel); library(doParallel)
library(ggplot2)

###### function definitions ######

varLapply <- function(X, FUN, 
                      parallel   = TRUE, 
                      # cluster    = makeCluster(min(detectCores()-2,1)),
                      # exportVars = ls(), 
                      ...) {
    if (!parallel) {
        lapply(X, FUN, ...)
    } else {
        require(parallel)
        cluster <- makeForkCluster(nnodes = max(detectCores()-2,1))
        clusterExport(cl = cluster, varlist = c(), envir = parent.frame())
        result <- parLapply(cl = cluster, X = X, fun = FUN, ...)
        stopCluster(cluster)
        return(result)
    }
}

quick_model <- function(modeltype, classe_set, datasource) {
    ## quick_model just allows us to factor some cleaning code and 
    ## pass some custom options
    modeltype_i <- c("binomial", "tree",  "random_forest", "svm_linear", "svm_radial")
    modeltype_o <- c("glm",      "rpart", "rf",            "svmLinear",  "svmRadial" )
    modeltype_2 <- modeltype_o[which(modeltype == modeltype_i)]
    ## we replace i-th classe value by "IN" (resp. "OUT") if classe(i) %in% classes
    ## to please caret's train
    datasource[,"classe"] <- sapply(datasource$classe %in% classe_set, 
                                    function(b) if (b) {"IN"} else {"OUT"}) %>% factor
    if (modeltype %in% c("binomial","tree","random_forest")) {
        train(classe ~ ., method = modeltype_2, data = datasource,
              preProc = "scale")
    } else if (modeltype %in% c("svm_linear","svm_radial")) {
        train(classe ~ ., method = modeltype_2, data = datasource, 
              preProc = c("center","scale"), trControl = trainControl(classProbs=TRUE))
    } else {
        message("unsupported modeltype")
    }
}
predict_probas <- function(models, datasource, 
                           parallel = TRUE, cluster = cluster <- makeCluster(detectCores()-2)) {
    ## takes a list of N models and a training/test set
    ## returns a vector of N probabilities with col names P1, P2, ..., PN
    if (parallel) {
        probas_list <- parLapply(cl = cluster, 
                                 X = models, 
                                 fun = function(m) predict(m, newdata=datasource, type="prob")["IN"])    
    } else {
        probas_list <- lapply(X = models, 
                              FUN = function(m) predict(m, newdata=datasource, type="prob")["IN"])
    }
    probas_df <- do.call(cbind, probas_list) # convert the list into a data frame
    colnames(probas_df) <- sapply(X = 1:length(models), 
                                  FUN = function(i) paste0("P",i))
    probas_df
}
stack_voter <- function(probs) { 
    ## takes 5 probabilities P1, ..., P5 and returns 
    ## the letter A...E corresponding to the highest probability
    sapply(X = apply(X = probs, FUN = which.max, MARGIN = 1),
           FUN = function(x) LETTERS[1:5][x]) 
}
stacked_model <- function(modeltype, classe_set_list, datasource, method = "gbm",
                          parallel = TRUE, cluster = cluster <- makeCluster(detectCores()-2)) {
    ## trains a list of models of type modeltype to estimate the probabilities
    ## P1 = P(x in classes[1]) ... PN = P(x in classes[N]), then stacks a 
    ## boosting model on top 
    model_list  <- lapply(X = classe_set_list, FUN = function(c) quick_model(modeltype, c, datasource))
    train_probs <- predict_probas(model_list, datasource)
    data_boost  <- data.frame(classe = datasource$classe, train_probs)
    ifelse(method == "gbm",
           classifier  <- train(classe ~ ., method = method, data = data_boost, verbose=FALSE),
           classifier  <- train(classe ~ ., method = method, data = data_boost) )
    list(models = model_list, classifier = classifier)
}
predict_stacked <- function(stacked_model, datasource) {
    predict(stacked_model$classifier, 
            newdata = predict_probas(stacked_model$models, datasource))
}
accuracy_print <- function(train_preds, train_true, test_preds, test_true) {
    data.frame(Training_Accuracy = mean(train_preds == train_true),
               Test_Accuracy     = mean(test_preds  == test_true)) %>% round(3) %>% print
}

###### importing data ######

data_files <- c("../pml-training-v2.txt", "../pml-testing-2.txt")
params     <- list(c(rep("NULL", 7), rep("numeric", 152), "factor"), 
                   c('""', "NA", "#DIV/0!") )
train      <- read.csv(data_files[1], sep=",", colClasses=params[[1]], na.strings=params[[2]])
train      %<>% select(-contains("_x"), -contains("_y"), -contains("_z"))
na_indices <- sapply(X = 1:dim(train)[2], FUN = function(i) !anyNA(train[,i]))
train      <- train[,na_indices]
set.seed(11111)
train_indices <- createDataPartition(y = train$classe, p = 0.6, list=FALSE)
train_xv      <- train[ train_indices,] # 11776 observations
train         %<>% sample_n(size = 5000)
test_xv       <- train[-train_indices,] #  7846 observations
rm(na_indices, params, data_files, train, train_indices)

###### setting parralel options ######

?makeCluster()
detectCores() # 8
summary(cluster)

###### save and load variables ######

save(list = ls(), file = "./env_save.Rdata")
rm(list = ls())
load("./env_save.Rdata")

###### testing 1 ######

binom_rf_test <- function() {
    small_class_list <- as.list(LETTERS[1:5])
    binomial_boost <- stacked_model(modeltype = "binomial", 
                                    classe_set_list = small_class_list,
                                    datasource = train_xv,
                                    method = "rpart")
    train_predict <- predict(binomial_boost$classifier)
    test_predict  <- predict_stacked(binomial_boost, test_xv)
}

system.time(binom_rf_test())

cluster <- makeCluster(detectCores())
registerDoParallel(cluster)
system.time(binom_rf_test())
stopCluster(cluster)

###### testing 2 ######

system.time({
    lapply(X = 1:10000000, FUN = function(j) sqrt(j)*exp(j)*log(j))
})

cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)
system.time({
    parLapply(cl = cluster, X = 1:10000000, fun = function(j) sqrt(j)*exp(j)*log(j))
})
stopCluster(cluster)

###### test 3 ######

library(numbers)

N1 <- 10^4
K <- 500
M <- 5
times <- data.frame(size = rep(NA,2*K), case = rep(NA,2*K), time = rep(NA,2*K))
for (i in 1:K) {
    for (b in c(TRUE,FALSE)) {
        times[K*b+i,"size"] <- M*i
        times[K*b+i,"case"] <- b
        times[K*b+i,"time"] <- system.time(varLapply(X = N1:(N1+M*i),
                                                     FUN = isPrime,
                                                     parallel = b))[3]
        gc()
    }
}
ggplot(data = times, aes(x = size, y = time, colour = case) ) + geom_line() + geom_smooth(method = "loess")

varLapply(X = N1:N2, 
          FUN = function (n) isPrime(n),
          parallel = T) 




