library(dplyr); library(magrittr); library(ggplot2); library(reshape2)
load("./test_2.Rdata")

### exploratory analysis

ggplot(data = test_1, aes(x = sqrt(n), y = time, color = factor(b))) + 
    geom_point() + facet_wrap(facets = b~k, scales = "free", ncol = 6) +
    geom_smooth(method = "loess", color = "purple", size = 1)

### algorithmic/grid model selection

cutoff_n <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.50)
sub_test <- test_1 %>% mutate(fact_k = factor(k), b = factor(b), train = (n <= cutoff_n))
alphas   <- seq(from = 0, to = 1,  by = 0.1)
betas    <- seq(from = 0, to = -4, by = -0.5)
grid     <- data.frame(alpha = rep(alphas, length(betas)), 
                       beta  = rep(betas, each = length(alphas)),
                       Rsqd  = 0,
                       SSE   = 0,
                       SSEadj= 0)
for (i in 1:dim(grid)[1]) {
    alpha   <- grid[i,"alpha"]
    beta    <- grid[i,"beta"]
    temp    <- sub_test %>% mutate(newvar = n^alpha * log(n)^beta)
    fit_cut <- lm(data = temp %>% filter(train), time ~ newvar * fact_k * b)
    fit_all <- lm(data = temp, time ~ newvar * fact_k * b)
    grid[i,"Rsqd"]    <- fit_all %>% summary %>% .$r.squared
    grid[i,"SSE"]     <- sum(  (predict(fit_cut, newdata = temp) - temp[,"time"])^2 )
    grid[i,"SSEadj"]  <- 10^6 * sum( ((predict(fit_cut, newdata = temp) - temp[,"time"])/temp[,"k"])^2 )
}
grid[which.max(grid$Rsqd)  ,]  # alpha = 0.5 ; beta = -2
grid[which.min(grid$SSE)   ,]  # alpha = 0.6 ; beta = -4
grid[which.min(grid$SSEadj),]  # alpha = 0.6 ; beta = -4
grid %>% filter(alpha > 0.4, alpha < 0.7) %>% filter(beta >= -4, beta <= -2)

# So alpha = 0.5 / beta = -2 wins; we re-process our initial charts

ggplot(data = test_1, aes(x = sqrt(n)/log(n)^2, y = time, color = factor(b))) + 
    geom_point() + facet_wrap(facets = b~k, scales = "free_y", ncol = 6) +
    geom_smooth(method = "loess", color = "purple", size = 1)

rm(list = ls()); load("./test_2.Rdata")

### fitting both models on n < median(n) and checking fit on n > median(n)

cutoff_n    <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.75)
sub_test_2  <- test_1 %>% 
                    filter(b == 0, k %in% c(50, 1000)) %>% 
                    mutate(newvar1 = k * sqrt(n), 
                           newvar2 = k * sqrt(n)/log(n), 
                           newvar3 = k * sqrt(n)/log(n)^2,
                           fact_k  = factor(k),
                           train = n < cutoff_n)
ggplot(data = sub_test_2, aes(x = n, y = n, color = train)) + geom_point()
ggplot(data = sub_test_2, aes(x = newvar3, y = time, color = fact_k)) + geom_point() + geom_smooth(method = "lm")

model_4 <- lm(data = sub_test_2 %>% filter(train), time ~ newvar1 + fact_k)
model_5 <- lm(data = sub_test_2 %>% filter(train), time ~ newvar2 + fact_k)
model_6 <- lm(data = sub_test_2 %>% filter(train), time ~ newvar3 + fact_k)  # the interaction term is not significant

lapply(list(model_4,model_5,model_6), . %>% summary %>% .$r.squared)  # 0.996 ; 0.998 ; 0.999

lapply(list(model_4,model_5,model_6), function(m) sum((predict(m, newdata = sub_test_2) - sub_test_2[,"time"])^2))  # 9.7 ; 3.2 ; 0.5

preds   <- sub_test_2 %>% data.frame(., 
                                     pred_4 = predict(model_4, .), 
                                     pred_5 = predict(model_5, .),
                                     pred_6 = predict(model_6, .))
                                     
ggplot(data = preds %>% filter(k == 1000) %>% select(-(k:b), -fact_k, -train) %>% melt(id.vars = c("n","newvar1","newvar2","newvar3")),
       aes(x = newvar1, y = value, color = variable)) + 
    geom_point() +
    geom_vline(xintercept = 1000*sqrt(cutoff_n), color = "purple") 

ggplot(data = preds %>% select(time, pred_4, pred_5, pred_6) %>% 
           melt(id.vars = c("time")),
       aes(x = time, y = value, color = variable)) + 
    geom_point() + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange")

rm(list = ls()); load("./test_2.Rdata")

## checking model_6 with all values of k

sub_test_3 <- test_1 %>% filter(b == 0) %>% mutate(newvar = k * sqrt(n)/log(n)^2, fact_k = factor(k))
           
model_7 <- lm(data = sub_test_3, time ~ newvar + k)
model_8 <- lm(data = sub_test_3, time ~ newvar * k)  # even if newvar:l slope is sig., its magnitude is very small
summary(model_7)

preds <- sub_test_3 %>% data.frame(., pred_7 = predict(model_7, .))

ggplot(data = preds, aes(x = log(time), y = log(pred_7), color = factor(k))) + 
    geom_point() + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange")

ggplot(data = preds %>% select(n, fact_k, time, pred_7) %>% 
           melt(id.vars = c("n", "fact_k")),
       aes(x = n, y = value, color = variable)) + 
    geom_point() + 
    geom_smooth() +
    facet_wrap(~ fact_k, scales = "free_y")

rm(list = ls()); load("./test_2.Rdata")

## going into b == 1 now

cutoff_n   <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.85)
sub_test_4 <- test_1 %>% filter(b == 1) %>% select(-p, -b) %>% mutate(newvar = k * sqrt(n)/log(n)^2, 
                                                                      fact_k = factor(k),
                                                                      train = n < cutoff_n)

ggplot(data = sub_test_4, aes(x = newvar, y = time, color = factor(k))) + geom_point() + geom_smooth()

model_9  <- lm(data = sub_test_4 %>% filter(train), time ~ newvar + k)
model_10 <- lm(data = sub_test_4 %>% filter(train), time ~ newvar * k)
model_11 <- lm(data = sub_test_4 %>% filter(train), time ~ newvar + fact_k)
model_12 <- lm(data = sub_test_4 %>% filter(train), time ~ newvar * fact_k)
summary(model_10)
summary(model_12)
anova(model_11, model_12)

lapply(list(model_9,model_10,model_11,model_12), . %>% summary %>% .$r.squared)  # 0.991 ; 0.994 ; 0.992 ; 0.994
lapply(list(model_9,model_10,model_11,model_12), 
       function(m) sum((predict(m, newdata = sub_test_4) - sub_test_4[,"time"])^2))  # .40 ; .28 ; .34 ; .28

preds <- sub_test_4 %>% data.frame(., 
                                   pred_9  = predict(model_9, .), 
                                   pred_10 = predict(model_10, .),
                                   pred_11 = predict(model_11, .),
                                   pred_12 = predict(model_12, .))

ggplot(data = preds %>% select(newvar, fact_k, time, pred_10, pred_12) %>% 
           melt(id.vars = c("newvar", "fact_k")),
       aes(x = newvar, y = value, color = variable)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    facet_wrap(~ fact_k, scales = "free")

ggplot(data = preds, aes(x = log(time), y = log(pred_12), color = factor(k))) + 
    geom_point() + geom_smooth(method = "lm") + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange")

ggplot(data = preds, aes(x = log(pred_10), y = log(pred_12), color = fact_k)) + 
    geom_point() + geom_abline(aes(intercept = 0, slope = 1), color = "orange")

rm(list = ls()); load("./test_2.Rdata")

## mixing everything

cutoff_n   <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.85)
sub_test_5 <- test_1 %>% select(-p) %>% mutate(newvar = k * sqrt(n)/log(n)^2, 
                                               fact_k = factor(k),
                                               fact_b = factor(b),
                                               train = n < cutoff_n)

model_13 <- lm(time ~ newvar * fact_k * fact_b, data = sub_test_5)
summary(model_13)
model_14 <- lm(time ~ newvar * fact_b + fact_k, data = sub_test_5)
summary(model_14)
anova(model_14, model_13)
model_15 <- lm(time ~ newvar * fact_b + k, data = sub_test_5)

preds <- sub_test_5 %>% data.frame(., 
                                   pred_13 = predict(model_13, .), 
                                   pred_14 = predict(model_14, .)) %<>% 
                        mutate(res_13  = pred_13 - time,
                               res_14  = pred_14 - time)
                                   
lapply(list(model_13,model_14), . %>% summary %>% .$r.squared)  # 0.999 ; 0.998
lapply(list(model_13,model_14), 
       function(m) sum((predict(m, newdata = sub_test_5) - sub_test_5[,"time"])^2))  # .72 ; 1.63

ggplot(data = preds, aes(x = log(pred_13), y = log(pred_14), color = fact_b)) + 
    geom_point() + geom_abline(aes(intercept = 0, slope = 1), color = "orange")

ggplot(data = preds %>% select(newvar, fact_k, fact_b, res_13, res_14) %>% 
           melt(id.vars = c("newvar", "fact_k", "fact_b")),
       aes(x = newvar, y = value, color = variable)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(fact_b ~ fact_k, scales = "free", ncol = 5) +
    ggsave(filename = "./plot_x.pdf", width = 15, height = 8)

sum_15 <- model_15 %>% summary
coeffs <- sum_15$coefficients
coeffs[2,1]/(coeffs[2,1] + coeffs[5,1])  # 5.1x












