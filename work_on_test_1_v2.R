library(dplyr); library(magrittr); library(ggplot2); library(reshape2)
rm(list = ls()); load("./test_2.Rdata")

##### I. exploratory analysis

ggplot(data = test_1, aes(x = sqrt(n), y = time, color = factor(b))) + 
    geom_point() + 
    facet_wrap(facets = b~k, scales = "free", ncol = 6) +
    geom_smooth(method = "loess", color = "purple", size = 1)

##### II. algorithmic/grid model selection

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

##### III. fitting model and picking parameters for (b == 0)

cutoff_n   <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.50)
sub_test_2 <- test_1 %>% filter(b == 0) %>% 
                         mutate(newvar = sqrt(n)/log(n)^2, fact_k = factor(k), train = (n < cutoff_n)) %>%
                         select(-b, -p)
           
model_a <- lm(data = sub_test_2, time ~ I(newvar * k) + k)
model_b <- lm(data = sub_test_2, time ~ newvar * fact_k)  # basically a much more flexible version of model_a
summary(model_a)
summary(model_b)  # does not really bring anything to the table, so will stick to model_a

preds <- sub_test_2 %>% data.frame(., pred_a = predict(model_a, .))
preds %>% head

# chart 1A: predicted versus actual (all k's in one)
ggplot(data = preds, aes(x = log(time), y = log(pred_a) - log(time), color = fact_k)) + 
    geom_point(alpha = 0.2) + 
    geom_abline(aes(intercept = 0, slope = 0), color = "orange")

# chart 1B: predicted versus actual (facets ~ k)
ggplot(data = preds, aes(x = time, y = pred_a, color = train)) + 
    geom_point(alpha = 0.2) + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange") + 
    facet_wrap(~ fact_k, ncol = 3, scales = "free")

rm(list = ls()); load("./test_2.Rdata")

##### IV. b == 1 now

cutoff_n   <- test_1 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.50)
sub_test_3 <- test_1 %>% filter(b == 1) %>% 
                         mutate(newvar = sqrt(n)/log(n)^2, fact_k = factor(k), train = (n < cutoff_n)) %>%
                         select(-b, -p)

model_c <- lm(data = sub_test_3, time ~ I(newvar * k) + k)
model_d <- lm(data = sub_test_3, time ~ newvar * fact_k)  # basically a much more flexible version of model_a
summary(model_c)
summary(model_d)  # difference is more pronounced than c vs. d

preds <- sub_test_3 %>% data.frame(., pred_c = predict(model_c, .), pred_d = predict(model_d, .))
preds %>% head

# chart 2A: predicted versus actual (all k's in one)
ggplot(data = preds %>% select(fact_k, time, pred_c, pred_d) %>%
                        melt(id.var = c("time", "fact_k")),
       aes(x = log(time), y = log(value), color = variable)) + 
    geom_point(alpha = 0.2) + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange")

# chart 2B: predicted versus actual (facets ~ k)
ggplot(data = preds %>% select(fact_k, time, pred_c, pred_d) %>%
           melt(id.var = c("time", "fact_k")),
       aes(x = log(time), y = log(value), color = variable)) + 
    geom_point(alpha = 0.2) + 
    geom_abline(aes(intercept = 0, slope = 1), color = "orange") +
    facet_wrap(~ fact_k, ncol = 3, scales = "free")

mc_coef <- model_c %>% .$coefficients
md_coef <- model_d %>% .$coefficients
newvar_coefs <- data.frame(k  = test_1 %>% select(k) %>% .[[1]] %>% unique %>% sort,
                           mc = NA, md = NA)
newvar_coefs$mc <- mc_coef[2]*newvar_coefs$k
newvar_coefs$md <- md_coef["newvar"] + c(0,md_coef[8:12])

ggplot(data = newvar_coefs %>% melt(id.vars = "k"),
       aes(x = k, y = value, color = variable)) +
    geom_point() +
    geom_line()

ggplot(data = newvar_coefs, aes(x = md, y = mc)) +
    geom_point() +
    geom_line() +
    geom_abline(aes(intercept = 0, slope = 1), color = "orange")







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












