library(dplyr); library(magrittr); library(ggplot2); library(reshape2)
rm(list = ls()); load("./test_3.Rdata")


##### I. exploratory analysis

ggplot(data = test_3, aes(x = sqrt(n), y = time, color = factor(c))) + 
    geom_point(alpha = 0.3) + 
    facet_wrap(facets = c~k, scales = "free", ncol = 6) +
    geom_smooth(method = "loess", color = "purple", size = 0.6)


##### II. algorithmic/grid model selection

cutoff_n <- test_3 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.50)
sub_test <- test_3 %>% mutate(fact_k = factor(k), fact_c = factor(c), train = (n <= cutoff_n))
alphas   <- seq(from = -1, to = +1,  by = 0.1)
betas    <- seq(from = +1, to = -4, by = -0.5)
grid     <- data.frame(alpha = rep(alphas, length(betas)), 
                       beta  = rep(betas, each = length(alphas)),
                       Rsqd  = 0,
                       SSE   = 0,
                       SSEadj= 0)
for (i in 1:dim(grid)[1]) {
    alpha   <- grid[i,"alpha"]
    beta    <- grid[i,"beta"]
    temp    <- sub_test %>% filter(c > 0) %>% mutate(newvar = n^alpha * log(n)^beta)
    fit_cut <- lm(data = temp %>% filter(train), time ~ newvar : k : fact_c + fact_k)
    fit_all <- lm(data = temp                  , time ~ newvar : k : fact_c + fact_k)
    grid[i,"Rsqd"]   <- fit_all %>% summary %>% .$r.squared
    grid[i,"SSE"]    <- sum( (predict(fit_cut, newdata = temp) - temp[,"time"])^2 )
    grid[i,"SSEadj"] <- 10^6 * sum( ((predict(fit_cut, newdata = temp) - temp[,"time"])/temp[,"k"])^2 )
}
save(grid, file = "./grid_test_3.Rdata")
grid[which.max(grid$Rsqd)  ,]  # alpha = 0.5 ; beta = -2.5
grid[which.min(grid$SSE)   ,]  # alpha = 0.6 ; beta = -4.0
grid[which.min(grid$SSEadj),]  # alpha = 0.4 ; beta = -4.0
grid %>% filter(alpha > 0.35, alpha < 0.65) %>% filter(beta >= -4.0, beta <= -1.0) %>% filter(SSE < 100)
# So alpha = 0.5 / beta = -2 wins because it looks more explainable; we re-process our initial charts

ggplot(data = test_3, aes(x = sqrt(n)/log(n)^2, y = time, color = factor(c))) + 
    geom_point(alpha = 0.4, size = 1.5) + 
    facet_wrap(facets = c~k, scales = "free", ncol = 6) +
    geom_smooth(method = "loess", color = "purple", size = 0.6) +
    ggsave(filename = "./plot_test_3_i.pdf", width = 15, height = 8)

rm(list = ls()); load("./test_3.Rdata")


##### III. comparing serial and parallel execution times

sub_test_2 <- test_3 %>% mutate(c = (c+abs(c))/2) %>% # just replaces -1 by 0 for serial cases
                         group_by(n,k,c) %>% 
                         summarize(time = mean(time)) %>%
                         mutate(c = paste0("c",c)) %>% 
                         dcast(n + k ~ c, value.var = "time") %>%
                         melt(id.var = c("n","k","c0"), value.name = "time")
ggplot(data = sub_test_2, aes(x = c0, y = c0/time, color = variable)) +
    geom_point(alpha = 0.5, size = 1.5) + 
    geom_smooth(method = "loess") +
    facet_wrap( ~ k, scales = "free", ncol = 3) +
    ggsave(filename = "./plot_test_3_ii.pdf", width = 15, height = 8)

rm(list = ls()); load("./test_3.Rdata")


##### IV. modelling

cutoff_n   <- test_3 %>% select(n) %>% .[[1]] %>% quantile(probs = 0.50)
sub_test_3 <- test_3 %>% filter(c > 0) %>% 
                         mutate(newvar = sqrt(n)/log(n)^2, 
                                fact_k = factor(k), 
                                fact_c = factor(c), 
                                train = (n < cutoff_n)) %>%
                         select(-p)

model_a <- lm(data = sub_test_3, time ~ newvar : k : fact_c + fact_k)
model_b <- lm(data = sub_test_3, time ~ newvar : fact_k : fact_c + fact_k)  # basically a much more flexible version of model_a
summary(model_b)






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












