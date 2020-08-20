# regression onto demographics
library(tidyverse)
library(broom)
library(glmnet)

nany <- function(x) !any(is.na(x))
### least squares

regress_all <- function(wdemo, lhs){
regressors <- wdemo %>% 
  #filter(!is.na(v3v5overV2)) %>% 
  select_if(nany) #%>%
  #mutate_all(outliers)
print(lhs)
regressed <- tibble()
for (clmn in colnames(select_if(regressors, is.numeric))){
  # need to drop NA here rather than in regressors...
  variables <- c(clmn, 'ageyear_at_meas', 'male', 'as.factor(race3)', 'as.factor(clusterid)')
  f <- as.formula(
    paste(lhs, 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  lmfit <- tidy(lm(f, data=regressors))
  
  regressed <- rbind(regressed, 
                     filter(lmfit, term==sym(!!clmn)))
}
phack <- arrange(regressed, p.value)
ehack <- filter(arrange(regressed, estimate), p.value < 0.05)
print(phack, n=100)
phack
}
#regress_all(wdemo, 'rV5_rV2')
#regress_all(wdemo, 'rV2_rV5')
p_followup <- list()
for (v in names(wdemo)[startsWith(names(wdemo), 'rV')]){
  print(v)
  p_followup[[v]] <- regress_all(wdemo, v)
}

# regressors$bikes <- as.factor(regressors$slh8c_d > 0)
ggplot(wdemo, aes(x=nordball, y=rV3_rV6))+
  #geom_boxplot(color='blue')+
  geom_point()
  #stat_summary_bin(color='red', bins=5)#+
  #geom_point()
# 
# ### ridge regression
# y <- select(regressors, V2) %>% data.matrix()
# x <- select(regressors, -V1, -V2, -V3) %>% data.matrix()
# lambdas <- logspace(-10,10,n=500)
# cv <- cv.glmnet(x,y, lambda=lambdas, alpha=0)
# plot(cv)
# small.lambda.index <- which(cv$lambda == cv$lambda.min)
# 
# fit <- glmnet(x,y, lambda= cv$lambda.min, alpha=0)
# tidy(fit) %>% arrange(desc(abs(estimate)))

