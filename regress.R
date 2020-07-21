# regression onto demographics
library(tidyverse)
library(broom)
library(glmnet)


### least squares

regress_all <- function(wdemo, lhs){
regressors <- wdemo %>% select_if(~ !any(is.na(.))) %>%
  rm_outliers(lhs) %>% filter(baseline<95)#%>% mutate(V1=log(V1),V2=log(V2), V3=log(V3)) %>%
  #filter(V1 > -2, V2 > -10, V3 > -10)

regressed <- tibble()
for (clmn in colnames(select_if(regressors, is.numeric))){
  #print(clmn)
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
print(phack)
}

for (v in names(wdemo)[startsWith(names(wdemo), 'V')]){
  regress_all(wdemo, v)
}

regressors$bikes <- as.factor(regressors$slh8c_d > 0)
ggplot(regressors, aes(x=pedsql_total_scale_score_par , y=V1))+
  #geom_boxplot(color='blue')+
  #ggbeeswarm::geom_beeswarm()+
  stat_summary_bin(color='red', bins=5)+
  geom_point()

### ridge regression
y <- select(regressors, V2) %>% data.matrix()
x <- select(regressors, -V1, -V2, -V3) %>% data.matrix()
lambdas <- logspace(-10,10,n=500)
cv <- cv.glmnet(x,y, lambda=lambdas, alpha=0)
plot(cv)
small.lambda.index <- which(cv$lambda == cv$lambda.min)

fit <- glmnet(x,y, lambda= cv$lambda.min, alpha=0)
tidy(fit) %>% arrange(desc(abs(estimate)))

