# regression onto demographics
library(tidyverse)
library(broom)
library(glmnet)

nany <- function(x) !any(is.na(x))
### least squares
isnt.numeric <- function(x) !is.numeric(x)
nonunique_factors <- function(x) (length(unique(x)) > 1)

regress_all <- function(wdemo, lhs){
  regressors <- wdemo %>% 
    #filter(!is.na(v3v5overV2)) %>% 
    select_if(nany) %>%
    #select(sym(!!lhs), ageyear_at_meas, male, race3, clusterid) %>%
    mutate_if(is.numeric, outliers) %>%
    mutate_if(isnt.numeric, factor) %>%
    select_if(nonunique_factors)
    #mutate(race3 = as.factor(race3), male=as.factor(male)) 
print(lhs)
regressed <- tibble()
for (clmn in colnames(regressors)){
  # need to drop NA here rather than in regressors...
  #' variables <- c(clmn, 'ageyear_at_meas', 
  #'                'as.factor(race3)', 
  #'                'male'#,
  #'                #'as.factor(unittype)'
  #'                )
  variables <- c(clmn, 'age', 
                 'as.factor(race)', 
                 'SEX'#,
                 #'as.factor(unittype)'
  )
  f <- as.formula(
    paste(lhs, 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  #print(f)
  lmfit <- tidy(lm(f, data=regressors))
  
  regressed <- rbind(regressed, 
                     filter(lmfit, term==sym(!!clmn)))
}
phack <- arrange(regressed, p.value)
ehack <- filter(arrange(regressed, estimate), p.value < 0.05)
print(phack, n=30)
phack
}
#regress_all(wdemo, 'rV1_rV4')
#regress_all(wdemo, 'rV2_rV5')

# Wwide <- pivot_wider(filter(Wall6_wake, component=='V6'), names_from=c('FR'), values_from='value')
# #W <- left_join(Wwide, demo)
# Wwide <- Wwide %>% separate(ID, c('STUDY','SESSION','nsrrid')) %>% mutate(nsrrid=as.numeric(nsrrid))
# um <- umap(select(Wwide, -component, -STUDY, -SESSION, -nsrrid))
# plot(um$layout[,1], um$layout[,2])
# wdemo2$regress1 <- um$layout[,1]
# wdemo2$regress2 <- um$layout[,2]

phackvars <- NULL
for (v in names(wdemo)[startsWith(names(wdemo), 'dV')]){
  p <- regress_all(wdemo, v)
  p$regressor <- v
  phackvars <- rbind(phackvars, p)
  
}


# regressors$bikes <- as.factor(regressors$slh8c_d > 0)
ggplot(wdemo, aes(x=SEX, y=MDS4))+
  stat_summary_bin(color='blue')+
  #geom_boxplot(color='blue')+
  #geom_point()
  ggbeeswarm::geom_beeswarm()#+
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




