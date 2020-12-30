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
    drop_na(sym(!!lhs)) %>%
    #filter(!is.na(v3v5overV2)) %>% 
    select_if(nany) %>%
    #select(sym(!!lhs), ageyear_at_meas, male, race3, clusterid) %>%
    mutate_if(is.numeric, outliers) %>%
    mutate_if(is.numeric, outliers) %>%
    mutate_if(isnt.numeric, factor) %>%
    select_if(nonunique_factors)
    #mutate(race3 = as.factor(race3), male=as.factor(male)) 
print(lhs)
regressed <- tibble()
for (clmn in colnames(regressors)){
  # need to drop NA here rather than in regressors...
  variables <- c(clmn, 'age',
                 'race',
                 'SEX'
                 )
  #' variables <- c(clmn, 'age',
  #'                'as.factor(race)',
  #'                'SEX'#,
  #'                #'as.factor(unittype)'
  #' )
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

phack <- function(wdemo, prefix){
phackvars <- NULL
for (v in names(wdemo)[startsWith(names(wdemo), prefix)]){
  p <- regress_all(wdemo, v)
  p$regressor <- v
  phackvars <- rbind(phackvars, p)
}
phackvars
}





