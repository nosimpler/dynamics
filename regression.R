# Regression on distance matrices
library(tidyverse)

`%notin%` <- negate(`%in%`)




regress_on_demo_and_standard <- function(b, h){
  standard_measures <- c(
    'TST',
    'MINS_N1',
    'MINS_N2',
    'MINS_N3',
    'MINS_REM',
    'SLP_LAT',
    'REM_LAT',
    'SLP_EFF',
    'SLP_MAIN_EFF',
    'WASO'
  )
  
  
  h$baseline_stats <- filter(h$baseline_stats, ID %in% unique(b$demo$nsrrid))
  comp_b <- cmdscale(b$baseline_dist, 3)
  meas <- h$baseline_stats %>% select(standard_measures)
  meas$age <- b$demo$ageyear_at_meas
  meas$male <- b$demo$male
  meas$cog <- b$demo$bri13b
  meas$ess <- b$demo$epworthscore_adult
  meas$race <- as.factor(b$demo$race3)
  meas$bmiz <- b$demo$bmiz
  
  mds1 <- meas
  mds1$mds <- comp_b[,1]
  print(mds1)
  mds2 <- meas
  mds2$mds <- comp_b[,2]
  mds3 <- meas
  mds3$mds <- comp_b[,3]
  m1 <- summary(lm(mds~., data=mds1))
  m2 <- summary(lm(mds~., data=mds2))
  m3 <- summary(lm(mds~., data=mds3))
  list(fitmds1=m1, fitmds2=m2, fitmds3=m3)
}

regress_on_standard_measures <- function(b, h){
  standard_measures <- c(
    'TST',
    'MINS_N1',
    'MINS_N2',
    'MINS_N3',
    'MINS_REM',
    'SLP_LAT',
    'REM_LAT',
    'SLP_EFF',
    'SLP_MAIN_EFF',
    'WASO'
  )
  
  
  h$baseline_stats <- filter(h$baseline_stats, ID %in% unique(b$demo$nsrrid))
  comp_b <- cmdscale(b$baseline_dist, 3)
  meas <- h$baseline_stats %>% select(standard_measures)
  mds1 <- meas
  mds1$mds <- comp_b[,1]
  print(mds1)
  mds2 <- meas
  mds2$mds <- comp_b[,2]
  mds3 <- meas
  mds3$mds <- comp_b[,3]
  m1 <- summary(lm(mds~., data=mds1))
  m2 <- summary(lm(mds~., data=mds2))
  m3 <- summary(lm(mds~., data=mds3))
  list(fitmds1=m1, fitmds2=m2, fitmds3=m3)
}

mds_demo_fit <- function(b, var){
  b$baseline_mds <- cmdscale(b$baseline)
  b$demo[[var]]
}

