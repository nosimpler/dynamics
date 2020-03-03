# specific to CHAT study
library(tidyverse)

`%notin%` <- Negate(`%in%`)

# Use IDs that exist in both sessions and where DTW succeeds
IDlist <- tibble(ID=intersect(unique(m2$ID), unique(m1$ID))) %>%
  filter(ID %notin% c('300215', 
                      '300295',
                      '300397',
                      '300442',
                      '300513',
                      '300612',
                      '300853',
                      '300868',
                      '300944',
                      '300968',
                      '301053',
                      '301055',
                      '301057',
                      '301108'))

m1 <- m1 %>% drop_na()
m2 <- m2 %>% drop_na()


catses <- function(df1,df2, var){
  df1 <- select(df1, E, ID, !!sym(var))
  df2 <- select(df2, E, ID, !!sym(var))
  left_join(df1, df2, by='ID')
}


#hypno <- left_join(hypno_mm1, hypno_mm2, by='ID') %>% 
  #drop_na()

# gather channels and bands
pivot_l <- function(df, sep='_'){
  pivot_longer(df, 3:98, names_to=c('B', 'CH'), names_sep=sep)
}

pivot_dist <- function(df){
  pivot_longer(df, 1:96, names_to=c('B','CH'), names_sep='_')
}

# ID
recast <- function(df){
  df <- mutate(df, ID=str_sub(ID, -6))
}

# some data are off by 10^3
rescale <- function(df){
  for (ID in unique(df$IDs)){
    flag_outliers() %>% max()
  }
}

# filter by whether 
merge_stage_info <- function(m1,m2){
  
}