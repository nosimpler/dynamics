# specific to CHAT study
library(tidyverse)

`%notin%` <- Negate(`%in%`)

# demographic info
demofile <- '~/dyn/data/chat/chat-baseline.csv'
demo <- read_csv(demofile)


# update this function to include/exclude participants based on 
# demographic or other info
filter_dataset <- function(demo, baseline_data, followup_data){
  dataset <- list()
  EXCLUDE <- c(300058,300368,300668) # first epoch isn't wake
  IDwholenight <- wholenight_only(demo)$nsrrid
  IDb <- unique(baseline_data$ID)
  IDf <- unique(followup_data$ID)
  # include participants with whole-night recording for baseline/followup
  INCLUDE <- intersect(intersect(IDwholenight, IDb), IDf)
  dataset$baseline <- baseline_data %>%
    filter(ID %in% INCLUDE, ID %notin% EXCLUDE)
  dataset$followup <- followup_data %>%
    filter(ID %in% INCLUDE, ID %notin% EXCLUDE)
  dataset$demo <- demo %>%
    filter(nsrrid %in% INCLUDE, nsrrid %notin% EXCLUDE)
  dataset$IDs <- unique(dataset$demo$nsrrid)
  dataset
}

#### LOAD DATA

#hypno-epochs
load_hypno <- function(){
  hypno_baseline <- read_table2('~/dyn/data/chat/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')

  hypno_followup <- read_table2('~/dyn/data/chat/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  
  hypno_baseline_stats <-  read_table2('~/dyn/data/chat/HYPNO.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')
  
  hypno_followup_stats <-  read_table2('~/dyn/data/chat/HYPNO.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  
  hypno_ds <- filter_dataset(demo, hypno_baseline, hypno_followup)
  hypno_ds$followup_stats <- hypno_followup_stats %>% 
    filter(ID %in% hypno_ds$IDs)
  hypno_ds$baseline_stats <- hypno_baseline_stats %>% 
    filter(ID %in% hypno_ds$IDs)
  hypno_ds
}
# need to be more delicate about dropping NA (due to NREM2-specific variables)
# this part will change if different variables are used
# m1 <- m1c %>% recast() %>% 
#   select(3:98,STAGE, ID, CYCLE,E) %>% 
#   drop_na()
# 
# m2 <- m2c %>% recast() %>% 
#   select(3:98,STAGE, ID, CYCLE,E) %>% 
#   drop_na()



# make sure all stages are in the cycle

# m1stages <- list()
# m2stages <- list()
# for (id in flatten(IDlist)){
#   m1stages[[id]] <- m1 %>% filter(ID==id, CYCLE==1) %>% select(STAGE) %>% unique()
#   m2stages[[id]] <- m2 %>% filter(ID==id,CYCLE==1) %>% select(STAGE) %>% unique()
# }


# concatenate sessions for one variable
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

# note: some data are off by 10^3
rescale <- function(df){
  for (ID in unique(df$IDs)){
    flag_outliers() %>% max()
  }
}

wholenight_only <- function(demo){
  demo %>% 
    filter(recbeaw==0,losbeg==0,losoth==0)
}




