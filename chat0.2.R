library(tidyverse)

`%notin%` <- Negate(`%in%`)

# demographic info
demofile <- '~/dyn/data/chat/tab/chat-baseline.csv'

demo <- read_csv(demofile)

load_data <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_hypno <- function(){
  df <- read_table2('~/dyn/data/chat/tab/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID))  %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_sleepstats <- function(){
df <-  read_table2('~/dyn/data/chat/tab/HYPNO.txt', guess_max = 1000000) %>%
  filter(!grepl('nonrandomized', ID)) %>%
  separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

paired_data <- function(df){
  IDs <- df %>% select(ID, COND) 
  baselineIDs <- IDs %>% filter(COND=='baseline') %>% unique()
  followupIDs <- IDs %>% filter(COND=="followup") %>% unique()
  newIDs <- intersect(baselineIDs$ID, followupIDs$ID)
  df %>% filter(ID %in% newIDs)
}



bpower <- load_data() %>% paired_data()
hyp <- load_hypno()
sleepstats <- load_sleepstats()
dataset <- left_join(bpower, hyp, by=c('ID', 'E', 'COND'))
