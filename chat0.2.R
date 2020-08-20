library(tidyverse)

`%notin%` <- Negate(`%in%`)

# demographic info
demofile <- '~/dyn/data/chat/tab/chat-baseline.csv'

demo <- read_csv(demofile)


# 8 EEG channels, beta 18-25(?)Hz
load_data_beta2 <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.beta2.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID), CH == 'C3') #%>%
    #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

# 1 EEG, 1 EMG, 1Hz bins
load_data_C3M2 <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD-E_F_CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) #%>%
    #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_hypno <- function(){
  df <- read_table2('~/dyn/data/chat/tab/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID))  #%>%
    #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
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


#idlist = c('301060','300862','300857')
#psd <- load_data_C3M2() #%>% paired_data() #%>% filter(ID %in% idlist)
conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/psd2.db")
#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/chat.db")

#dbWriteTable(conn, "chat", psd)
data <- tbl(conn, 'chat')
#hyp <- load_hypno()
#dbWriteTable(conn, "chat-hypno", hyp)
hypno <- tbl(conn, 'chat-hypno')
datahyp <- left_join(data, hypno)
#hyp <- load_hypno() %>% filter(ID %in% idlist)
#sleepstats <- load_sleepstats()
#dataset <- left_join(psd, hyp)
