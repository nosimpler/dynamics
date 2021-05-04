library(tidyverse)

`%notin%` <- Negate(`%in%`)

# demographic info
demofile <- '~/dyn/data/chat/tab/chat-baseline.csv'
#demofile2 <-

demo <- read_csv(demofile)


# sync data
load_sync <- function(){
  conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/sync.db")
  tbl(conn, 'sync')
}

# 8 EEG channels, beta 18-25(?)Hz
load_data_beta2 <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.beta2.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID), CH == 'C3') #%>%
    #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_chat_bands <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD-E_B_CH.txt', guess_max = 1000000) %>%
   filter(CH == 'C3') %>%
   drop_na()
  #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_data_mtm <- function(){
  df <- read_table2('~/dyn/data/chat/tab/MTM-E_F_CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID), CH == 'C3') #%>%
  #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

# 1 EEG, 1 EMG, 1Hz bins
load_data_C3M2 <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD-E_F_CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) #%>%
    #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_chat_psd <- function(){
  df <- read_table2('~/dyn/data/chat/tab/PSD-E_F_CH_2.txt', guess_max = 1000000) %>%
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

load_so<- function(){
   df <- read_table2('~/dyn/data/chat/tab/SPINDLES-E_CH_THR.txt', guess_max=1000000)
}

load_spindles <- function(){
  df <- read_table2('~/dyn/data/chat/tab/SPINDLES-E_F_CH.txt', guess_max=1000000)
}

# for six-factor case
reorder_factors6 <- function(nmf_fit){
  nmf_fit$H <- nmf_fit$H %>%
    mutate(component=recode(component, V2='V1',
                            V3='V2',
                            V4='V3',
                            V5='V4',
                            V1='V5')) %>%
    arrange(component) %>%
    mutate(component=factor(component))

  nmf_fit$W <- nmf_fit$W %>%
    mutate(component=recode(component, V2='V1',
                            V3='V2',
                            V4='V3',
                            V5='V4',
                            V1='V5'))  %>%
    arrange(component) %>%
    mutate(component=factor(component))
  labels <- c(V1="Slow",
              V2="Delta",
              V3="Theta",
              V4="Sigma",
              V5="Beta",
              V6="Gamma")
  nmf_fit$W <- mutate(nmf_fit$W, component=recode_factor(component, !!!labels))

  nmf_fit$H <- mutate(nmf_fit$H, component=recode_factor(component, !!!labels))
  nmf_fit
}


#idlist = c('301060','300862','300857')
#psd <- load_data_C3M2() #%>% paired_data() #%>% filter(ID %in% idlist)
#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/chatmtm.db")
#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/psd100.db")
#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/psd2.db")
#psd <- load_chat_psd() %>% drop_na(CH)
#psd_bands <- load_data_bands()
#psd_bands <- psd_bands %>%
#  filter(B != "TOTAL") %>%
#  select(-RELPSD) %>%
#  rename(component=B, value=PSD)
#psd_bands_hyp <- hyp %>% left_join(psd_bands)

#DBI::dbWriteTable(conn, "chat", mtm)
#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/sync.db")
#data <- tbl(conn, 'chat')
#hypno <- load_hypno()
#datahyp <- left_join(hyp, psd)
#so <- load_so()
#so <- left_join(hyp, so)
#DBI::dbWriteTable(conn, "chat-hypno", hyp)
#conn2 <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/sync.db")
#hypno <- load_hypno()
#DBI::dbWriteTable(conn, "hypno", hypno)

hyp <- tbl(conn, 'hypno')
sync <- tbl(conn, 'sync')
hypsync <- left_join(hyp, sync)
#datahyp <- left_join(data, hypno, copy=TRUE)
#hyp <- load_hypno() %>% filter(ID %in% idlist)
#sleepstats <- load_sleepstats()
#dataset <- left_join(mtm, hyp)
