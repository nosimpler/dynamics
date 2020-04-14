# specific to CHAT study
library(tidyverse)

`%notin%` <- Negate(`%in%`)

# demographic info
demofile <- '~/dyn/data/chat/tab/chat-baseline.csv'
demo <- read_csv(demofile)

# build chat dataset object
# update this function to include/exclude participants based on 
# demographic or other info
compile_dataset <- function(demo, baseline_data, followup_data){
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
  hypno_baseline <- read_table2('~/dyn/data/chat/tab/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')

  hypno_followup <- read_table2('~/dyn/data/chat/tab/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  
  hypno_baseline_stats <-  read_table2('~/dyn/data/chat/tab/HYPNO.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')
  
  hypno_followup_stats <-  read_table2('~/dyn/data/chat/tab/HYPNO.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  
  hypno_ds <- compile_dataset(demo, hypno_baseline, hypno_followup)
  hypno_ds$followup_stats <- hypno_followup_stats %>% 
    filter(ID %in% hypno_ds$IDs)
  hypno_ds$baseline_stats <- hypno_baseline_stats %>% 
    filter(ID %in% hypno_ds$IDs)
  hypno_ds
}

# band-epoch-channel
load_bands <- function(){
  bands_baseline <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>% select(ID, E, B, CH, PSD, RELPSD) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')
  
  bands_followup <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) %>% select(ID, E, B, CH, PSD, RELPSD) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  bands <- compile_dataset(demo, bands_baseline, bands_followup)
  
}

# band-epoch-channel
load_band_ch <- function(band, ch){
  bands_baseline <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.txt', guess_max = 1000000) %>%
    filter(B==sym(!!band), CH==sym(!!ch), !grepl('nonrandomized', ID)) %>% select(ID, E, PSD) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='baseline')
  
  bands_followup <- read_table2('~/dyn/data/chat/tab/PSD.E.B.CH.txt', guess_max = 1000000) %>%
    filter(B==sym(!!band), CH==sym(!!ch), !grepl('nonrandomized', ID)) %>% select(ID, E, PSD) %>%
    separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
    filter(COND=='followup')
  bands <- compile_dataset(demo, bands_baseline, bands_followup)
  bands$ch <- ch
  bands$band <- band
  bands
}

join_tables <- function(df1, df2, var){
  df2 <- select(df2, 'ID', 'E', sym(!!var))
  left_join(df1, df2, by=c('ID', 'E'))
}

# needs hypno table h
append_stage_info <- function(b,h){
  b$baseline <- join_tables(b$baseline,h$baseline,'STAGE_N')
  b$followup <- join_tables(b$followup, h$followup, 'STAGE_N')
  b
}

filterchb <- function(b, ch, band){
  b$baseline <- filter(b$baseline, CH==sym(!!ch), B==sym(!!band))
  b$followup <- filter(b$followup, CH==sym(!!ch), B==sym(!!band))
  b
  }

