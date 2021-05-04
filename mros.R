library(tidyverse)

# mros

load_mros <- function(){
  df <- read_table2('~/dyn/data/mros/PSD_E_F_CH.txt', guess_max = 1000000)
}
load_mros_bands <- function(){
  df <- read_table2('~/dyn/data/mros/PSD_E_B_CH.txt', guess_max = 1000000) %>%
    drop_na() %>%
    select(-RELPSD) %>%
    rename(component=B, value=PSD) %>%
    left_join(hypno_cfs %>% select(ID, E, CYCLE, STAGE_N)) %>%
    filter(CYCLE >=1)
  
}

load_hypno_mros <- function(){
  df <- read_table2('~/dyn/data/mros/HYPNO_E.txt', guess_max = 1000000)
}
load_demo_mros <- function(){
  df <- read_csv('~/dyn/data/mros/mros-visit1-dataset-0.4.0.csv', guess_max=1000000) %>%
    mutate(nsrrid = as.numeric(gsub("[^0-9.-]", "", nsrrid)))
}

#psd <- load_mros()
#hypno <- load_hypno_mros()
