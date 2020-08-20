library(tidyverse)

load_cfs <- function(){
df <- read_table2('~/dyn/data/cfs/PSD-E_F_CH.txt', guess_max = 1000000) %>%
  filter(!grepl('nonrandomized', ID)) #%>%
#separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}
load_hypno_cfs <- function(){
  df <- read_table2('~/dyn/data/cfs/HYPNO-E.txt', guess_max = 1000000) %>%
    filter(!grepl('nonrandomized', ID)) 
}


#cfsdata <- load_cfs()
conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/cfs/psd.db")
#DBI::dbWriteTable(conn, "cfs", cfsdata)

cfsdata <- tbl(conn, 'cfs')
hyp <- load_hypno_cfs()
dbWriteTable(conn, "cfs-hypno", hyp)
hypno <- tbl(conn, 'cfs-hypno')
datahyp <- left_join(data, hypno)