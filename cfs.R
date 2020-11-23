library(tidyverse)

load_cfs <- function(){
  df <- read_table2('~/dyn/data/cfs/PSD-E_F_CH.txt', guess_max = 1000000)
}
load_cfs_bands <- function(){
  df <- read_table2('~/dyn/data/cfs/PSD-E_B_CH.txt', guess_max = 1000000)
}

load_hypno_cfs <- function(){
  df <- read_table2('~/dyn/data/cfs/HYPNO-E.txt', guess_max = 1000000)
}
load_demo_cfs <- function(){
  df <- read_csv('~/dyn/data/cfs/cfs-visit5-dataset-0.5.0.csv', guess_max=1000000)
}
remove_10pm <- function(df){
  df %>% 
    group_by(ID) %>% 
    arrange(E) %>% 
    mutate(onset=first(which(PERSISTENT_SLEEP ==1), order_by=E)) %>% 
    filter(row_number() > onset) %>% 
    filter(first(CLOCK_HOURS)>22 | first(CLOCK_HOURS<2))
}

remove_10pm_band <- function(df){
  df %>% 
    group_by(ID, B) %>% 
    arrange(E) %>% 
    mutate(onset=first(which(PERSISTENT_SLEEP ==1), order_by=E)) %>% 
    filter(row_number() > onset) %>% 
    filter(first(CLOCK_HOURS)>22 | first(CLOCK_HOURS<2))
}

load_so<- function(){
  df <- read_table2('~/dyn/data/cfs/SPINDLES-E_CH.txt', guess_max=1000000)  
}

load_spindles <- function(){
  df <- read_table2('~/dyn/data/cfs/SPINDLES-E_F_CH.txt', guess_max=1000000)  
}

demo <- load_demo_cfs()
#cfsdata <- load_cfs()
conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/cfs/psd.db")
#DBI::dbWriteTable(conn, "cfs", cfsdata)

#cfsdata <- tbl(conn, 'cfs')
#cfsbands <- load_cfs_bands()
#hyp <- load_hypno_cfs()

hypno <- tbl(conn, 'cfs-hypno10pm') %>% collect()
#datahyp <- left_join(cfsdata, hypno)

#hypno10 <- hypno %>% collect() %>% remove_10pm()
#DBI::dbWriteTable(conn, "cfs-hypno10pm", hypno10)
#bhyp10 <- left_join(cfsdata, hypno, copy=TRUE)
so <- left_join(hypno, load_so())
sp <- left_join(hypno, load_spindles())

