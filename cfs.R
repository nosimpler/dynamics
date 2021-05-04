library(tidyverse)

`%notin%` <- Negate(`%in%`)

load_cfs <- function(){
  df <- read_table2('~/dyn/data/cfs/PSD-E_F_CH_2.txt', guess_max = 1000000)
}
load_cfs_bands <- function(){
  df <- read_table2('~/dyn/data/cfs/PSD-E_B_CH.txt', guess_max = 1000000) %>%
    drop_na() %>%
    select(-RELPSD) %>%
    #rename(component=B, value=PSD) %>%
    left_join(hypno_cfs %>% select(ID, E, CYCLE, STAGE_N)) %>%
    filter(CYCLE >=1)
    
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

load_spindle_so_coupling <- function(){
  df <- read_table2('~/dyn/data/cfs/SPINDLES-F_CH_THR.txt', guess_max=1000000)  
}

load_psd_clean <- function() {
  hypno <- load_hypno_cfs()
  cfsdata <- load_cfs()
  left_join(hypno, cfsdata) %>% 
    remove_10pm() %>%
    exclude_nas()
} 

# some indivs had NAs in some frequency bands
exclude_nas <- function(df){
  na <- find_na(df) %>% select(ID, F)
  exclude <- unique(na$ID)
  filter(df, ID %notin% exclude) %>%
     filter(E!=min(E)) # hack to circumvent off-by-one(?)
}

# for six-factor case
reorder_factors6 <- function(nmf_fit){
  nmf_fit$H <- nmf_fit$H %>% 
    mutate(component=recode(component, V3='V1',
                            V4='V2', 
                            V2='V3', 
                            V5='V4',
                            V1='V5')) %>%
    arrange(component) %>%
    mutate(component=factor(component))
  
  nmf_fit$W <- nmf_fit$W %>% 
    mutate(component=recode(component, V3='V1',
                            V4='V2', 
                            V2='V3', 
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

# standardize demographic variables
# cleandemo <- function(demo){
#   demo %>% rename(sex=SEX)
# }
# demo <- cleandemo(demo)
# hypno <- load_hypno_cfs()

#demo <- load_demo_cfs()
#datahyp <- load_psd_clean()
#datahyp <- datahyp %>% exclude_nas()

#conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/cfs/psd.db")
#DBI::dbWriteTable(conn, "cfs", cfsdata)

#cfsdata <- tbl(conn, 'cfs')
#cfsbands <- load_cfs_bands()


#hypno <- tbl(conn, 'cfs-hypno10pm') %>% collect()
#datahyp <- left_join(cfsdata, hypno)

#hypno10 <- hypno %>% collect() %>% remove_10pm()
#DBI::dbWriteTable(conn, "cfs-hypno10pm", hypno10)
#bhyp10 <- left_join(cfsdata, hypno, copy=TRUE)
#so <- left_join(hypno, load_so())
#sp <- left_join(hypno, load_spindles())

