##### EDIT DISTANCE ON HYPNOGRAM
library(tidyverse)
library(stringdist)
library(TSdist)
library(pdc)
library(dtw)


# construct time-series matrix from IDs, 
# epochs, and values in column 'colname'

ts_matrix <- function(df, colname) {
  epochs <- unique(df$E)
  mat <- df %>% select(ID, E, sym(!!colname)) %>%
    arrange(E) %>%
    pivot_wider(names_from = ID, values_from = sym(!!colname)) %>%
    select(-E) %>%
    as.matrix()
  rownames(mat) <- epochs
  mat
}

fr_matrix <- function(df, colname) {
  epochs <- unique(df$FR)
  mat <- df %>% select(ID, FR, sym(!!colname)) %>%
    arrange(FR) %>%
    pivot_wider(names_from = ID, values_from = sym(!!colname)) %>%
    select(-FR) %>%
    as.matrix()
  rownames(mat) <- epochs
  mat
}



# compute worst and best match
worst_match <- function(dist_table){
  dist_table %>% 
    ungroup() %>%
    group_by(MEASURE) %>%
    arrange(MEASURE) %>%
    filter(value==max(value)) %>%
    filter(row_number()==1)
}

best_match <- function(dist_table){
  dist_table %>% 
    ungroup() %>%
    group_by(MEASURE) %>%
    arrange(MEASURE) %>%
    filter(value==min(value)) %>%
    filter(row_number()==1)
}



m1 <- Hhyp %>% 
  filter(component == 'V5') %>%
  truncate_to_cycle(1, 500) %>%
  mutate(value=normalize(value)) %>%
  ungroup() %>%
  drop_na(value)
m <- m1 %>%
  ts_matrix('value') %>% t() %>%
  dist(method='TSDistances', distance='acf') %>%
  cmdscale() %>% as_tibble()

#tsm <- ts_matrix(filter(hyp25, component=='V6') %>% ungroup(), 'value') 
#d <- dist(t(tsm), method='TSDistances', distance='erp', g=0)
#d <- pdcDist(tsm, t=1, m=5)
#m <- as_tibble(cmdscale(d))
m$ID <- unique(m1$ID)
mid <- split_id(m)
mbase <- filter(mid, session=='baseline')
mdemo <- left_join(demo, mbase) %>% drop_na(V1)

source('~/dyn/src/dynamics/regress.R')
