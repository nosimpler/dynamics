# distances

library(TSclust)
library(TSdist)

# make distance matrix

prerem <- get_prerem(hypnoH, cycle=1) %>% 
  filter(ID != 'chat-followup-300853') 

IDs <- demo %>% 
#filter(unittype %in% c(1)) %>%
  select(nsrrid) %>%
  as.vector()
  

tstable <- prerem %>% 
  split_id() %>%
  filter(nsrrid %in% IDs$nsrrid) %>%
  #filter(session == 'baseline') %>%
  filter(component=='V6') %>% 
  group_by(nsrrid) %>%
  mutate(value = normalize(value)) %>%
  mutate(E = E-min(E)) %>%
  arrange(E) %>%
  select(nsrrid, E, value) %>%
  pivot_wider(id_cols=nsrrid, names_from = E, values_from=value) %>%
  drop_na()

IDs <- unique(tstable$nsrrid)

tsmatrix <- tstable %>%
  ungroup() %>%
  select(-nsrrid) %>%
  as.matrix()
  


dst <- TSDatabaseDistances(tsmatrix, distance='euclidean')
V <- as_tibble(cmdscale(dst, k=4))
colnames(V) <- c('MDS1', 'MDS2', 'MDS3', 'MDS4')
V$nsrrid <- IDs
wdemo <- left_join(demo, V) %>% drop_na(MDS1)


source('~/dyn/src/dynamics/regress.R')