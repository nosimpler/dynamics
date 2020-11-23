# distances
library(tidyverse)
library(TSclust)
library(TSdist)
library(NMF)
library(broom)
library(patchwork)

# TO DO: get COMP x CYCLE table

comp <- c('V1','V2','V3','V4','V5','V6')
cyc <- 5
n_epochs <- 50
n_components <- 2

#demo <- demo2 %>% filter(QuEEG1 == 5, QuEEG2 == 5) %>%
 # filter(age < 50)
#Hw <- Hall6_wake %>% split_id()
#hyp <- select(hypno, ID, E, CYCLE) %>% collect() %>% split_id()
#hypnoH <- left_join(Hw, hyp) %>% unite_id()
#hypnoH <- left_join(refit$H, hypno10)
prerem <- get_prerem(hypnoH, cycle=cyc, n_epochs = n_epochs) %>% 
  filter(ID != 'chat-followup-300853') 

for (COMP in comp){


# find REM duration
rem_dur <- rem_duration(hypnoH, cycle=cyc)

IDs <- demo %>% 
#filter(unittype %in% c(1)) %>%
  select(nsrrid) %>%
  as.vector()
#IDs <- demo %>% select(nsrrid)


tstable <- prerem %>% 
  split_id() %>%
  filter(nsrrid %in% IDs$nsrrid) %>%
  unite_id() %>%
  #filter(session == 'followup') %>%
  filter(component==COMP) %>% 
  #group_by(nsrrid) %>%
  group_by(ID) %>%
  #mutate(value = normalize(value)) %>%
  mutate(E = E-min(E)) %>%
  arrange(E) %>%
  select(ID, E, value) %>%
  pivot_wider(id_cols=ID, names_from = E, values_from=value) %>%
  drop_na()


IDs <- unique(tstable$ID)

tsmatrix <- tstable %>%
  ungroup() %>%
  select(-ID) %>%
  as.matrix()
  

#dst <- TSDatabaseDistances(tsmatrix, distance='euclidean')

#dstm <- as_tibble(as.matrix(dst))
#dstm <- alignment
#colnames(dstm) <- IDs
#dstm$ID <- IDs


#V <- as_tibble(cmdscale(dst, k=4))
#colnames(V) <- c('MDS1', 'MDS2', 'MDS3', 'MDS4')

fit <- nmf(tsmatrix, n_components)
H_comp <- as_tibble(t(fit@fit@H))
H_comp$MINS <- seq(-1,-n_epochs)/2
H_comp <- H_comp %>% 
  pivot_longer(-MINS, names_to='NMF', names_prefix='V')

V <- as_tibble(fit@fit@W)
colnames(V) <- c('NMF1','NMF2', 'NMF3', 'NMF4')
V$ID <- IDs

durreg <- left_join(V, rem_dur)
#durreg$lastvalue <- tsmatrix[,ncol(tsmatrix)]
#durreg <- mutate_if(durreg, is.numeric, log10)
durreg <- mutate_if(durreg, is.numeric, outliers)
durreg <- mutate_if(durreg, is.numeric, outliers) %>% drop_na()
durreg_regress <- durreg %>% split_id()
durreg_regress <- left_join(durreg_regress, select(demo, c(nsrrid, ageyear_at_meas, race3, male))) %>%
  mutate(race3 = factor(race3))
print(COMP)

print(tidy(lm(duration ~ ., data=select(durreg_regress, -nsrrid, -study, -session))))

durreg$dur_bin <- durreg$duration %>% cut_number(2)
durreg_long <- pivot_longer(durreg, cols=c(-ID,-duration, -dur_bin), names_to='NMF')
p1 <- ggplot(H_comp, aes(x=MINS, y=value, color=NMF))+
  geom_line()+
  scale_color_brewer(palette='Set1')+
  ggtitle(COMP)

p2 <- ggplot(durreg_long, aes(x=value, y=duration, color=NMF))+
  geom_point(alpha=0.1)+
  stat_summary_bin()+
  scale_color_brewer(palette='Set1')+
  facet_wrap(~NMF, nrow=n_components)

#print(GGally::ggpairs(durreg, 1:2, mapping=aes(color=dur_bin)))+theme_grey()
#print(p3)
print(p1+p2)
wdemo <- left_join(demo, split_id(V)) %>% drop_na(NMF1)
print(COMP)
regress_all(wdemo, 'NMF1')
regress_all(wdemo, 'NMF2')

}

#fit@fit@W/rowSums(fit@fit@W)
#Wnorm <- fit@fit@W/rowSums(fit@fit@W)
#dstm <- as_tibble(Wnorm %*% t(Wnorm))
#colnames(dstm) <- IDs
#dstm$ID <- IDs
# 
# ccord <- dstm %>%
#   pivot_longer(-ID, names_to='ID2', values_to='distance')
# ccord <- ccord %>% split_id() %>%
#   separate(ID2, c('study2', 'session2','nsrrid2')) %>%
#   mutate(nsrrid2 = as.numeric(nsrrid2))
# ccord$self <- (ccord$nsrrid == ccord$nsrrid2)
# ccord <- ccord %>% filter(session=='baseline', session2=='followup')
# ggplot(ccord, aes(x=distance, y=..density..,fill=self))+
#   geom_histogram(position='identity', alpha=0.5)+
#   scale_fill_brewer(palette='Set1')


#wdemo <- left_join(demo, split_id(V)) %>% drop_na(NMF1)
# source('~/dyn/src/dynamics/regress.R')
