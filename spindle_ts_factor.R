# time-series factorization for spindles/so 
library(patchwork)
library(tidyverse)
library(NMF)
library(ggsci)

hyp <- load_hypno()
sp <- load_spindles()
so <- load_so()
so <- left_join(hyp, so)
sp <- left_join(hyp, sp)

prerem_allcyc <- function(df) {
  df2 <- NULL
  for (i in seq(5)){
    cyc_i <- get_prerem2(df, cycle = i)
    df2 <- rbind(df2, cyc_i)
  }
  df2
}
spindles_pr <- prerem_allcyc(sp) %>% drop_na(F)
so_pr <- prerem_allcyc(so) %>% drop_na(CH)

ggplot(spindles_pr %>% rename(FR=F), aes(x=E, y=N, color=as.factor(FR)))+
         stat_summary(size=0.1)+
         facet_grid(CH~CYCLE)+
  scale_color_nejm()+
  theme_minimal()

ggplot(so_pr, aes(x=E, y=N))+
  stat_summary(size=0.1)+
  facet_grid(CH~CYCLE)+
  scale_color_nejm()+
  theme_minimal()
ggplot(so_pr, aes(x=E, y=P2P_AMP))+
  stat_summary(size=0.1)+
  facet_grid(CH~CYCLE)+
  scale_color_nejm()+
  theme_minimal()
ggplot(so_pr, aes(x=DOWN_AMP, y=UP_AMP))+
  stat_summary_bin(size=0.1, geom='crossbar')+
  #scale_color_nejm()+
  facet_grid(CH~CYCLE)+
  theme_minimal()
ggplot(so_pr, aes(x=E, y=UP_AMP/abs(DOWN_AMP)))+
  stat_summary(size=0.1)+
  #scale_color_nejm()+
  facet_grid(CH~CYCLE)+
  theme_minimal()
ratio <- so_pr %>% 
  group_by(ID,CH) %>% 
  drop_na(UP_AMP, DOWN_AMP) %>%
  summarize(AMP_RATIO=median(UP_AMP/abs(DOWN_AMP)), 
            UP_AMP = median(UP_AMP),
            DOWN_AMP = median(DOWN_AMP)) %>%
  pivot_wider(names_from=CH, 
              values_from=DOWN_AMP,
              names_prefix='CH_')
demoratio <- left_join(split_id(ratio), demo) 
phack(demoratio, 'CH_')


spncyc <- so %>% get_prerem2(cycle=1)
n_components <- 5
n_epochs <- 50
rem_dur <- rem_duration2(hypno)

IDs <- demo %>% 
  #filter(unittype %in% c(1)) %>%
  select(nsrrid)

tstable <- spncyc %>% 
  split_id() %>%
  filter(nsrrid %in% IDs$nsrrid) %>%
  unite_id() %>%
  #filter(F == 15, CH=='C3') %>%
  filter(CH=='C3') %>%
  #filter(session == 'followup') %>%
  #filter(component==COMP) %>% 
  #group_by(nsrrid) %>%
  group_by(ID) %>%
  #mutate(value = normalize(value)) %>%
  #mutate(E = E-min(E)) %>%
  arrange(E) %>%
  filter(sum(N)>0) %>%
  select(ID, E, P2P_AMP) %>%
  pivot_wider(id_cols=ID, names_from = E, values_from=P2P_AMP) %>%
  mutate_all(~replace_na(.,0))

IDs <- unique(tstable$ID)

tsmatrix <- tstable %>%
  ungroup() %>%
  select(-ID) %>%
  as.matrix()

fit <- nmf(tsmatrix, n_components)
H_comp <- as_tibble(t(fit@fit@H))
H_comp$MINS <- seq(-1,-n_epochs)/2
H_comp <- H_comp %>% 
  pivot_longer(-MINS, names_to='NMF', names_prefix='V')

V <- as_tibble(fit@fit@W)
colnames(V) <- c('NMF1','NMF2', 'NMF3', 'NMF4', 'NMF5')
V$ID <- IDs

durreg <- left_join(V, rem_dur)
#durreg$lastvalue <- tsmatrix[,ncol(tsmatrix)]
#durreg <- mutate_if(durreg, is.numeric, log10)
durreg <- mutate_if(durreg, is.numeric, outliers)
durreg <- mutate_if(durreg, is.numeric, outliers) %>% drop_na()
durreg_regress <- durreg %>% split_id()
durreg_regress <- left_join(durreg_regress, select(demo,
                                                   c(nsrrid,
                                                     ageyear_at_meas,
                                                     race3,
                                                     male))) %>%
  mutate(race3 = factor(race3))

# durreg_regress <- left_join(durreg_regress, select(demo, 
#                                                    c(nsrrid, 
#                                                      age, 
#                                                      BLACK, 
#                                                      SEX))) %>%
#   mutate(BLACK = factor(BLACK))

print(tidy(lm(duration ~ ., 
              data=select(durreg_regress, -nsrrid, -study, -session))))

durreg$dur_bin <- durreg$duration %>% cut_number(2)
durreg_long <- pivot_longer(durreg, cols=c(-ID,-duration, -dur_bin), names_to='NMF')
p1 <- ggplot(H_comp, aes(x=MINS, y=value, color=NMF))+
  geom_line()+
  scale_color_brewer(palette='Set1')

p2 <- ggplot(durreg_long, aes(x=value, y=duration, color=NMF))+
  geom_point(alpha=0.1)+
  #stat_summary_bin()+
  scale_color_brewer(palette='Set1')+
  facet_wrap(~NMF)
p1/p2
wdemo <- left_join(demo, split_id(V)) %>% drop_na(NMF1)
regress_all(wdemo, 'NMF1')
regress_all(wdemo, 'NMF2')
regress_all(wdemo, 'NMF3')
regress_all(wdemo, 'NMF4')
regress_all(wdemo, 'NMF5')

