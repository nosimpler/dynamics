# ascending/descending 

wake <- filter(df_all, B=="DELTA") %>% 
  mutate(dnormPSD2 = diffx(dnormPSD)) %>%
  mutate(dnormPSD3 = diffx(dnormPSD2))

TotD <- filter(wake, dnormPSD>=0) %>%
  drop_na(CYCLE) %>%
  filter(CYCLE<4, STAGE != 'REM') %>%
  group_by(nsrrid) %>% count() %>%
  rename(nDTOT = n)

D <- filter(wake, dnormPSD >=0, WASO==1) %>%
  drop_na(CYCLE) %>% 
  filter(CYCLE<4, STAGE != 'REM') %>%
  group_by(nsrrid) %>% count()

TotA <- filter(wake, dnormPSD < 0) %>%
  drop_na(CYCLE) %>%
  filter(CYCLE<4, STAGE != 'REM') %>%
  group_by(nsrrid) %>% count() %>%
  rename(nATOT = n)

A <- filter(wake, dnormPSD < 0, WASO==1) %>% 
  drop_na(CYCLE) %>% 
  filter(CYCLE<4, STAGE != 'REM') %>%
  group_by(nsrrid) %>% count() 

#Tot <- filter()

df <- full_join(D,A, by='nsrrid', suffix=c('D','A')) %>% 
  full_join(TotD, by='nsrrid') %>%
  full_join(TotA, by='nsrrid') %>%
  mutate(nD=replace_na(nD,0)) %>%
  mutate(nA=replace_na(nA,0)) %>%
  mutate(nDTot = replace_na(nDTOT,0)) %>%
  mutate(nATot = replace_na(nATOT,0)) %>%
  mutate(DESCENDING_AROUSAL_PERCENT = nD/nDTOT*100) %>%
  mutate(ASCENDING_AROUSAL_PERCENT = nA/nATOT*100) %>%
  drop_na(DESCENDING_AROUSAL_PERCENT) %>%
  drop_na(ASCENDING_AROUSAL_PERCENT) #%>%
  #mutate(AD_RATIO = ASCENDING_AROUSAL_PERCENT/DESCENDING_AROUSAL_PERCENT)

demodf <- left_join(df, demo)

ggplot(demodf, aes(x=age, y=DESCENDING_AROUSAL_PERCENT))+
  geom_point()+scale_y_log10()
ggplot(demodf, aes(x=age, y=ASCENDING_AROUSAL_PERCENT))+
  geom_point()+scale_y_log10()
ggplot(demodf, aes(x=nD/(nDTOT+nATOT), y=nA/(nDTOT+nATOT)))+
  geom_point()

ggplot(demodf, aes(x=nDTOT, y=nATOT))+
  geom_point()

ggplot(demodf, aes(x=DESCENDING_AROUSAL_PERCENT,
                   y=ASCENDING_AROUSAL_PERCENT)
       )+geom_point()+scale_x_log10()+scale_y_log10()+
  geom_abline(intercept=0, slope=1)

#demoratio <- filter(demodf, is.finite(AD_RATIO))

aregress <- regress_all(demodf, "ASCENDING_AROUSAL_PERCENT")
dregress <- regress_all(demodf, "DESCENDING_AROUSAL_PERCENT")
#demoratio <- filter(demodf, is.finite(AD_RATIO))
#adregress <- regress_all(drop_na(demoratio, AD_RATIO), "AD_RATIO")

ggplot(filter(demodf), aes(x=male, y=ASCENDING_AROUSAL_PERCENT))+
  ggbeeswarm::geom_beeswarm()+stat_summary()
ggplot(demodf, aes(x=MedAlert, y=ASCENDING_AROUSAL_PERCENT))+
  ggbeeswarm::geom_beeswarm()+stat_summary()

# hilbert
wasos <- df_all %>%
group_by(nsrrid, B) %>%
mutate(hphase=Arg(seewave::hilbert(normPSD, f=1))) %>%
mutate(dnormPSD2 = diffx(dnormPSD)) %>%
mutate(dnormPSD3 = diffx(dnormPSD2))


ggplot(wasos, aes(x=hphase, fill=factor(WASO))) +
geom_histogram(bins=100, position='identity', alpha=0.5)+
facet_wrap(~B) +
scale_y_log10() +
scale_fill_brewer(palette='Set1')
ggplot(wasos, aes(x=hphase, y=..density.., fill=factor(WASO))) +
geom_histogram(bins=50, position='identity', alpha=0.5)+
facet_wrap(~B) +
#scale_y_log10() +
scale_fill_brewer(palette='Set1')

