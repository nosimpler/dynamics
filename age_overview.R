# age overview

df <- hypb10 %>% 
  truncate_to_persistent_sleep_onesided() %>%
  split_id() %>%
  filter(B != 'TOTAL') %>%
  filter(E < 500) %>%
  group_by(nsrrid, B) %>%
  filter(PERSISTENT_SLEEP == 1, WASO==0) %>%
  mutate(normPSD = normalize(tvr(PSD))) %>%
  mutate(lognormPSD = log10(normPSD)) %>%
  mutate(dnormPSD = tvrd1(normPSD)) %>%
  left_join(select(demo, nsrrid, age_category)) %>%
  drop_na(age_category) %>%
  mutate(age_category = fct_collapse(factor(age_category), 
               younger = c('2','3'), 
               middle = c('4','5','6'),
               older = c('7','8','9','10'))) %>%
  mutate(mod_clock_hours = ((CLOCK_HOURS-22) %% 24)-2) %>%
  mutate(age_category = fct_relevel(age_category, 
                            "younger","middle","older")) %>%
  mutate(B = fct_relevel(B, 'SLOW',
                         'DELTA',
                         'THETA',
                         'ALPHA',
                         'SIGMA',
                         'BETA',
                         'GAMMA'))

# don't remove wake epochs (for WASO histogram)
df_all <- hypb10 %>% 
  #truncate_to_persistent_sleep_onesided() %>%
  split_id() %>%
  filter(B != 'TOTAL') %>%
  #filter(E < 500) %>%
  group_by(nsrrid, B) %>%
  #filter(PERSISTENT_SLEEP == 1) %>% #WASO==0
  mutate(normPSD = normalize(tvr(PSD))) %>%
  mutate(lognormPSD = log10(normPSD)) %>%
  mutate(dnormPSD = tvrd1(normPSD)) %>%
  left_join(select(demo, nsrrid, age_category)) %>%
  drop_na(age_category) %>%
  mutate(age_category = fct_collapse(factor(age_category), 
                                     younger = c('2','3'), 
                                     middle = c('4','5','6'),
                                     older = c('7','8','9','10'))) %>%
  mutate(mod_clock_hours = ((CLOCK_HOURS-22) %% 24)-2) %>%
  mutate(age_category = fct_relevel(age_category, 
                                    "younger","middle","older")) %>%
  mutate(B = fct_relevel(B, 'SLOW',
                         'DELTA',
                         'THETA',
                         'ALPHA',
                         'SIGMA',
                         'BETA',
                         'GAMMA'))

ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, fill=age_category))+
  geom_histogram(bins=100,alpha=0.5, position='identity')+
  scale_fill_brewer(palette='Set1')
ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours))+
  xlim(0,4)+
  geom_histogram(bins=100,alpha=0.5, position='identity')+
  scale_fill_brewer(palette='Set1')


ggplot(df, aes(x=E,y=normPSD,color=factor(age_category)))+
  stat_summary_bin(bins=100, geom='linerange', alpha=0.5)+
  stat_summary_bin(bins=100, geom='point', size=0.5)+
  facet_wrap(~B, scales='free')+
  scale_color_brewer(palette='Set1')

ggplot(df, aes(x=E,y=normPSD))+
  stat_summary_bin(bins=100, geom='linerange', alpha=0.5)+
  stat_summary_bin(bins=100, geom='point', size=0.5)+
  facet_wrap(~B, scales='free')+
  scale_color_brewer(palette='Set1')

ggplot(df, aes(x=mod_clock_hours,y=dnormPSD))+#,color=factor(age_category)))+
  stat_summary_bin(bins=100, geom='linerange', alpha=0.5)+
  stat_summary_bin(bins=100, geom='point', size=0.5)+
  facet_wrap(~B, scales='free')+
  scale_color_brewer(palette='Set1')+
  xlim(0,4)

ggplot(df, aes(x=mod_clock_hours,y=dnormPSD,color=factor(age_category)))+
  stat_summary_bin(bins=100, geom='linerange', alpha=0.5)+
  stat_summary_bin(bins=100, geom='point', size=0.5)+
  facet_wrap(~B, scales='free')+
  scale_color_brewer(palette='Set1')+
  xlim(0,4)

ggplot(df, aes(x=E,y=dnormPSD))+
  geom_smooth()+
  scale_color_brewer(palette='Set1')+
  #xlim(1,3)+
  facet_wrap(~B, scales='free')

ggplot(df, aes(x=E,y=dnormPSD, color=factor(age_category)))+
  geom_smooth()+
  scale_color_brewer(palette='Set1')+
  #xlim(1,3)+
  facet_wrap(~B, scales='free')

ggplot(df, aes(x=mod_clock_hours,y=normPSD))+
  stat_summary_bin(bins=100, geom='linerange', alpha=0.5)+
  stat_summary_bin(bins=100, geom='point', size=0.5)+
  scale_color_brewer(palette='Set1')+
  xlim(0,4)+
  facet_wrap(~B, scales='free')

ggplot(df, aes(x=mod_clock_hours,y=dnormPSD))+
  geom_point(size=0.5,alpha=0.05)+
  scale_color_brewer(palette='Set1')+
  xlim(0,4)+
  facet_wrap(~B, scales='free')


ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=df, aes(x=mod_clock_hours, y=normPSD, color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('Scaled power')

ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=df, aes(x=mod_clock_hours, y=dnormPSD, color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('Time-derivatives')

ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=df, aes(x=mod_clock_hours, 
                                y=normalize(diffx(dnormPSD)), 
                                color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('2nd derivative')

# just one
ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=filter(df, B=='DELTA'), aes(x=mod_clock_hours+0.2, 
                                y=dnormPSD, 
                                color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('Time-derivative (t + 12 min)')


ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=filter(df, B=='DELTA'), aes(x=mod_clock_hours+0.2, 
                                                    y=detrend(normPSD), 
                                                    color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('Detrended bandpower')

# by age
ggplot(filter(df_all, WASO==1), aes(x=mod_clock_hours, y=..density..))+
  xlim(1.25,4)+
  geom_histogram(bins=100,alpha=0.5)+
  stat_summary_bin(data=filter(df, B=='DELTA'), aes(x=mod_clock_hours+0.2, 
                                                    y=dnormPSD, 
                                                    color=B),
                   bins=100, geom='path', alpha=0.5, size=2)+
  scale_color_brewer(palette='Set1')+
  ggtitle('Time-derivative (t + 12 min)')+
  facet_wrap(~age_category, nrow=3)

##### CYCLES #####
labels <-  c(`1`='SLOW', 
             `2`='DELTA',
             `3`='THETA',
             `4`='ALPHA',
             `5`='SIGMA',
             `6`='BETA',
             `7`='GAMMA')

#derivative
ggplot(df %>% drop_na(CYCLE) %>% filter(CYCLE<5),
       aes(x=CYCLE_POS_REL, y=dnormPSD, color=age_category))+
  stat_summary_bin(bins=10, geom='linerange', alpha=0.9)+
  stat_summary_bin(bins=10, geom='path', alpha=0.9)+
  facet_grid(B~CYCLE, scales='free')+
  scale_color_brewer(palette='Set1')+
  xlim(0, 0.25)

#normalized
ggplot(df_all %>% drop_na(CYCLE) %>% filter(CYCLE<5),
       aes(x=CYCLE_POS_REL, y=normPSD, color=age_category))+
  stat_summary_bin(bins=50, geom='linerange', alpha=0.9)+
  stat_summary_bin(bins=50, geom='path', alpha=0.9)+
  facet_grid(B~CYCLE)+
  scale_color_brewer(palette='Set1')+
  scale_fill_brewer(palette='Set1')+
  scale_y_log10()

#stage histogram
ggplot(mutate(df_all, STAGE=recode(STAGE, NREM4='NREM3')) %>%
       drop_na(CYCLE) %>% 
         filter(CYCLE<5, STAGE != '?'), 
              aes(x=CYCLE_POS_REL,fill=STAGE,color=STAGE))+
         geom_histogram(position='fill')+
  facet_grid(age_category~CYCLE)+
  scale_fill_brewer(palette='Set1')+
  scale_color_brewer(palette='Set1')
  
    
