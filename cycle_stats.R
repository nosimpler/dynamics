
# computing stats on first semicycle for demographics
cycle_max <- H_sindy %>% group_by(ID) %>%
  summarize(slopedelta = (max(drV6))-min(drV6)) %>%
  separate(ID, c('study', 'condition', 'nsrrid')) %>% 
  mutate(nsrrid = as.numeric(nsrrid)) %>%
  pivot_wider(names_from=condition, values_from=slopedelta) %>%
  filter(baseline < 75, followup < 75) %>%
  mutate(mean=(baseline+followup)/2)

wdemo <- left_join(cycle_max, demo) %>% filter(!is.na(mean))
cor.test(cycle_max$baseline, cycle_max$followup)
ggplot(cycle_max, aes(x=baseline, y=followup))+geom_point()
regress_all(wdemo, 'mean')
ggplot(wdemo, aes(y=mean, x=ageyear_at_meas))+
  ggbeeswarm::geom_beeswarm(alpha=0.3)+
  stat_summary()



  