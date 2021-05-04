# average over sessions, then do stats across individuals, and see repeatability

indiv_means <- function(db){
  db %>%
    select(ID, E, F, STAGE_N, KOP) %>%
    group_by(ID,F,STAGE_N) %>%
    summarize(INDIV_MEAN = mean(KOP)) %>%
    collect()
}

# #means <- indiv_means(hypsync)
 ggplot(means %>% filter(STAGE_N >= -3), aes(x=F, y=INDIV_MEAN, color=factor(STAGE_N)))+
   stat_summary_bin(bins=100, geom='linerange')+
   scale_color_brewer(palette='Set1')
#
blfu <- split_id(means) %>%
  pivot_wider(names_from = session, values_from=INDIV_MEAN) %>%
  group_by(F, STAGE_N) %>%
  filter(STAGE_N > -4) %>%
  drop_na() %>%
  group_modify(~tidy(cor.test(.$baseline, .$followup)))

ggplot(blfu, aes(x=F, y=estimate, color=factor(STAGE_N)))+
  stat_summary_bin(bins=60, geom='pointrange')+
  scale_color_brewer(palette='Set1')

g <- 'nep12b_nepsy'
demo_sync <- left_join(means %>%
                         split_id() %>%
                         filter(session=='baseline'),
                       select(demo, nsrrid, ageyear_at_meas, !!sym(g), male)) %>%
  ungroup() %>%
  select(-study, -session) %>%
  mutate(!!g := outliers(outliers(!!sym(g)))) %>%
  mutate(INDIV_MEAN = outliers(outliers(INDIV_MEAN)))
  #mutate(bri13b = cut_number(bri13b, 3, ordered_result=TRUE))

sync_stats <- demo_sync %>%
  group_by(F, STAGE_N) %>%
  drop_na() %>%
  group_modify(~tidy(lm(INDIV_MEAN ~ ., data=.)))

ggplot(sync_stats %>%
         filter(term != "(Intercept)"), aes(x=F, y=-log10(p.value), color=factor(term)))+
  geom_point()+
  facet_wrap(~STAGE_N)+
  scale_color_brewer(palette='Set1')


ggplot(demo_sync, aes(x=F, y=INDIV_MEAN, color=factor(ageyear_at_meas)))+
  stat_summary_bin(bins=60, geom='linerange')+facet_wrap(~STAGE_N)+
  scale_color_brewer(palette='Set1')

ggplot(demo_sync %>% filter(F==5), aes(x=!!sym(g), y=INDIV_MEAN, color=ageyear_at_meas))+
  ggbeeswarm::geom_beeswarm()+geom_smooth()+facet_wrap(~STAGE_N)

ggplot(demo_sync %>% filter(F==5, ageyear_at_meas != 10, STAGE_N !=-4), aes(x=!!sym(g), y=INDIV_MEAN, color=ageyear_at_meas))+
  ggbeeswarm::geom_beeswarm()+geom_smooth()+facet_grid(ageyear_at_meas~STAGE_N)
