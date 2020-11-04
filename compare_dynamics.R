# comparative dynamics 

# construct data table tagged with demographic info
library(patchwork)

fact <- 'male'
H_demo0 <- H_sindy %>%
  separate(ID, c("study", 'session', 'nsrrid')) %>%
  mutate(nsrrid = as.numeric(nsrrid))
cmpr_dyn <- left_join(select(H_demo, -PERIOD),
                      demo) %>%
  group_by(nsrrid) %>%
  mutate(E = E-min(E)) %>%
  filter(E < 100)
# cmpr_dyn <- filter(cmpr_dyn, age_category < 10)
# 
# cmpr_dyn$NEAREST_WAKE_SIGNED <- 
#   diffx(cmpr_dyn$NEAREST_WAKE)*cmpr_dyn$NEAREST_WAKE
# 
# cmpr10 <- filter(cmpr_dyn, E==10)


p1 <- ggplot(cmpr_dyn, aes(x=E, y=drV5, 
                     color=as.factor(aspirintherapy)))+
  geom_smooth()+
  geom_point(size=0.5, alpha=0.1)+
  scale_color_brewer(palette='Set1')+
  geom_vline(xintercept=10)

p2 <- ggplot(filter(cmpr_dyn, E==10), aes(y=drV5, 
                                    x=as.factor(aspirintherapy)))+
  stat_summary()+
  ggbeeswarm::geom_beeswarm(alpha=0.1)

formula <- y ~ x
# p3 <- ggplot(filter(cmpr_dyn, E==10), aes(y=drV5, 
#                                           x=IBUPRMON))+
#   geom_point()+
#   ggpmisc::stat_poly_eq(formula=formula, parse=TRUE)+
#   geom_smooth(method='lm')+
#   scale_color_brewer(palette='Set1')
tidy(lm(data=cmpr10, drV5~aspirintherapy+age))

#ggplot(wdemo, aes(x=NEAREST_WAKE, y=drV3))+geom_point()

p1/p2 #/p3
h5 <- filter(hypnoH, component=='V5')

# find betamax
betamax <- h5 %>%
  group_by(ID) %>%
  filter(CYCLE==1) %>%
  arrange(E) %>%
  mutate(E = E-min(E)) %>%
  #filter(E < 100) %>%
  filter(value == max(value)) %>%
  #filter(WASO==1) %>%
  separate(ID, c('study', 'session', 'nsrrid')) %>%
  mutate(nsrrid = as.numeric(nsrrid)) %>%
  select(-PERIOD) %>%
  left_join(demo)

ggplot(betamax, aes(x=CYCLE_POS_REL, 
                    fill=as.factor(age_category))) + 
  geom_histogram(bins=50)
