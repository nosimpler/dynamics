# AR stuff
library(forecast)
library(sweep)

# bl <- dataset %>%
#   select(ID,E,B,CH,RELPSD,COND, STAGE_N,CYCLE) %>%
#   group_by(CH, B, COND, ID) %>%
#   filter(B != 'TOTAL') %>%
#   arrange(CH, B, E) %>%
#   filter(CYCLE==1) #%>%
  #mutate(RELPSD=rx(RELPSD)) 
  #filter(row_number() >= first(which(STAGE_N < 1))) #%>%
  #filter(row_number() < first(which(STAGE_N == 0)))

ar_coeff <- bl %>%
  group_modify(~sw_tidy(Arima(.$RELPSD, order=c(6,0,0), method="CSS")))

ar_bf <- ar_coeff %>% 
  pivot_wider(names_from=COND,values_from=estimate) %>% ungroup() 

corrs <- ar_bf %>% group_by(CH,B,term) %>%
  group_modify(~tidy(cor.test(.$baseline, .$followup)))

ggplot(ar_bf, aes(x=baseline, y=followup))+
  stat_smooth(method='lm')+
  geom_point(alpha=0.1,size=0.1)+
  facet_wrap(term~B, scales='free')

