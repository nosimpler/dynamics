# functions for fitting and plotting NTP model
library(patchwork)
library(broom)
ntp1 <- dataset %>%
  select(ID,E,B,CH,PSD,COND, STAGE_N,CYCLE, CYCLE_POS_REL) %>%
  group_by(CH, B, COND, ID) %>%
  filter(CYCLE==1) %>%
  top_n(-50, E) %>%
  filter(B %in% c('BETA', 'SIGMA', 'DELTA')) #%>%
  #mutate(RELPSD=RELPSD/max(RELPSD))
for (id in head(IDlist, 10)){

p1 <- ggplot(filter(ntp1, ID==id), aes(x=E, y=log(PSD),color=B))+
  stat_summary_bin(bins=50)+
  geom_smooth(method='lm')+
  #geom_point()+
  facet_wrap(~COND, nrow=2)
plot(p1)
}

ntp2 <- ntp1 %>%
  group_modify(~tidy(lm(log(PSD)~E, data=.)))
ntp3 <- filter(ntp2, term=='E')
pntp_bl <- filter(ntp3, COND=='baseline') %>% ungroup()
pntp_fl <- filter(ntp3, COND=='followup') %>% ungroup()
pntp <- mutate(pntp_bl, estimate.fl = pntp_fl$estimate)
ggplot(pntp, aes(x=estimate, y=estimate.fl))+
  geom_point(size=0.1)+
  facet_grid(B~CH,scales='free')+
  geom_density2d()+
  stat_summary_bin()+
  geom_abline(slope=1, intercept=0)

pntp_slope <- pntp %>% group_by(B,CH) %>% 
  group_modify(~tidy(lm(estimate~estimate.fl+0, data=.)))

pntp_cor <- pntp %>% group_by(B,CH) %>% 
  group_modify(~tidy(cor.test(.$estimate, .$estimate.fl)))

demo2 <- demo %>% rename(ID=nsrrid)
demo2 <- demo2 %>% mutate(ID=as.character(ID))
regressors <- left_join(demo2, pntp, by="ID")
regressors_C3b <- filter(regressors, B=='SIGMA', CH=='T3')  %>% 
  replace(is.na(.), 0)# %>%
  #filter(abs(estimate-estimate.fl<0.01))
regressed <- tibble()
for (clmn in colnames(select_if(regressors_C3b, is.numeric))){
  #print(clmn)
  variables <- c(clmn, 'ageyear_at_meas', 'male', 'as.factor(race3)')
  f <- as.formula(
    paste('estimate', 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  lmfit <- tidy(lm(f, data=regressors_C3b))
  
  regressed <- rbind(regressed, 
                     filter(lmfit, term==sym(!!clmn)))
}
phack <- filter(arrange(regressed, p.value), p.value < 0.05)
ehack <- filter(arrange(regressed, estimate), p.value < 0.05)
print(phack, n=nrow(phack))
ggplot(regressors_C3b, aes(y=estimate, x=t3dur))+
  ggbeeswarm::geom_beeswarm(color='blue', size=0.1)+stat_summary()