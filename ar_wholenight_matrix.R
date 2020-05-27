# AR stuff
library(forecast)
library(sweep)


bl <- dataset %>%
  select(ID,E,B,CH,RELPSD,COND, STAGE_N,CYCLE) %>%
  group_by(CH, B, COND, ID) %>%
  filter(B != 'TOTAL') %>%
  arrange(CH, B, E) %>%
  filter(CYCLE>=1) %>%
  mutate(RELPSD=log(RELPSD))
  #filter(row_number() >= first(which(STAGE_N < 1))) #%>%
  #filter(row_number() < first(which(STAGE_N == 0)))

bl_4var <- pivot_wider(bl, names_from=c(CH,B), values_from=RELPSD)

ar_coeff <- bl %>% #mutate(RELPSD=rx(RELPSD)) %>%
  group_modify(~sw_tidy(Arima(.$RELPSD, order=c(20,2,0), method="CSS")))

ar_bf <- ar_coeff %>% 
  pivot_wider(names_from=COND,values_from=estimate) %>% ungroup() 

#var_coeff <- bl_4var %>%
#  group_modify(~sw_tidy(fitVAR(.[,6:61])))

# rvars <- select(ar_bf, c(ID,CH, B,term, baseline)) %>% 
#   left_join(filter(sleepstats, COND=='baseline'))
# 
# rvars_test <- filter(rvars, CH=='O2', B=='SLOW',term=='ar1')
# summary(lm(baseline~SLP_EFF+REM_LAT+PER_SLP_LAT+TPST+MINS_N1+MINS_N2+MINS_N3+MINS_REM,data=rvars_test))
# 
corrs <- ar_bf %>% group_by(CH,B,term) %>%
  group_modify(~tidy(cor.test(.$baseline, .$followup)))

make_tensar <- function(df){
  dfa <- array(data = df$estimate, 
               dim=c(length(unique(df$ID)), 
                     3,
                     2,
                     length(unique(df$B)), 
                     length(unique(df$CH))), 
               dimnames=list(unique(df$ID), unique(df$term), c('b','f'), unique(df$B), unique(df$CH)))
  as.tensor(dfa)
}

# blO2 <- bl %>% filter(CH=='O2', B=='SLOW', COND=='baseline', ID=='300001')
# ar1_O2 <- Arima(blO2$RELPSD, c(1,1,0), method='CSS')
# ar2_O2 <- Arima(blO2$RELPSD, c(1,0,0), method='CSS')
# ar100_O2 <- Arima(blO2$RELPSD, c(2,1,0), method='CSS')
# plot(pacf(blO2$RELPSD, lag=160))
# 
# plot(blO2$RELPSD, pch=20)
# lines(fitted(ar1_O2), col='blue')
# lines(fitted(ar2_O2), col='red')
# lines(fitted(ar100_O2), col='green')
# plot(spectrum(blO2$RELPSD, method='ar', order=20))
# tensar <- make_tensar(ar_coeff)
# #td <- cp(tensar, 1)$U[[1]]
# td <- NTF(tensar,2)[['A']][[1]]
# td <- t(td)
# names <- names(tensar@data[,1,1,1,1])
# names <- as.numeric(names)
# rownames(td) <- NULL
# data <- cbind(names,td)
# tdt <- as_tibble(data) %>% rename(ID=names)
#   
# sleepstat <- mutate(sleepstats, ID = as.numeric(ID)) %>% 
#   select(standard_measures, COND,ID)
# 
# meas <- left_join(tdt,filter(sleepstat, COND=='baseline'))
# summary(lm(V3~ ., data=select(meas,-COND,-ID,-V2)))

regressors <- left_join(demo2, filter(ar_bf, term=='ar4'), by="ID")
regressors_C3b <- filter(regressors, B=='SIGMA', CH=='C3')  %>% 
  replace(is.na(.), 0)# %>%

regressed <- tibble()
for (clmn in colnames(select_if(regressors_C3b, is.numeric))){
  #print(clmn)
  variables <- c(clmn, 'ageyear_at_meas', 'male', 'as.factor(race3)')
  f <- as.formula(
    paste('baseline', 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  lmfit <- tidy(lm(f, data=regressors_C3b))
  
  regressed <- rbind(regressed, 
                     filter(lmfit, term==sym(!!clmn)))
}
phack <- filter(arrange(regressed, p.value), p.value < 0.05)
ehack <- filter(arrange(regressed, estimate), p.value < 0.05)
print(phack)
ggplot(regressors_C3b, aes(y=baseline, x=slpprdp))+
  ggbeeswarm::geom_beeswarm(color='blue', size=0.1)+stat_summary()

ggplot(corrs, aes(x=CH, y=B, fill=estimate))+
  geom_tile()+
  facet_wrap(~term)


ggplot(filter(ar_bf,term=='ar4',B=="SIGMA"), aes(x=baseline, y=followup))+
  stat_summary_bin()+
  stat_smooth(method='lm')+
  geom_point(size=0.1)+
  facet_wrap(~CH, scales='free')


