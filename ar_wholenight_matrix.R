# AR stuff
library(forecast)
library(sweep)


bl <- dataset %>%
  select(ID,E,B,CH,RELPSD,COND, STAGE_N,CYCLE) %>%
  group_by(CH, B, COND, ID) %>%
  filter(B != 'TOTAL') %>%
  arrange(CH, B, E) #%>%
#   filter(CYCLE==1) #%>%
  #mutate(RELPSD=rx(RELPSD)) 
  #filter(row_number() >= first(which(STAGE_N < 1))) #%>%
  #filter(row_number() < first(which(STAGE_N == 0)))

ar_coeff <- bl %>% #mutate(RELPSD=rx(RELPSD)) %>%
  group_modify(~sw_tidy(Arima(.$RELPSD, order=c(2,0,0), method="CSS")))

ar_bf <- ar_coeff %>% 
  pivot_wider(names_from=COND,values_from=estimate) %>% ungroup() 

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
tensar <- make_tensar(ar_coeff)
td <- cp(tensar, 1)$U[[1]]
data[,1,1,1,1]
names <- names(tensar@data[,1,1,1,1])
names <- as.numeric(names)
rownames(td) <- NULL
data <- cbind(names,td)
tdt <- as_tibble(data) %>% rename(nsrrid=names)

ggplot(filter(ar_bf,term=='ar1',B=="SLOW"), aes(x=baseline, y=followup))+
  stat_summary_bin()+
  stat_smooth(method='lm')+
  geom_point(size=0.1)+
  facet_wrap(~CH, scales='free')

