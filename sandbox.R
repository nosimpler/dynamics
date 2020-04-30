library(tidyverse)
library(broom)
library(corrr)
library(NMF)
#### source other files
pd <- function(a) paste(datadir, a, sep='/')
ps <- function(a) paste(sourcedir, a, sep='/')
sourcedir <- '~/dyn/src/dynamics'
datadir <- '~/dyn/data/chat/tab'


source(ps('figures.R'))
source(ps('distances0.2.R'))
source(ps('chat.R'))
source(ps('cleandata.R'))
source(ps('regression.R'))
h <- load_hypno()
ball <- load_bands() %>% append_stage_info(., h)
# measures <- c('erp')
# distances2 <- NULL
# bands <- c("SLOW", "DELTA", "THETA", "ALPHA", "SIGMA", "BETA", "GAMMA")#, "TOTAL")
# for (measure in measures){
#   for (ch in unique(ball$baseline$CH)){
#     for (band in bands){
#       print(ch)
#       print(band)
#       dist_mat <- as_tibble(get_distances(ball, ch, band, measure, ids=b$IDs))
#       dist_mat$CH <- ch
#       dist_mat$B <- band
#       dist_mat$MEAS <- measure
#       distances2 <- rbind(distances2, dist_mat)
#     }
#   }
# }
# have to rewrite for n/2 rather than 341
load("~/dyn/rdata/distances_erp_first300.Rdata")

n <- dim(dist_mat)[2]-3
mds_baseline <- function(x) {
   x_new <- as_tibble(cmdscale(as.matrix(x[1:(n/2),1:(n/2)]), 3)[,3]) %>%
     rm_outliers()
   rownames(x_new) <- ball$demo$nsrrid
   rownames_to_column(x_new,var='nsrrid')
   
}

mds_followup <- function(x) {
  x_new <- as_tibble(cmdscale(as.matrix(x[(n/2+1):n,(n/2+1):n]), 3)[,3]) %>%
    rm_outliers()
  rownames(x_new) <- ball$demo$nsrrid
  rownames_to_column(x_new,var='nsrrid')
  
}

nmf_baseline <- function(x) {
  x_new <- as_tibble(nmf(as.matrix(x[1:341,1:341]), 1)@fit@W) %>%
    rename(value=V1)
  rownames(x_new) <- ball$demo$nsrrid
  rownames_to_column(x_new,var='nsrrid')
    
}

nmf_followup <- function(x) {
  x_new <- as_tibble(nmf(as.matrix(x[342:682,342:682]),1)@fit@W) %>%
    rename(value=V1)
  rownames(x_new) <- ball$demo$nsrrid
  rownames_to_column(x_new,var='nsrrid')
}

dist_diagonal <- function(x){
  x_new <- as_tibble(diag(as.matrix(x[1:(n/2), (n/2+1):n])))
}

#distance_t1 <- distances %>% group_by(B, CH) %>% 
  #group_modify(~mds(.x)) %>%
  

#distance_t2 <- distances2 %>% group_by(B, CH) %>% 
  #group_modify(~mds(.x))

#cor.test(distance_t1$V1, distance_t2$V1)

standard_measures <- c(
  'TST',
  'MINS_N1',
  'MINS_N2',
  'MINS_N3',
  'NREMC',
  'NREMC_MINS',
  'TIB',
  'TPST',
  'TWT',
  'MINS_REM',
  'SLP_LAT',
  'PER_SLP_LAT',
  'REM_LAT',
  'SLP_EFF',
  'SLP_MAIN_EFF',
  'WASO'
)

other_comparisons <- c('cog',
  'ess',
  'bmiz',
  'omahi3',
  'bpmavg',
  'avgsat',
  'ai_all')

comparators <- c(standard_measures, other_comparisons)



h$baseline_stats <- filter(h$baseline_stats, ID %in% unique(ball$demo$nsrrid))
h$followup_stats <- filter(h$followup_stats, ID %in% unique(ball$demo$nsrrid))
meas_bsl <- h$baseline_stats %>% select(standard_measures)
meas_bsl$age <- ball$demo$ageyear_at_meas
meas_bsl$male <- ball$demo$male
meas_bsl$cog <- ball$demo$bri13b
meas_bsl$ess <- ball$demo$epworthscore_adult
meas_bsl$race <- as.factor(ball$demo$race3)
meas_bsl$bmiz <- ball$demo$bmiz
meas_bsl$bpmavg <- ball$demo$bpmavg
meas_bsl$omahi3 <- ball$demo$omahi3
meas_bsl$avgsat <- ball$demo$avgsat
meas_bsl$ai_all <- ball$demo$ai_all
meas_bsl$nsrrid <- as.character(ball$demo$nsrrid)

meas_flp <- h$followup_stats %>% select(standard_measures)
meas_flp$age <- ball$demo$ageyear_at_meas
meas_flp$male <- ball$demo$male
meas_flp$cog <- ball$demo$bri13b
meas_flp$ess <- ball$demo$epworthscore_adult
meas_flp$race <- as.factor(ball$demo$race3)
meas_flp$bmiz <- ball$demo$bmiz
meas_flp$nsrrid <- as.character(ball$demo$nsrrid)
meas_flp$omahi3 <- ball$demo$omahi3
meas_flp$bpmavg <- ball$demo$bpmavg
meas_flp$avgsat <- ball$demo$avgsat
meas_flp$ai_all <- ball$demo$ai_all


pdc_retest <- distances %>% group_by(B,CH) %>%
  group_modify(~dist_diagonal(.x))
erp_retest <- distances2 %>% group_by(B,CH) %>%
  group_modify(~dist_diagonal(.x))

retest <- tibble(B=pdc_retest$B, CH=pdc_retest$CH, pdc=pdc_retest$value, erp=erp_retest$value)

retest_corr <- retest %>% group_by(CH,B) %>%
  group_modify(~tidy(cor.test(x=.$pdc,y=.$erp))) %>%
  select(CH, B, estimate, statistic, p.value) 



baseline_erp <- distances2 %>% group_by(B, CH) %>% 
  group_modify(~mds_baseline(.x))
  

followup_erp <- distances2 %>% group_by(B, CH) %>% 
  group_modify(~mds_followup(.x)) 
         
baseline_pdc <- distances %>% group_by(B, CH) %>% 
 group_modify(~mds_baseline(.x)) 

followup_pdc <- distances %>% group_by(B, CH) %>% 
  group_modify(~mds_followup(.x)) 


rvars_pdc_followup <- left_join(followup_pdc, meas_flp)

rvars_erp_followup <- left_join(followup_erp, meas_flp) 

rvars_pdc_baseline <- left_join(baseline_pdc, meas_bsl) 

rvars_erp_baseline <- left_join(baseline_erp, meas_bsl) 

rvars <- rbind(add_column(rvars_erp_followup,MEAS='erp', COND='followup'),
               add_column(rvars_erp_baseline,MEAS='erp', COND='baseline'),
               add_column(rvars_pdc_followup,MEAS='pdc', COND='followup'),
               add_column(rvars_pdc_baseline,MEAS='pdc', COND='baseline')
               )

rvars <- pivot_longer(rvars, 
                           cols=comparators, 
                           names_to='var', 
                           values_to='rhs') %>% 
  rename(lhs=value)

sigs <- rvars %>% group_by(CH, B, MEAS, COND, var) %>%
  group_modify(~tidy(lm(lhs ~ ., data=select(.,lhs,rhs,age,male,race, -nsrrid))))

# pdc/erp correlation
mds_corr <- rvars %>% group_by(CH,B,COND,var) %>%
  pivot_wider(names_from=MEAS, values_from=lhs) %>%
  filter(var=='ai_all') %>% #doesn't matter which one
  group_modify(~tidy(cor.test(x=.$erp,y=.$pdc))) %>%
  select(CH, B, COND, estimate, statistic, p.value) 

ggplot(mds_corr, aes(x=CH, y=B,color=estimate, size=abs(estimate)))+
  geom_point()+
  facet_wrap(~COND)+
  scale_color_distiller(palette='RdYlBu')

# test/retest correlation
retest_corr <- rvars %>% group_by(CH,B,MEAS,var) %>%
  pivot_wider(names_from=COND, values_from=lhs) %>%
  filter(var=='ai_all') %>% #doesn't matter which one
  group_modify(~tidy(cor.test(x=.$baseline,y=.$followup))) %>%
  select(CH, B, MEAS, estimate, statistic, p.value) 

ggplot(retest_corr, aes(x=CH, y=B,color=estimate, size=abs(estimate)))+
  geom_point()+
  facet_wrap(~MEAS)+
  scale_color_distiller(palette='RdYlBu')

  summarize(mds_pos, corr=correlate(.x, x=pdc, y=erp))
