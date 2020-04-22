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
# h <- load_hypno()
# ball <- load_bands() %>% append_stage_info(., h)
measures <- c('erp')
distances2 <- NULL
bands <- c("SLOW", "DELTA", "THETA", "ALPHA", "SIGMA", "BETA", "GAMMA")#, "TOTAL")
for (measure in measures){
  for (ch in unique(ball$baseline$CH)){
    for (band in bands){
      print(ch)
      print(band)
      dist_mat <- as_tibble(get_distances(ball, ch, band, measure, ids=b$IDs))
      dist_mat$CH <- ch
      dist_mat$B <- band
      dist_mat$MEAS <- measure
      distances2 <- rbind(distances2, dist_mat)
    }
  }
}
# have to rewrite for n/2 rather than 341
n <- dim(dist_mat)[2]-3
mds <- function(x) as_tibble(cmdscale(as.matrix(x[,1:n]), 3)[,1])
mds_baseline <- function(x) {
   x_new <- as_tibble(cmdscale(as.matrix(x[1:(n/2),1:(n/2)]), 3)[,1])
   rownames(x_new) <- ball$demo$nsrrid
   rownames_to_column(x_new,var='nsrrid')
}

mds_followup <- function(x) {
  x_new <- as_tibble(cmdscale(as.matrix(x[(n/2+1):n,(n/2+1):n]), 3)[,1])
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


h$baseline_stats <- filter(h$baseline_stats, ID %in% unique(ball$demo$nsrrid))
meas <- h$baseline_stats %>% select(standard_measures)
meas$age <- ball$demo$ageyear_at_meas
meas$male <- ball$demo$male
meas$cog <- ball$demo$bri13b
meas$ess <- ball$demo$epworthscore_adult
meas$race <- as.factor(ball$demo$race3)
meas$bmiz <- ball$demo$bmiz
meas$nsrrid <- as.character(ball$demo$nsrrid)
  
baseline_erp <- distances2 %>% group_by(B, CH) %>% 
  group_modify(~mds_baseline(.x)) %>%
  

followup_erp <- distances2 %>% group_by(B, CH) %>% 
  group_modify(~mds_followup(.x)) 
         
#baseline_pdc <- distances %>% group_by(B, CH) %>% 
# group_modify(~mds_baseline(.x)) 

#followup_pdc <- distances %>% group_by(B, CH) %>% 
#  group_modify(~mds_followup(.x)) 

#baseline_pdc_nmf <- distances %>% group_by(B,CH) %>%
#  group_modify(~nmf_baseline(.x))

#followup_pdc_nmf <- distances %>% group_by(B,CH) %>%
#  group_modify(~nmf_followup(.x))

#rvars_pdc_followup <- left_join(followup_pdc, meas)

rvars_erp_followup <- left_join(followup_erp, meas) 

#rvars_pdc_baseline <- left_join(baseline_pdc, meas) 

rvars_erp_baseline <- left_join(baseline_erp, meas) 

#rvars_pdc_baseline_nmf <- left_join(baseline_pdc_nmf, meas)
#rvars_pdc_followup_nmf <- left_join(followup_pdc_nmf, meas)
#sigs_pdc_baseline <- rvars_pdc_baseline %>% group_by(CH, B) %>%
#  group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid)))) %>%
#  add_column(MEAS='pdc', COND='baseline')

#sigs_pdc_followup <- rvars_pdc_followup %>% group_by(CH, B) %>%
# group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid)))) %>%
#  add_column(MEAS='pdc', COND='followup')

sigs_erp_baseline <- rvars_erp_baseline %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid))))%>%
  add_column(MEAS='erp', COND='baseline')

sigs_erp_followup <- rvars_erp_followup %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid)))) %>%
  add_column(MEAS='erp', COND='followup')

#sigs_pdc_baseline_nmf <- rvars_pdc_baseline_nmf %>% group_by(CH, B) %>%
#  group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid)))) %>% 
#  add_column(MEAS='pdc', COND='baseline')

#sigs_pdc_followup_nmf <- rvars_pdc_followup_nmf %>% group_by(CH, B) %>%
#  group_modify(~tidy(lm(value ~ ., data=select(.x,-nsrrid)))) %>% 
#  add_column(MEAS='pdc', COND='followup')

sigs <- rbind(sigs_erp_baseline, 
              sigs_erp_followup) 
              #sigs_pdc_baseline, 
              #sigs_pdc_followup)

rvars <- rbind(add_column(rvars_erp_followup,MEAS='erp', COND='followup'),
               add_column(rvars_erp_baseline,MEAS='erp', COND='baseline')
               #add_column(rvars_pdc_followup,MEAS='pdc', COND='followup'),
               #add_column(rvars_pdc_baseline,MEAS='pdc', COND='baseline')
               )


  rvars %>%
    pivot_wider(names_from=COND, values_from=value) %>%
    select(baseline, followup) %>%
    group_modify(~correlate(.x)) %>%
    filter(rowname=='baseline', MEAS=='pdc') %>%
    select(-baseline, -rowname) %>%
    rename(corr=followup) %>%
    ggplot(aes(x=CH, y=B,size=abs(corr), color=corr)) +
    geom_point()+scale_color_distiller(palette="RdYlBu")+
    theme_minimal()
