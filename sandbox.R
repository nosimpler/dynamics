library(tidyverse)
library(broom)
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

#ball <- load_bands() %>% append_stage_info(., h)
#b <- filterchb(ball, ch='C3', band='DELTA')
#tsm <- build_ts_matrix(b, var='PSD')
#tsmd <- xdx_all(tsm)

#mat <- as.matrix(TSDatabaseDistances(t(tsmd), 'pdc'))
#mat <- as.matrix(TSDatabaseDistances(t(tsmd), 'erp', g=0))
#print(traitlikeness(mat))
#b <- get_distance_measures(b)
#b$baseline_dist <- baseline_submatrix(b$dist_mat$lbk)
#b$followup_dist <- followup_submatrix(b$dist_mat$lbk)
#m <- regress_on_standard_measures(b,h)
#m <- regress_on_demo_and_standard(b,h)
#print(m)
#plot_mds_var(b$demo, b$baseline_dist, 1, 'cog')
#measures <- c('pdc', 'erp', 'ncd', 'klb', )
# distances <- NULL
# bands <- c("SLOW", "DELTA", "THETA", "ALPHA", "SIGMA", "BETA", "GAMMA")
#   for (ch in unique(ball$baseline$CH)){
#     for (band in bands){
#       print(ch)
#       print(band)
#       dist_mat <- as_tibble(get_distances(ball, ch, band, 'erp', ids=b$IDs))
#       dist_mat$CH <- ch
#       dist_mat$B <- band
#       dist_mat$MEAS <- 'erp'
#       distances <- rbind(distances, dist_mat)
#     }
#   }

component <- 2
mds <- function(x) as_tibble(cmdscale(as.matrix(x[,1:682]), 3)[,1])
mds_baseline <- function(x) {
   x_new <- as_tibble(cmdscale(as.matrix(x[1:341,1:341]), 3)[,1])
   rownames(x_new) <- b$demo$nsrrid
   rownames_to_column(x_new,var='nsrrid')
}

mds_followup <- function(x) {
  x_new <- as_tibble(cmdscale(as.matrix(x[342:682,342:682]), 3)[,1])
  rownames(x_new) <- b$demo$nsrrid
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


h$baseline_stats <- filter(h$baseline_stats, ID %in% unique(b$demo$nsrrid))
meas <- h$baseline_stats %>% select(standard_measures)
meas$age <- b$demo$ageyear_at_meas
meas$male <- b$demo$male
meas$cog <- b$demo$bri13b
meas$ess <- b$demo$epworthscore_adult
meas$race <- as.factor(b$demo$race3)
meas$bmiz <- b$demo$bmiz
meas$nsrrid <- as.character(b$demo$nsrrid)
  
baseline_erp <- distances %>% group_by(B, CH) %>% 
  group_modify(~mds_baseline(.x)) 

followup_erp <- distances %>% group_by(B, CH) %>% 
  group_modify(~mds_followup(.x)) 
         
baseline_pdc <- distances2 %>% group_by(B, CH) %>% 
 group_modify(~mds_baseline(.x)) 

followup_pdc <- distances2 %>% group_by(B, CH) %>% 
  group_modify(~mds_followup(.x)) 

rvars_pdc_followup <- left_join(followup_pdc, meas) %>%
  select(-nsrrid)

rvars_erp_followup <- left_join(followup_erp, meas) %>%
  select(-nsrrid)

rvars_pdc_baseline <- left_join(baseline_pdc, meas) %>%
  select(-nsrrid)

rvars_erp_baseline <- left_join(baseline_erp, meas) %>%
  select(-nsrrid)

sigs_pdc_baseline <- rvars_pdc_baseline %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=.x))) %>%
  filter(p.value < 0.05)

sigs_pdc_followup <- rvars_pdc_followup %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=.x))) %>%
  filter(p.value < 0.05)

sigs_erp_baseline <- rvars_erp_baseline %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=.x))) %>%
  filter(p.value < 0.05)

sigs_erp_followup <- rvars_erp_followup %>% group_by(CH, B) %>%
  group_modify(~tidy(lm(value ~ ., data=.x))) %>%
  filter(p.value < 0.05)

print(inner_join(sigs_pdc_baseline, sigs_pdc_followup, by=c('B','CH','term')))
print(inner_join(sigs_erp_baseline, sigs_erp_followup, by=c('B','CH','term')))
