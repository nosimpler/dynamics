# MDS/PCA Regression on distance matrices
library(tidyverse)

`%notin%` <- negate(`%in%`)


#exclude <- c(300058,300368,300668) # for some reason PDC and EDR distances go to NA here


dist_matrix <- function(dist_tab, D){ 
  dist_tab %>% 
    select('COND','ID1', 'ID2', sym(!!D)) %>%
    pivot_wider(names_from=c('COND','ID1'), values_from=sym(!!D)) %>%
    select(2:length(.)) %>%
    as.matrix()
}

big_dist_matrix <- function(baseline, followup, cross, D){
  baseline <- baseline %>% select(ID1, ID2, sym(!!D))
  followup <- followup %>% select(ID1, ID2, sym(!!D))
  cross <- cross %>% select(ID1, ID2, sym(!!D))
  baseline$COND <- 'baseline'
  followup$COND <- 'followup'
  fb_cross <- cross
  fb_cross$COND <- 'baseline'
  bf_cross <- cross
  bf_cross$COND <- 'followup'
  bb <- t(dist_matrix(baseline, D))
  ff <- t(dist_matrix(followup, D))
  bf <- t(dist_matrix(bf_cross, D))
  fb <- dist_matrix(fb_cross, D)
  mat_tot <- rbind(cbind(bb, bf), cbind(fb, ff))
  mat_tot <- t(mat_tot)
}
#bdc <- big_dist_matrix(baselinetable, followuptable, crosstable, "D_erp")
bdc <- erpdist
#nmf_fit <- nmf(bdc, 3, seed='nndsvd')

#dist_mat <- dist_matrix(dist_diag_l_baseline, 'erp')
hb <- h$baseline_stats
hf <- h$followup_stats
hyp_stats <- full_join(hb,hf)


# transition counts from hypno.R
htb <- tca$baseline
htb$COND <- 'baseline'
htf <- tca$followup
htf$COND <- 'followup'
ht <- full_join(htb, htf)
htbn <- htb %>% select(ID,  starts_with('n'))
htfn <- htf %>% select(ID,  starts_with('n'))
htn <- full_join(htbn, htfn)

htbp <- htb %>% select(ID,  starts_with('p'))
htfp <- htf %>% select(ID,  starts_with('p'))
htp <- full_join(htbp, htfp)

not_all_na <- function(x) any(!is.na(x))

demo <- h$demo %>% select_if(not_all_na)
demo1 <- demo
demo1$COND <- 'baseline'
demo2 <- demo
demo2$COND <- 'followup'
demo3 <- rbind(demo1, demo2)
#pc <- prcomp(dist_mat)
fit <- cmdscale(bdc, k=3)

rvars <- htp %>%
                    mutate(
                      mds1=fit[,1]
                      #mds1=nmf_fit@fit@H[2,]
                           ) %>%
  mutate(male=as.factor(demo3$male), 
         age=demo3$ageyear_at_meas, 
         race=as.factor(demo3$race3),
         bmiz=demo3$bmiz,
         br=demo3$bri13b,
         qol=demo3$pedsql_emotional_score_chld,
         tst=hyp_stats$TST,
         bpm=demo3$bpmavg) %>%
  mutate_all(~replace(., is.na(.), 0)) #%>%
  #filter(abs(mds1) < 2e-06)
#idx <- nn(rvars$mds1)[['Location of Outlier']]
#rvars <- rvars[-idx,]
hist(rvars$mds1)
summary(lm(mds1~., data=select(rvars,-ID)),
        signif.stars=TRUE)
coeffs <- summary(lm(mds1~., data=select(rvars,-ID)),
        signif.stars=TRUE)$coefficients

partial_fits <- NULL
for (column in colnames(select(rvars, -ID))){
  row <- list()
  data=select(rvars, mds1, sym(!!column))
  regfits <- summary(lm(mds1~.+0,data=data ))
  coeffs <- regfits$coefficients
  
  row$coeff <- coeffs[,1]
  row$p <- coeffs[,4]
  row$var <- column
  partial_fits <- rbind(partial_fits, as_tibble(row))
}
p05 <- filter(partial_fits, p < 0.05)


coeffs[coeffs[,4]< 0.05,]