# MDS/PCA Regression on distance matrices

`%notin%` <- negate(`%in%`)


#exclude <- c(300058,300368,300668) # for some reason PDC and EDR distances go to NA here


dist_matrix <- function(dist_tab, D){ 
  dist_tab %>% 
    filter(MEASURE == sym(!!D)) %>%
    select('ID1', 'ID2', 'value') %>%
    pivot_wider(names_from='ID1', values_from=value) %>%
    select(2:length(.)) %>%
    as.matrix()
}
dist_mat <- dist_matrix(dist_diag_l, 'pdc')
hb <- hypno_ds$baseline_stats
hf <- hypno_ds$followup_stats
htb <- tca$baseline
htf <- tca$followup
htbn <- htb %>% select(ID, starts_with('n'))
htfn <- htf %>% select(ID, starts_with('n'))

htbp <- htb %>% select(ID, starts_with('p'))
htfp <- htf %>% select(ID, starts_with('p'))

not_all_na <- function(x) any(!is.na(x))
demo <- hypno_ds$demo %>% select_if(not_all_na)
pc <- prcomp(dist_mat)
fit <- cmdscale(dist_mat, k=3)

rvars <- left_join(htbn, htfn, by="ID", suffix=c(".baseline", ".followup")) %>%
                    mutate(
                      #pc1=log(pc$x[,1]),
                      mds1=fit[,1],
                           #mds2=fit[,2],
                 #          mds3=fit[,3]
                           ) %>%
  mutate(male=demo$male)

summary(lm(mds1~., data=select(rvars,-ID)),
        signif.stars=TRUE)