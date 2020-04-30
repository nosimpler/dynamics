# nnmf scripts
# takes time series matrix
library(ggplot2)
library(GGally)
library(patchwork)
nmfx <- function(x){
  out <- list()
  nmf_fit <- nmf(x, 2, method='brunet', seed='ica', nrun=10)
  out$W <- nmf_fit@fit@W
  out$H <- nmf_fit@fit@H
  out
}

#slo1 <- build_ts_matrix_spec(ball, 'SLOW', 'O1', 'RELPSD')
tsm <- rx_all(slo1)
nm1 <- nmfx(tsm[,1:379])
nm2 <- nmfx(tsm[,380:758])
l <- 300
h1 <- nm1$H
h2 <- nm2$H
w1 <- nm1$W
w2 <- nm2$W

ggpairs(as_tibble(t(h1)), lower = list(continuous = "density"),title='baseline')
ggpairs(as_tibble(t(h2)), lower = list(continuous = "density"),title='followup')
p1 <- qplot(1:l, w1[,1], geom='line')
p2 <- qplot(1:l, w2[,1], geom='line')
p3 <- qplot(1:l, w1[,2], geom='line')
p4 <- qplot(1:l, w2[,2], geom='line')
(p1+p3)/(p2+p4)
correlate(w1, w2)
correlate(t(h1), t(h2))
p5 <- qplot(h1[1,],h2[1,])
p6 <- qplot(h1[2,],h2[2,])
p5/p6


nnH <- inner_join(
  rownames_to_column(as_tibble(t(h1)), var='nsrrid'), 
  rownames_to_column(as_tibble(t(h2)), var='nsrrid'), 
  by='nsrrid')
nnW <- inner_join(rownames_to_column(as_tibble(w1), var='E'), 
                 rownames_to_column(as_tibble(w2), var='E'),
                 by='E') %>% mutate(E=as.numeric(E))

meas_bsl$nnH <- h1[2,]
meas_flp$nnH <- h2[2,]
summary(lm(nnH~., data=select(meas_bsl, -nsrrid,-TWT,-MINS_REM)))
summary(lm(nnH~., data=select(meas_flp, -nsrrid,-TWT,-MINS_REM)))

um1 <- umap(-tsm[,1:379], metric='manhattan')
um2 <- umap(-tsm[,380:758], metric='manhattan')
um <- inner_join(
  rownames_to_column(as_tibble(um1$layout), var='nsrrid'), 
  rownames_to_column(as_tibble(um2$layout), var='nsrrid'), 
  by='nsrrid')

