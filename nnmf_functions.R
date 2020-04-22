# nnmf scripts
# takes time series matrix
library(ggplot2)
library(GGally)
library(patchwork)
nmfx <- function(x){
  out <- list()
  nmf_fit <- nmf(x, 2, method='brunet', seed='ica')
  out$W <- nmf_fit@fit@W
  out$H <- nmf_fit@fit@H
  out
}


#tsm <- rx_all(build_ts_matrix_spec(ball, 'SLOW', 'O1', 'RELPSD'))
nm1 <- nmfx(-tsm[,1:379])
nm2 <- nmfx(-tsm[,380:758])
h1 <- nm1$H
h2 <- nm2$H
w1 <- nm1$W
w2 <- nm2$W

ggpairs(as_tibble(t(h1)), lower = list(continuous = "density"),title='baseline')
ggpairs(as_tibble(t(h2)), lower = list(continuous = "density"),title='followup')
p1 <- qplot(1:300, w1[,1], geom='line')
p2 <- qplot(1:300, w2[,1], geom='line')
p3 <- qplot(1:300, w1[,2], geom='line')
p4 <- qplot(1:300, w2[,2], geom='line')
(p1+p3)/(p2+p4)
correlate(w1, w2)
correlate(t(h1), t(h2))
p5 <- qplot(h1[1,],h2[1,])
p6 <- qplot(h1[2,],h2[2,])
p5/p6


nn <- inner_join(
  rownames_to_column(as_tibble(t(h1)), var='nsrrid'), 
  rownames_to_column(as_tibble(t(h2)), var='nsrrid'), 
  by='nsrrid')


um1 <- umap(-tsm[,1:379], metric='manhattan')
um2 <- umap(-tsm[,380:758], metric='manhattan')
um <- inner_join(
  rownames_to_column(as_tibble(um1$layout), var='nsrrid'), 
  rownames_to_column(as_tibble(um2$layout), var='nsrrid'), 
  by='nsrrid')

