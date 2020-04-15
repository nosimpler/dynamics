# distances v0.2
library(TSdist)
library(coin)

# constructs time-series matrix for finding distancees
# hypnograms
build_ts_matrix <- function(h, var='STAGE_N'){
  seqmat <- matrix(nrow=300, ncol=0)
  for (cond in c('baseline', 'followup')){
      for (id in unique(h$baseline$ID)) {
        seq <- h[[cond]] %>%
          filter(ID==id) %>%
          arrange(E) %>%
          filter(row_number() >= first(which(STAGE_N < 1))) %>%
          select(sym(!!var))
        colnames(seq) <- paste(cond, id, sep = '_')
    seqmat <- cbind(seqmat, seq[1:300,])
      }
  }
  t(as.matrix(seqmat))
}

# for spectral data
build_ts_matrix_spec <- function(h, b, ch, var='STAGE_N'){
  seqmat <- matrix(nrow=300, ncol=0)
  for (cond in c('baseline', 'followup')){
    for (id in unique(h$baseline$ID)) {
      seq <- h[[cond]] %>%
        filter(ID==id, B==sym(!!b), CH==sym(!!ch)) %>%
        arrange(E) %>%
        filter(row_number() >= first(which(STAGE_N < 1))) %>%
        select(sym(!!var))
      colnames(seq) <- paste(cond, id, sep = '_')
      seqmat <- cbind(seqmat, seq[1:300,])
    }
  }
  t(as.matrix(seqmat))
}

# requires baseline and followup
# assess whether A -> A is different from A -> B  
traitlikeness <- function(dist, plot=TRUE){
   mat <- as.matrix(dist)
   n <- dim(mat)[1]
   transfer_matrix <- mat[1:(n/2),(n/2+1):n]
   ondiag <- tibble(d=as.vector(diag(transfer_matrix)))
   offdiag <- tibble(d=as.vector(lava::offdiag(transfer_matrix)))
   ondiag$diag <- 1
   offdiag$diag <- 0
   distances <- rbind(ondiag, offdiag)
   if(plot==TRUE){
    p <- ggplot(distances, aes(x=d, y=..density.., fill=as.factor(diag)))+
      geom_hist()+
      scale_color_brewer(palette='Set1')
    print(p)
   }
   statsdiag <- independence_test(diag~d, data=distances, alternative='less')
}

baseline_submatrix <- function(dist_mat) {
  n <- dim(dist_mat)[1]
  print(n)
  dist_mat[1:(n/2),1:(n/2)]
}

followup_submatrix <- function(dist_mat) {
  n <- dim(dist_mat)[1]
  dist_mat[(n/2+1):n,(n/2+1):n]
}

get_distances <- function(df,ch, band, measure, ids){
  tsm <- build_ts_matrix_spec(df, band, ch, var='PSD')
  tsmd <- xdx_all(tsm)
  if (measure=='pdc'){
    dist_mat <- as.matrix(TSDatabaseDistances(t(tsmd), 'pdc'))
  }
  else if (measure=='erp'){
    dist_mat <- as.matrix(TSDatabaseDistances(t(tsmd), 'erp', g=0))
  }
  else if (measure=='ncd'){
    dist_mat <- as.matrix(TSDatabaseDistances(t(tsmd), 'ncd'))
  }
  #dist_mat$lbk <- as.matrix(TSDatabaseDistances(t(tsmd),'lb.keogh', window.size=61))
  #dist_mat$sax <- as.matrix(TSDatabaseDistances(t(tmsd), 'mindist.sax'), window.size=30)
  #dist_mat$tam <- as.matrix(TSDatabaseDistances(t(tmsd), 'tam'))
}



