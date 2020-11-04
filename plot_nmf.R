# plot factors and reconstructed spectrogram
# highlight factors
library(tidyverse)
library(patchwork)
source('~/dyn/src/dynamics/cleandata.R')
plot_factor_spectrogram <- function(W,H){
  WH <- left_join(W,H, by=c('component', 'ID'), suffix=c('W','H')) %>%
    mutate(valueWH = valueW*valueH) %>%
    group_by(E) %>% mutate(valueWH=normalize(valueWH)) %>%
    mutate(FR=replace(FR, FR ==0, -0.5))
  #p_fr <- ggplot(W)
  #p_time <- ggplot(H)
  #p_hypno <- ggplot(H)
  #p_spectrogram <- 
  ggplot(WH, aes(x=E, y=FR, fill=valueWH))+
    geom_tile()+
    geom_line(aes(y=(STAGE_N+3)*10), alpha=0.3)+
    facet_wrap(~component)+
    scale_fill_distiller(palette='Spectral')
}

plot_factor_reconstruct <- function(W,H){
  WH <- left_join(W,H, by=c('component', 'ID'), suffix=c('W','H')) %>%
    mutate(valueWH = valueW*valueH) %>%
    group_by(component) %>% mutate(valueWH=normalize(valueWH)) %>%
    mutate(FR=replace(FR, FR ==0, -0.5)) %>% ungroup() %>%
    group_by(E) %>% summarize(tot = sum(valueWH))
  #p_fr <- ggplot(W)
  #p_time <- ggplot(H)
  #p_hypno <- ggplot(H)
  #p_spectrogram <- 
  p1 <- ggplot(WH, aes(x=E, y=FR, fill=tot))+
    geom_tile()+
    #geom_line(aes(y=(STAGE_N+3)*10), alpha=0.3)+
    scale_fill_distiller(palette='Spectral')
  p2 <- ggplot(H, aes(x=E, y=STAGE_N))+
    geom_line()
  p3 <- ggplot(H, aes(x=E, y=value, color=component))+geom_point()+
    scale_color_brewer(palette='Set1')
  p3/p1
}

plot_spectrogram <- function(orig){
  
  p1 <- ggplot(orig, aes(x=E, y=FR, fill=log(value)))+
    geom_tile()+
    scico::scale_fill_scico(palette='vik')+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"))
  print(p1)
}

plot_factors <- function(results){
  p1 <- ggplot(results$H, aes(x=E, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')
  p2 <- ggplot(results$W, aes(x=FR, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')
  p2/p1
}

plot_hilo <- function(H, wdemo, V='V1'){
  maxID <- wdemo %>% filter(!!sym(V)==max(!!sym(V)))
  minID <- wdemo %>% filter(!!sym(V)==min(!!sym(V)))
  H1 <- filter(H, ID==maxID)
  H0 <- filter(H, ID==minID)
  p1 <- ggplot(H1, aes(x=E, y=value, color=component))+geom_point()
  p0 <- ggplot(H0, aes(x=E, y=value, color=component))+geom_point()
  p1/p0
}

plot_range <- function(wdemo, prerem, var, comp) {
  wdemo <- wdemo %>% arrange(!!sym(var))
  IDlist <- wdemo$nsrrid[seq(1, length(wdemo$nsrrid), 50)]
  ts <- filter(prerem %>% split_id(), nsrrid %in% IDlist) %>%
    mutate(nsrrid = factor(nsrrid, levels = IDlist)) %>%
    group_by(nsrrid, component) %>%
    mutate(E = E - min(E)) %>%
    filter(component==comp)
  
  ggplot(ts, aes(x=E, y=value,color=nsrrid))+
    geom_point()+
    facet_wrap(~nsrrid, nrow=1)+
    theme_void()
}

#id <- sample(unique(Wall6$ID),1)
#H <- filter(Hall6, ID==id)
#W <- filter(Wall6, ID==id)
#plot_spectrogram(results$orig)
#plot_factor_reconstruct(results$W,results$H)
#plot_factor_spectrogram(results$W,results$H)