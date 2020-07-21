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

id <- sample(unique(Wall6$ID),1)
H <- filter(Hall6, ID==id)
W <- filter(Wall6, ID==id)
plot_factor_reconstruct(W,H)
plot_factor_spectrogram(W,H)