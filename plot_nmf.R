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
    group_by(E, FR) %>% 
    summarize(valueWH=sum(valueWH)) %>%
    #mutate(valueWH=normalize(valueWH)) %>%
    mutate(FR=replace(FR, FR ==0, -0.5))YC3 <- Y %>% filter(CH=='C3')

    
  
  ggplot(WH, aes(x=E, y=FR, fill=valueWH))+
    geom_tile()+
    #geom_line(aes(y=(STAGE_N+3)*10), alpha=0.3)+
    scico::scale_fill_scico(palette='vik') +
    theme_void()
}

plot_spectrogram <- function(Y){
  
  p1 <- ggplot(Y, aes(x=E, y=FR, fill=log(PSD)))+
    geom_tile()+
    scico::scale_fill_scico(palette='vik')+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"))
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

plot_range <- function(wdemo, data_segment, var, comp) {
  wdemo <- wdemo %>% arrange(!!sym(var))
  IDlist <- wdemo$nsrrid[seq(1, length(wdemo$nsrrid), 20)]
  ts <- filter(data_segment %>% 
                 split_id() %>% 
                 filter(session=='baseline'), 
               nsrrid %in% IDlist) %>%
    mutate(nsrrid = factor(nsrrid, levels = IDlist)) %>%
    group_by(nsrrid, component) %>%
    mutate(E = E - min(E)) %>%
    filter(component==comp)
  
  ggplot(ts, aes(x=E, y=value,color=nsrrid))+
    geom_point()+
    facet_wrap(~nsrrid, nrow=1)+
    theme_void()
}

# plot spectrogram, factors
plot_factorization <- function(Y, H, W){
  
  Y <- Y %>%
    mutate(FR=replace(FR, FR ==0, -0.5))
  
  p1 <- ggplot(H, aes(x=E, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')+
    theme_void()
  p2 <- ggplot(W, aes(x=FR, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')+coord_flip()+
    theme_void()
  p3 <- plot_spectrogram(Y)+theme_void()
  p4 <- plot_factor_reconstruct(W,H)
  (p3 | p2) / (p1 | p4) + 
    plot_layout(guides='collect')
}



order_nmf_factors <- function(H,W){
  
}

#id <- sample(unique(Wall6$ID),1)
#H <- filter(Hall6, ID==id)
#W <- filter(Wall6, ID==id)
#plot_spectrogram(results$orig)
#plot_factor_reconstruct(results$W,results$H)
#plot_factor_spectrogram(results$W,results$H)