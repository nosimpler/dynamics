# mouse stuff
library(tidyverse)
library(ggforce)
library(scico)
load_data <- function(){
  df <- read_table2('~/dyn/data/mouse/PSD_E_F_CH.txt', guess_max = 1000000)
  #separate(ID, into = c("DATASET", "COND", "ID"), sep='-')
}

load_hypno <- function(){
  df <- read_table2('~/dyn/data/mouse/STAGE_E.txt', guess_max= 1000000)
}

split_id <- function(data) {
  separate(data, ID, into=c('nsrrid', 'session'), sep='_')
}


plot_spectrograms_all <- function(data) {
  data <- data %>% 
    split_id_mouse()
  pdf("~/Desktop/mouse.pdf", 10, 10)
  for (i in unique(data$mouseid)) {
    p <- ggplot(data %>% filter(mouseid == i), aes(x=E, y=F, fill=log(PSD)))+geom_tile()+
      scale_fill_scico(palette='vik') + 
      facet_grid(CH~cycle)+
      ggtitle(i)
    print(p)
  }
  dev.off()
}

plot_H_all <- function(data){
  
}