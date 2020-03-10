library(tidyverse)
library(patchwork)
library(tvR)

plot_defaults <- function(p) {
p + scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1')+
    theme_minimal()
  }

hypno_plot <- function(data, var){
  ggplot(data=data, aes(x=E, y=!!sym(var), color=STAGE))+geom_point(size=1)
}

hypno_plot_outliers <- function(data, var){
  ggplot(data=data, aes(x=E, y=!!sym(var), color=FLAGGED))+geom_point()
}

hypno_plot_tv <- function(data, var, lambda){
  #data <- range_normalize(data, var)
  hypno_plot(data, var)+geom_point(y=denoise1(data[[var]], lambda=lambda), color='black', size=0.2)
}

compare_CHB <- function(m1,m2,ID='300001',CH='O1',B="SIGMA",cycle=1){
  f1 <- m1 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE==cycle) %>%
    arrange(E) %>% remove_outliers()
  p1 <- ggplot(f1, aes(x=E,y=dnx(!!sym(paste(B,CH,sep='_')))))+
    geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=0.1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1')+
    theme_minimal()+scale_y_log10()+ggtitle(ID)
    
  f2 <- m2 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE==cycle) %>%
    arrange(E) %>% remove_outliers()
  p2 <- ggplot(f2, aes(x=E,y=dnx(!!sym(paste(B,CH,sep='_')))))+
    geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=0.1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1')+
    theme_minimal()+scale_y_log10()
  
  p1/p2
}

ggplot(m1cycle_length, aes(x=cycle_length))+geom_histogram()
ggplot(m2cycle_length, aes(x=cycle_length))+geom_histogram()
ggplot(diffcycle_length, aes(x=abs(difflength)))+
  geom_histogram(bins=40)+
  geom_freqpoly()



