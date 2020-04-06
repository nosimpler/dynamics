library(tidyverse)
library(patchwork)
library(tvR)

theme_rob <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#colors <- scale_color_brewer(palette='Set1') +
#    scale_fill_brewer(palette='Set1') 


##### 
#SINGLE TIME SERIES
# Basic hypnogram
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

# ONE CHANNEL, ONE BAND, ONE INDIVIDUAL, TWO NIGHTS
###### 
# X/Xmax
compare_CHB <- function(m1,m2,ID='300001',CH='O1',B="SIGMA",cycle=c(1,2,3,4,5), value=NULL){
  f1 <- m1 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE %in% cycle) %>%
    arrange(E) %>% remove_outliers()

    
  f2 <- m2 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE %in% cycle) %>%
    arrange(E) %>% remove_outliers()
  
  ranges <- c(min(f1$E), max(f1$E), min(f2$E), max(f2$E))
  print(ranges)
  
  p1 <- ggplot(f1, aes(x=E,y=dnx(!!sym(paste(B,CH,sep='_')))))+
    geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1', drop=FALSE)+
    theme_minimal()+scale_y_log10()+
    ggtitle(ID, subtitle=paste("Divergence",value, sep=''))+
    xlim(min(ranges), max(ranges))
  
  p2 <- ggplot(f2, aes(x=E,y=dnx(!!sym(paste(B,CH,sep='_')))))+
    geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1', drop=FALSE)+
    theme_minimal()+scale_y_log10()+
    xlim(min(ranges),max(ranges))
  
  p1/p2
}

# Xdot/X 
compare_CHB_diffscale <- function(m1,m2,ID='300001',CH='O1',B="SIGMA",cycle=1, value=NULL){
  f1 <- m1 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE==cycle) %>%
    arrange(E) %>% remove_outliers()


  
  f2 <- m2 %>% 
    filter(ID==sym(!!ID),
           CH==sym(!!CH),
           B==sym(!!B),
           CYCLE==cycle) %>%
    arrange(E) %>% remove_outliers()
  ranges <- c(min(f1$E), max(f1$E), min(f2$E), max(f2$E))
  
  p1 <- ggplot(f1, aes(x=E,y=c(NA,dnx(diff(!!sym(paste(B,CH,sep='_')))))))+
    #geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1', drop=FALSE)+
    theme_minimal()+
    ggtitle(ID, subtitle=paste("Divergence",value, sep=' '))+
    geom_hline(yintercept=0)+xlim(min(ranges), max(ranges))
  
  p2 <- ggplot(f2, aes(x=E,y=c(NA,dnx(diff(!!sym(paste(B,CH,sep='_')))))))+
    #geom_point(aes(y=!!sym(paste(B,CH,sep='_')), color=STAGE), size=1)+
    geom_path()+ scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1', drop=FALSE)+
    theme_minimal()+geom_hline(yintercept=0)+xlim(min(ranges), max(ranges))
  
  p1/p2
}



# ONE CHANNEL, ONE BAND, ALL INDIVIDUALS, TWO NIGHTS
#####
night_night <- function(data){
  data <- arrange(data, ID1, ID2)
  diagonal <- (data$ID1 == data$ID2)
  data <- mutate(data, DIAGONAL=diagonal)
  p0 <- ggplot(data, aes(x=ID1,y=ID2,fill=D))+
    geom_tile()+theme_void()
  
  p2 <- ggplot(data, aes(x=ID1, y=D, color=DIAGONAL, alpha=DIAGONAL))+
    geom_point()+theme_void()
  p1 <- ggplot(data, aes(x=ID2, y=D, color=DIAGONAL, alpha=DIAGONAL))+
    geom_point()+theme_void()+coord_flip()
  p3 <- ggplot(data, aes(x=D, color=DIAGONAL))+geom_histogram(stat='density')
  p0 + p1 + p2 + p3 + 
    plot_layout(widths = c(1, 5), heights = unit(c(1, 5), c('cm', 'null')), guides='collect')
}

# HYPNOGRAM, TWO INDIVIDUALS, TWO NIGHTS
hypno_compare <- function(h, id1, id2, title='', remove_first_wake=TRUE, epoch_range=NULL){
  h1 <- h$baseline
  h2 <- h$followup
  h1 <- filter(h1, ID==id1) 
  h2 <- filter(h2, ID==id2)
  if (remove_first_wake){
    h1 <- h1 %>% arrange(E) %>%
      select(E, STAGE_N) %>% 
      filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
    h2 <- h2 %>% arrange(E) %>%
      select(E, STAGE_N) %>% 
      filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
  }
  if (!is.null(epoch_range)){
      p1 <- ggplot(h1[epoch_range,], aes(x=E, y=STAGE_N))+geom_line()
      p2 <- ggplot(h2[epoch_range,], aes(x=E, y=STAGE_N))+geom_line()
  } else {
    p1 <- ggplot(h1, aes(x=E, y=STAGE_N))+geom_line()
    p2 <- ggplot(h2, aes(x=E, y=STAGE_N))+geom_line()
  }
  (p1 + ggtitle(title))/p2 
}

# Set of hypnogram pairs from best and/or worst match
plot_matches <- function(match_table, h){
  match_table_plots <- match_table %>% 
    mutate(plot=list(hypno_compare(h, ID1, ID2, epoch_range=1:300, title=MEASURE)))
  match_table_plots$plot
}


