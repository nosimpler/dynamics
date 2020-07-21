# ODE fitting
library(tidyverse)
library(sindyr)
library(tvR)
library(splus2R)
library(patchwork)
dnx <- function(x) {denoise1(log10(x/max(x)), 1e30)}




figdir <- '/Users/rl422/dyn/figs/'

# Cross-comparison (takes lots of time)
#####
# r -- regularized
# dr -- differentiated regularized
L=1000

sindify <- function(Hall) {
Hall %>% 
  pivot_wider(names_from='component') %>%
  group_by(ID) %>%
  arrange(ID,E) %>%
    select(-V1,-V6) %>%
  mutate(across(starts_with('V'), ~tv(.x,100000), .names="r{col}")) %>%
  mutate(across(starts_with('r'), diffx, .names='d{col}')) %>%
  mutate(peak=splus2R::peaks(rV3, span=60)) %>%
  drop_na() %>%
  filter(row_number()<= first(which(peak==TRUE))) %>%
    ungroup()
}

H_sindy <- sindify(Hall6)
IDs <- unique(Hall6$ID)
B <- list()
E <- list()
for (id in sample(IDs)){
  print(id)
H_pl <- pivot_longer(H_sindy, contains('V'), names_to = 'V') %>% 
  filter(ID==id)
W_pl <- filter(Wall6, ID==id)
r_pl <- filter(H_pl, str_locate(V, 'r')==1)
d_pl <- filter(H_pl, str_locate(V, 'd')==1)
# s -- signal
s_pl <- filter(H_pl, str_locate(V, 'V')==1)



    mat_sindy <- filter(H_sindy, ID==id) %>%
      select(starts_with('r')) %>%
      as.matrix()
    pts <- as.matrix(select(filter(H_sindy, ID==id), starts_with('d')))
    mat_sindy <- apply(mat_sindy, 2, normalize)

    feat <- features(mat_sindy, polyorder=1)
    sindy_fit <- sindy(mat_sindy, 
                       lambda=0.01,
                       dx=apply(mat_sindy, 2, diffx),
                       verbose=TRUE,
                       Theta=feat,
                       fit.its=5000)
    print(sindy_fit$B)
    B[[id]] <- sindy_fit$B
    reconst <- as.matrix(sindy_fit$Theta) %*% as.matrix(sindy_fit$B)
    reconst <- as.tibble(reconst)

    reconst <- pivot_longer(reconst, everything(), names_to = 'V')
    d_pl$reconst <- reconst$value
    ps <- ggplot(s_pl, aes(x = as.numeric(row.names(s_pl)), 
                           y=value, 
                           color=V))+
      geom_line()+
      geom_line(data=r_pl, aes(x=as.numeric(row.names(r_pl)),
                                  y=value,
                                  group=V), color='grey')
    pr <- ggplot(r_pl, aes(x = as.numeric(row.names(r_pl)), 
                           y=value, 
                           color=V))+
      geom_line()
    pd <- ggplot(d_pl, aes(x = as.numeric(row.names(d_pl)), 
                           y=value, 
                           color=V))+
      geom_line()
    pre <- ggplot(d_pl, aes(x=as.numeric(row.names(d_pl)), 
                               y=reconst, 
                               color=V))+
      geom_line()
    pw <- ggplot(W_pl, aes(x=FR, y=value, color=component))+
      geom_line()
    gc()
    #plot(pw)
    plot((ps+pd/pre))
    ggsave(paste(figdir, id, '_dynamics.pdf',sep=''))
    E[[id]] <- eig(as.matrix(sindy_fit$B[2:5,]))
    plot(E[[id]])
    
}
