#pdc stuff
##### 
# PDC FIRST 360 EPOCHS
#####

IDlist <- flatten(tibble(ID=intersect(unique(m2$ID), unique(m1$ID))))

# select band/channel
chb <- 'DELTA_F4'

# m time-delays, t steps back
m=4
t=1

# use n epochs
n_e <- 100


dc3_1 <- m1c %>% recast() %>%
  select(ID, E, !!sym(chb)) %>% 
  filter(ID %in% IDlist) %>%
  group_by(ID) %>% top_n(-n_e,E) %>% 
  arrange(ID,E) %>% 
  select(ID, !!sym(chb)) 

dc3_2 <- m2c %>% recast() %>%
  select(ID, E, !!sym(chb)) %>% 
  filter(ID %in% IDlist) %>%
  group_by(ID) %>% top_n(-n_e,E) %>% 
  arrange(ID,E) %>% 
  select(ID, !!sym(chb))

dc3_1$E_SLEEP <- rep(seq(1:n_e), times=405)
dc3_2$E_SLEEP <- rep(seq(1:n_e), times=405)

dc3w_1 <- pivot_wider(dc3_1, names_from=ID, values_from=!!sym(chb))
dc3w_2 <- pivot_wider(dc3_2, names_from=ID, values_from=!!sym(chb))

um01 <- umap(t(dc3w_1[,2:406]))
um02 <- umap(t(dc3w_2[,2:406]))

plot(um01$layout[,1], um01$layout[,2])
plot(um02$layout[,1], um02$layout[,2])

pdc_dist1 <- pdcDist(dc3w_1[,2:406],m=m, t=t)
pdc_dist2 <- pdcDist(dc3w_2[,2:406],m=m, t=t)


p_matrix1 <- matrix(0,405, 405)
p_matrix1[lower.tri(p_matrix1, diag=FALSE)] <- pdc_dist1

p_matrix2 <- matrix(0,405, 405)
p_matrix2[lower.tri(p_matrix2, diag=FALSE)] <- pdc_dist2

um1 <- umap(p_matrix1)
um2 <- umap(p_matrix2)

plot(um1$layout[,1], um1$layout[,2])
plot(um2$layout[,1], um2$layout[,2])

nights_umap <- tibble(N1C1=um1$layout[,1],
                      N1C2=um1$layout[,2],
                      N2C1=um2$layout[,1],
                      N2C2=um2$layout[,2])
ggplot(nights_umap,aes(x=N1C1, y=N1C2, color=N2C2))+
  geom_point()+
  scale_color_distiller(palette='Spectral')

