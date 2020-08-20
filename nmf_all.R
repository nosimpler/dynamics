
library(NMF)
library(tidyverse)
# select ID


figdir <- '/Users/rl422/dyn/figs/'

#####
# use Wgroup to seed
n_components <- 6 

Hall <- tibble(component=as.character(),
                      value=as.numeric(),
                      ID=as.character(),
                      E=as.numeric(),
                      STAGE_N=as.numeric())
Wall <- tibble(component=as.character(),
                      value=as.numeric(),
                      ID=as.character(),
                      FR=as.numeric())
IDlist <- distinct(select(data, ID)) %>% collect()
err <- list()
# do nmf for each individual
for (id in IDlist$ID) {
  print(id)
  idmat <- datahyp %>%
    filter(CH == 'C3',
           ID == id,
          # STAGE=='NREM2',
           #CYCLE >= 1
          ) %>%
    select(E, F, PSD, STAGE_N) %>%
    arrange(E, F) %>% collect()
  epochs <- unique(idmat$E)
  fr <- unique(idmat$F)
  stage_n <- idmat %>% filter(F==min(F)) %>% select(STAGE_N)
  idmat <- idmat %>% 
    select(E,F,PSD) %>%
    pivot_wider(names_from = E, values_from = PSD)
   
  # remove F column
  # nmf initial conditions
  nH <- ncol(idmat)-1
  nW <- nrow(idmat)
  #w0 <- cbind(rep(1.0, nW), rep(1.0, nW), rep(1.0, nW))
  #w0[1, 1] <- 10
  #w0[12, 2] <- 10
  #w0[25, 3] <- 10
  
  h0 <- rep(1.0,nH)
  w0 <- rep(1.0,nW)
  for (i in seq(n_components-1)){
    h0 <- rbind(h0, rep(1.0, nH))
    w0 <- cbind(w0, rep(1.0,nW))
    w0[i,i] <- 2
  }
  colnames(h0) <- NULL
  colnames(w0) <- NULL
  #w0 <- Wgroup
  sansf <- idmat[, -1]
  nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
  init <- nmfModel(n_components, nmat, W = w0, H = h0)
  nmffit <- nmf(log(nmat / min(nmat)), n_components, method = 'lee', seed = init)
  err[[id]] <- nmffit@residuals
  resultsH <- as.tibble(t(nmffit@fit@H))
  resultsW <- as.tibble(nmffit@fit@W)
  resultsH$E <- epochs
  resultsW$FR <- fr
  resultsW$ID <- id
  resultsH$ID <- id
  resultsH$STAGE_N <- stage_n$STAGE_N
  
  #rename hack
  resultsW <- resultsW %>% 
    rename(V1='h0') %>% 
    pivot_longer(starts_with('V'), 
                          names_to='component', 
                          values_to = 'value')
  
  Wall <- add_row(Wall, resultsW)
  
  resultsH <- resultsH %>% 
    rename(V1='h0') %>% 
    pivot_longer(starts_with('V'), 
                                        names_to='component', 
                                        values_to = 'value')
  Hall <- add_row(Hall, resultsH)
  graphics.off()
  p1 <- ggplot(resultsW, aes(x=FR,y=value,color=component))+
    geom_line()
 
  p2 <- ggplot(resultsH, aes(x=E,y=value,color=component))+
    geom_line()
  plot(p1/p2)
  ggsave(paste(figdir, id, '_withwake.pdf',sep=''))
}
ggplot(Hall, aes(x=E,
                        y=value,
                        color=component, 
                        shape=as.factor(STAGE_N)))+
  geom_point()+
  facet_wrap(~ID,nrow=3)


######
# now do nmf on FR x (ID x V) for class membership
Y_pop <-Wall %>%
  pivot_wider(names_from=c(ID,component), values_from=value)

nW <- 46
nH <- 816*n_components


h0 <- rep(1.0,nH)
w0 <- rep(1.0,nW)
#w0 <- Wgroup
for (i in seq(n_components-1)){
  h0 <- rbind(h0, rep(1.0, nH))
  w0 <- cbind(w0, rep(1.0,nW))
  w0[i,i] <- 2
}


sansf <- select(Y_pop, -FR)
Y <-matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
init <- nmfModel(n_components,Y, W = w0, H = h0)
#nmffit <- nmf(log(nmat/min(nmat)), 3, method='lee', seed='nndsvd')
nmffit <- nmf(Y, n_components, method = 'lee', seed = init)
Hgroup <- nmffit@fit@H
Wgroup <- nmffit@fit@W
colnames(Hgroup) <- colnames(sansf)

# convert to more readable format
Hpop <- as_tibble(t(Hgroup)) %>% rename(V1 = w0)
Hpop$IDvec <- rownames(t(Hgroup))
Hpop <- separate(Hpop, IDvec, 
                 into=c('study','condition','ID','component'))

# Hpop_regress <- Hpop %>% 
#   filter(component=='V2', condition=='followup') %>%
#   select(ID, 'V5') %>%
#   transmute(nsrrid=as.numeric(ID), V=log(V5)) %>%
#   left_join(demo) %>% filter(V > -0.5)
# regress_all(Hpop_regress, 'V')
# ggplot(Hpop_regress, aes(y=V, x=ageyear_at_meas))+
#   stat_summary()+
#   ggbeeswarm::geom_beeswarm(size=0.5)+
#   scale_color_brewer(palette='Set1')
plot(Wgroup[,1],type='l', col='red')
lines(Wgroup[,2], col='green')
lines(Wgroup[,3], col='blue')
lines(Wgroup[,4], col='violet')
lines(Wgroup[,5], col='brown')
lines(Wgroup[,6], col='pink')
lines(Wgroup[,7], col='orange')
lines(Wgroup[,8], col='cyan')
pairs(t(Hgroup))
