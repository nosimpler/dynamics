# DTW
library(dtw)
library(tidyverse)
library(tvR)
library(NMF)
source('~/Desktop/dynamics/outliers.R')
source('~/Desktop/dynamics/chat.R')
library(MASS)

dnx <- function(x){denoise1(x,1e30)}
dtw_table <- function(m1, m2){
  
  dist_tab <- NULL

  # builds dynamic-time-warping distance table
  for (id in flatten(IDlist)){
    print(id)
    rows <- list()
    for (i in 3:98){
      test1 <- m1 %>% filter(ID==id, CYCLE==1) %>% arrange(E) #%>% remove_outliers()
      test2 <- m2 %>% filter(ID==id, CYCLE==1) %>% arrange(E) #%>% remove_outliers()
      d1 <- diff(dnx(test1[, i]))
      d2 <- diff(dnx(test2[, i]))
      rows[[colnames(test1)[i]]] <- dtw(d1/max(d1), d2/max(d2),  
                                      step.pattern=rabinerJuangStepPattern(1, "c", TRUE), 
                                      open.begin=FALSE, open.end=TRUE)$normalizedDistance
    }
  t <- as_tibble(rows)
  t$ID <- id
  
  dist_tab <- bind_rows(dist_tab,t)
}
}

#for (i in 1:96){
  ggplot(dist_tab, aes(x=log(ALPHA_O1)))+geom_histogram()
#}
  medians <- pivot_dist(dist_tab) %>% 
    group_by(CH,B) %>% 
    summarize(med=median(value))
distp <- pivot_dist(dist_tab)

# Plot violins (note bimodality in beta)
ggplot(distp,aes(x=CH,y=value)) + 
  geom_violin() + 
  facet_wrap(~B) +
  scale_y_log10()

# plot beeswarm for each channel in beta band
ggplot(filter(distp,B=="BETA"),aes(x=0,y=value)) + 
  geom_beeswarm() + 
  facet_wrap(~CH)+
  scale_y_log10()

# plot beta distance F3
ggplot(filter(distp,B=="BETA", CH=='F3'), aes(x=value))+
  geom_histogram(bins=40)+
  scale_x_log10()

# plot beta distance T3
ggplot(filter(distp,B=="BETA", CH=='T3'), aes(x=value))+
  geom_histogram(bins=100) + xlim(0,10)

# plot beta distance all channels
ggplot(filter(distp,B=="BETA"), aes(x=value))+
  geom_histogram(bins=100) + xlim(0,10)

# plot beeswarm for each channel in theta band
ggplot(filter(distp,B=="THETA"),aes(x=0,y=value)) + 
  geom_beeswarm() + 
  facet_wrap(~CH)+
  scale_y_log10()

# plot theta distance O2
ggplot(filter(distp,B=="BETA", CH=='O2'), aes(x=value))+
  geom_histogram(bins=40)+
  scale_x_log10()

# plot theta distance T4
ggplot(filter(distp,B=="BETA", CH=='T4'), aes(x=value))+
  geom_histogram(bins=100) + xlim(0,10)

# plot theta distance M2
ggplot(filter(distp,B=="BETA", CH=='M2'), aes(x=value))+
  geom_histogram(bins=100) + xlim(0,10)

# plot cycle length for first recording
m1cycle_length <- m1 %>% select(ID,CYCLE) %>% 
  filter(CYCLE==1,ID %in% flatten(IDlist)) %>%
  group_by(ID) %>%
  summarize(cycle_length=n())
ggplot(m1cycle_length, aes(x=cycle_length))+geom_histogram()

# plot cycle length for second recording
m2cycle_length <- m2 %>% select(ID,CYCLE) %>% 
  filter(CYCLE==1,ID %in% flatten(IDlist)) %>%
  group_by(ID) %>%
  summarize(cycle_length=n())
ggplot(m2cycle_length, aes(x=cycle_length))+geom_histogram()

# plot difference in cycle length
diffcycle_length <- m2cycle_length %>% 
  transmute(ID=ID, 
            difflength=m1cycle_length$cycle_length-m2cycle_length$cycle_length)
ggplot(diffcycle_length, aes(x=difflength))+geom_histogram(bins=100)

# find individuals whose cycle lengths were similar
similar_lengthID <- filter(diffcycle_length, abs(difflength)<50)$ID

# plot beeswarm for cycles with similar length
ggplot(filter(distp,B=="BETA", ID %in% similar_lengthID),aes(x=0,y=value)) + 
  geom_beeswarm() + 
  facet_wrap(~CH)+
  scale_y_log10()

# look at O2
ggplot(filter(distp,B=="BETA", CH=='O2', ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) + xlim(0,10)

# look at O1
ggplot(filter(distp,B=="BETA", CH=='O1',ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) + xlim(0,10)

ggplot(filter(distp,B=="BETA",
             CH %in% c('C3','F3','O1','T3')), 
       aes(x=value)) +
  geom_histogram(bins=50) + xlim(0,10)

ggplot(filter(distp,B=="BETA",
              CH %in% c('C4','F4','O2','T4')), 
       aes(x=value)) +
  geom_histogram(bins=50) + xlim(0,10)

# look at all channels summed
ggplot(filter(distp,B=="BETA", ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) + xlim(0,10)

# Plot violins
ggplot(filter(distp, ID %in% similar_lengthID),aes(x=CH,y=value)) + 
  geom_violin() + 
  facet_wrap(~B) +
  scale_y_log10()

