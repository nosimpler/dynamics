# DTW
library(dtw)
library(tidyverse)
library(tvR)
library(NMF)

library(ggbeeswarm)

source('~/Desktop/dynamics/outliers.R')
source('~/Desktop/dynamics/chat.R')
source('~/Desktop/dynamics/figures.R')

dnx <- function(x){denoise1(x,1e30)}
#dtw_table <- function(m1, m2){
  
  distance2 <- NULL

  # builds dynamic-time-warping distance table
  for (id in flatten(IDlist)){
    print(id)
    rows <- list()
    test1 <- m1 %>% filter(ID==id, CYCLE==1) %>% arrange(E)
    test1 <- remove_outliers(test1)
    test2 <- m2 %>% filter(ID==id, CYCLE==1) %>% arrange(E)
    test2 <- remove_outliers(test2)
    for (i in 1:96){
      # use signal instead of derivative for comparison
      d1 <- diff(log10(dnx(test1[, i]/max(test1[,i]))))
      d2 <- diff(log10(dnx(test2[, i]/max(test2[,i]))))
      rows[[colnames(test1)[i]]] <- dtw(d1, d2,  
                                      step.pattern=rabinerJuangStepPattern(1, "c", TRUE), 
                                      open.begin=TRUE, open.end=TRUE)$normalizedDistance
    }
  t <- as_tibble(rows)
  t$ID <- id
  
  distance2 <- bind_rows(distance2,t)
}


######### FIGURES ########
# put in tidy form
distp <- pivot_dist(distance)
distp2 <- pivot_dist(distance2)

#### BIMODALITY AND CYCLE LENGTH SIMILARITY
# Plot violins (note bimodality in beta)
ggplot(distp,aes(x=CH,y=value)) + 
  geom_violin() + 
  facet_wrap(~B) +
  scale_y_log10()


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
ggplot(diffcycle_length, aes(x=abs(difflength)))+
  geom_histogram(bins=40)+
  geom_freqpoly()

# find individuals whose cycle lengths were similar
similar_lengthID <- filter(diffcycle_length, abs(difflength)<50)$ID


# plot beeswarm for cycles with similar length
ggplot(filter(distp,B=="BETA", ID %in% similar_lengthID),aes(x=0,y=value)) + 
  geom_beeswarm() + 
  facet_wrap(~CH)+
  scale_y_log10()

# look at T3
ggplot(filter(distp,B=="BETA", CH=='T3', ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) #+ xlim(0,10)

# look at T4
ggplot(filter(distp,B=="BETA", CH=='T4',ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) #+ xlim(0,10)

# sum left channels
ggplot(filter(distp,B=="BETA",
             CH %in% c('C3','F3','O1','T3'),
             ID %in% similar_lengthID), 
       aes(x=value)) +
  geom_histogram(bins=200) + xlim(0,10)

# sum right channels
ggplot(filter(distp,B=="BETA",
              CH %in% c('C4','F4','O2','T4'),
              ID %in% similar_lengthID), 
       aes(x=value)) +
  geom_histogram(bins=200) + xlim(0,10)

# look at all channels summed
ggplot(filter(distp,B=="BETA", ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) #+ xlim(0,10)

# Plot violins
ggplot(filter(distp, ID %in% similar_lengthID),aes(x=CH,y=value)) + 
  geom_violin() + 
  facet_wrap(~B) +
  scale_y_log10()

#### individuals

# plot nth place individual/channel/band
idx <- 1
df <- arrange(distp, value) %>% filter(ID %in% similar_lengthID)
compare_CHB(m1, m2, df[idx,]$ID, df[idx,]$CH, df[idx,]$B, cycle=1)
print(df[idx,])


length(similar_lengthID)
distpg <- distp %>% group_by(CH, B) %>% summarize(median=median(value))
dfpg <- df %>% group_by(CH, B) %>% summarize(median=median(value))
ggplot(distpg, aes(x=CH, y=B,fill=-log10(median)))+geom_tile()+ggtitle('all cycles')
ggplot(dfpg, aes(x=CH, y=B,fill=-log10(median)))+geom_tile()+ggtitle('similar length cycles (n=126)')

ggplot(df,aes(x=B,y=value))+
  geom_boxplot()+
  facet_wrap(~CH)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_log10()




distbeta <- df %>% filter(B == 'BETA') %>% arrange(value)
idx <- 1
compare_CHB(m1, m2, 
            distbeta[idx,]$ID, 
            distbeta[idx,]$CH, 
            distbeta[idx,]$B, 
            cycle=1)

distdelta <- df %>% filter(B == 'DELTA')
idx <- 1464
compare_CHB(m1, m2, 
            distdelta[idx,]$ID, 
            distdelta[idx,]$CH, 
            distdelta[idx,]$B, 
            cycle=1)



