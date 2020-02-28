# chat data scripts

library(tidyverse)
library(sindyr)
library(tvR)
library(GGally)
library(umap)
library(gdata)
#library(SimilarityMeasures)
library(dtw)
library(TSdist)
library(markovchain)
#### source other files
cd('~/Desktop/ultra')
source('figures.R')
source('asymmetry.R')
source('sampling.R')
source('chat.R')
source('outliers.R')

#### template for computing cross-session variability
       
hypno_mm1 <- m1 %>% group_by(ID) %>% 
  arrange(E) %>% 
  summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
            tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
            stagesum=sum(STAGE_N/sum(PERSISTENT_SLEEP)))

hypno_mm2 <- m2 %>% group_by(ID) %>% 
  arrange(E) %>% 
  summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
            tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
            stagesum=sum(STAGE_N)/sum(PERSISTENT_SLEEP))

hypno <- left_join(hypno_mm1, hypno_mm2, by='ID') %>% 
  drop_na()
cor(hypno$stagesum.x, hypno$stagesum.y)
plot(hypno$stagesum.x, hypno$stagesum.y)
##### unpack and summarize all Markov variables

# why doesn't this work
hypno_mm_full1 <- m1 %>% group_by(ID) %>%
  arrange(E) %>%
  summarize(!!!unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix))

hypno_mm_full2 <- m2 %>%
  arrange(E) %>% group_by(ID) %>%
  summarize(!!!(unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix)))


ggplot(hypno, aes(x=log(det.x)-log(det.y)))+geom_freqpoly()

#### partition entropy


##### dtw and regularization stuff
compare_CHB(m1, m2, '300001','O1','SLOW')
test1 <- filter(m1, ID=='300001') %>% select(E,ID,'SLOW_O1') %>% arrange(E)
test2 <- filter(m2, ID=='300001') %>% select(E,ID,'SLOW_O1') %>% arrange(E)
d <- dtw(test1[['SLOW_O1']], test2[['SLOW_O1']],  
    step.pattern=rabinerJuangStepPattern(4, "c", TRUE), 
    open.begin=TRUE, open.end=TRUE, keep=TRUE)
plot(d, 'twoway')
dtw_tab <- dtw_table(m1, m2)

