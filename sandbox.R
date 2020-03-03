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


       
m1n <- m1 %>% drop_na()
m2 <- m2 %>% drop_na()
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

