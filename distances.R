# DTW
library(dtw)
library(tidyverse)
library(tvR)
library(NMF)
library(TSdist)
library(ggbeeswarm)

source('~/Desktop/dynamics/outliers.R')
source('~/Desktop/dynamics/chat.R')
source('~/Desktop/dynamics/figures.R')

# change to get second, third cycle
dnx <- function(x){denoise1(x,1e30)}


##### DTW DISTANCE
dtw_table <- function(m1, m2){
  
  distance <- NULL

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
  
  distance <- bind_rows(distance2,t)
  }
  distance
}


###### ERP DISTANCE
erp_table <- function(m1, m2){
  # trying regularized value instead of differential
  # not restricted to first cycle
  distance <- NULL
  
  for (id in flatten(IDlist)){
    print(id)
    rows <- list()
    test1 <- m1 %>% filter(ID==id) %>% arrange(E)
    test1 <- remove_outliers(test1)
    test2 <- m2 %>% filter(ID==id) %>% arrange(E)
    test2 <- remove_outliers(test2)
    for (i in 1:96){
      # use signal instead of derivative for comparison
      d1 <- dnx(diff(log10(test1[, i]/max(test1[,i]))))
      d2 <- dnx(diff(log10(test2[, i]/max(test2[,i]))))
      rows[[colnames(test1)[i]]] <- TSDistances(d1, d2, distance='erp', g=0)
    }
    t <- as_tibble(rows)
    t$ID <- id
    
    distance <- bind_rows(distance,t)
  }
  distance
}
distance_nodiff <- distance
  
#CYCLE LENGTH
#####

# write as function for later cycles
  
# cycle length for first recording
m1cycle_length <- m1 %>% select(ID,CYCLE) %>% 
  filter(CYCLE==1,ID %in% flatten(IDlist)) %>%
  group_by(ID) %>%
  summarize(cycle_length=n())

# cycle length for second recording
m2cycle_length <- m2 %>% select(ID,CYCLE) %>% 
  filter(CYCLE==1,ID %in% flatten(IDlist)) %>%
  group_by(ID) %>%
  summarize(cycle_length=n())

# difference in cycle length
diffcycle_length <- m2cycle_length %>% 
  transmute(ID=ID, 
            difflength=m1cycle_length$cycle_length-m2cycle_length$cycle_length)


# find individuals whose cycle lengths were similar
similar_lengthID <- filter(diffcycle_length, abs(difflength)<50)$ID
distance_similar <- filter(distance, ID %in% similar_lengthID)
hist(filter(m1cycle_length, ID %in% similar_lengthID)$cycle_length)
#distance_similar_erp <- filter(dstnc, ID %in% similar_lengthID)
distp_copy <- distance_similar %>% pivot_dist() %>% arrange(value)

#distp <- distance_similar_erp %>% pivot_dist()
#distp <- dist_erp
# plot beeswarm for cycles with similar length
ggplot(filter(distp,B=="BETA", ID %in% similar_lengthID),aes(x=0,y=value)) + 
  geom_beeswarm() + 
  facet_wrap(~CH)+
  scale_y_log10()

ggplot(distp,aes(x=B,y=value))+
  geom_boxplot()+
  facet_wrap(~CH)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_log10()

##### PLOT SIMILARITY ALL BANDS/CHANNELS

distpg <- distp %>% 
  group_by(CH, B) %>% 
  summarize(median=median(value))
ggplot(distpg, aes(x=CH, y=B,fill=-log10(median)))+
  geom_tile()+
  ggtitle('all cycles')

##### SLOW
ggplot(filter(distp, B=='SLOW'),aes(x=CH,y=value))+
  geom_violin()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_log10()

#####
# CROSS-ID ERP DISTANCE with nearby lengths
##### 

dtab <- NULL
row <- list()

for (id1 in flatten(IDlist)){
    print(id1)
    test1 <- m1 %>% 
      filter(ID==id1, CYCLE %in% c(1,2)) %>% 
      select(ID, E, 'DELTA_F3') %>%
      arrange(E)
    
    for (id2 in flatten(IDlist)){
      test2 <- m2 %>% 
        filter(ID==id2, CYCLE %in% c(1,2)) %>% 
        select(ID, E, 'DELTA_F3') %>%
        arrange(E)
      d1 <- dnx(diff(log10(test1$DELTA_F3/max(test1$DELTA_F3))))
      d2 <- dnx(diff(log10(test2$DELTA_F3/max(test2$DELTA_F3))))
      row$ID1 <- id1
      row$ID2 <- id2
      row$D <- TSDistances(d1, d2, distance='erp', g=0)
      row <- as_tibble(row)
      dtab <- rbind(dtab, row)
    }
  }
#####

# CROSS-ID ERP DISTANCE (first three hours)
#####

dtab <- NULL
row <- list()
for (id1 in flatten(IDlist)){
  print(id1)
  test1 <- m1 %>% 
    filter(ID==id1) %>% 
    select(ID, E, 'DELTA_F3') %>%
    arrange(E) %>%
    filter(E < min(E)+360)
  
  for (id2 in flatten(IDlist)){
    test2 <- m2 %>% 
      filter(ID==id2) %>% 
      select(ID, E, 'DELTA_F3') %>%
      arrange(E) %>%
      filter(E < min(E)+360)
    # check for nearby lengths
      d1 <- dnx(diff(log10(test1$DELTA_F3/max(test1$DELTA_F3))))
      d2 <- dnx(diff(log10(test2$DELTA_F3/max(test2$DELTA_F3))))
      row$ID1 <- id1
      row$ID2 <- id2
      row$D <- TSDistances(d1, d2, distance='erp', g=0)
      row <- as_tibble(row)
      dtab <- rbind(dtab, row)
  }
}
dtab3hr <- dtab

#####
# ERP DISTANCE FIRST CYCLE
#####
for (id in flatten(IDlist)){
  print(id)
  rows <- list()
  
  test1 <- remove_outliers(test1)
  test2 <- m2 %>% filter(ID==id, CYCLE==1) %>% arrange(E)
  test2 <- remove_outliers(test2)
  for (i in 1:96){
    # use signal instead of derivative for comparison
    d1 <- dnx(diff(log10(test1[, i]/max(test1[,i]))))
    d2 <- dnx(diff(log10(test2[, i]/max(test2[,i]))))
    print(paste(max(test1[,i]), max(test2[,i])))
    rows[[colnames(test1)[i]]] <- TSDistances(d1, d2, distance='erp', g=0)
  }
  t <- as_tibble(rows)
  t$ID <- id
  
  distance <- bind_rows(distance,t)
}
distance




#### ONE VARIABLE PLOTS (use app)
# filter distp as needed









#####
# ERP DISTANCE FIRST 3 HOURS RESTRICTED TO IDENTICAL SLEEP DURATION
#####
dtab <- NULL
row <- list()
for (id1 in flatten(IDlist)){
  print(id1)
  test1 <- m1 %>% 
    filter(ID==id1) %>% 
    select(ID, E, 'DELTA_F3') %>%
    arrange(E) %>%
    filter(E < min(E)+360)
  
  for (id2 in flatten(IDlist)){
    test2 <- m2 %>% 
      filter(ID==id2) %>% 
      select(ID, E, 'DELTA_F3') %>%
      arrange(E) %>%
      filter(E < min(E)+360)
    if (nrow(test2) == nrow(test1)){
      d1 <- dnx(diff(log10(test1$DELTA_F3/max(test1$DELTA_F3))))
      d2 <- dnx(diff(log10(test2$DELTA_F3/max(test2$DELTA_F3))))
      row$ID1 <- id1
      row$ID2 <- id2
      row$D <- TSDistances(d1, d2, distance='erp', g=0)
      row <- as_tibble(row)
      dtab <- rbind(dtab, row)
    }
  }
}
dtab3hr_identicalduration <- dtab

#####
# FIRST 360 EPOCHS
#####

dc3_1 <- m1c %>% 
  select(ID, E, DELTA_C3) %>% 
  group_by(ID) %>% top_n(-360,E) %>% 
  arrange(E)

dc3_2 <- m2c %>% 
  select(ID, E, DELTA_C3) %>% 
  group_by(ID) %>% top_n(-360,E) %>% 
  arrange(E)
