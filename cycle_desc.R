##### RELATIVE POSITION IN CYCLE
# one channel

m1r <- m1c %>% recast() %>%
  filter(CYCLE==1, STAGE != 'REM', ID %in% similar_lengthID) %>% 
  select(ID, DELTA_C3, CYCLE_POS_REL) %>% 
  group_by(ID) %>%
  mutate(CYCLE_POS_REL=cut(CYCLE_POS_REL/max(CYCLE_POS_REL), 10), # normalizes to NREM part of the cycle
    DELTA_C3 = DELTA_C3/max(DELTA_C3)) %>%
  group_by(ID, CYCLE_POS_REL) %>%
  summarize(DELTA_C3_MEAN=mean(DELTA_C3))



m2r <- m2c %>% recast() %>%
  filter(CYCLE==1, STAGE != 'REM', ID %in% similar_lengthID) %>% 
  select(ID, DELTA_C3, CYCLE_POS_REL) %>% 
  group_by(ID) %>%
  mutate(CYCLE_POS_REL=cut(CYCLE_POS_REL/max(CYCLE_POS_REL), 10), # normalizes to NREM part of the cycle
         DELTA_C3 = DELTA_C3/max(DELTA_C3)) %>%
  group_by(ID, CYCLE_POS_REL) %>%
  summarize(DELTA_C3_MEAN=mean(DELTA_C3))


ggplot(filter(m1r,ID %in% similar_lengthID), 
       aes(x=CYCLE_POS_REL, y=DELTA_C3_MEAN))+
  geom_boxplot()+
  scale_y_log10()+ theme_rob
ggplot(filter(m2r,ID %in% similar_lengthID), 
       aes(x=CYCLE_POS_REL, y=DELTA_C3_MEAN))+
  geom_boxplot()+
  scale_y_log10()+ theme_rob

deltaC3 <- pivot_wider(m1r, names_from='CYCLE_POS_REL', values_from="DELTA_C3_MEAN")
# look at principal components
dC3pc1 <- c(ID=similar_lengthID, pc=prcomp(deltaC3[,2:11])$x[,1])


