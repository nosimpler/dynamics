# whole night NMF stats
library(Hmisc)

Hall6 %>% 
  group_by(ID) %>%
  select(starts_with('V')) %>%
  group_map(~rcorr(.))