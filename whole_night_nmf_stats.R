# whole night NMF stats
library(Hmisc)


# computes correlation for time series
Hall6 %>% 
  group_by(ID) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  select(-E, -STAGE_N) %>%
  group_modify(~tidy(rcorr(as.matrix(.)))) %>%
  ungroup() %>%
  group_by(column1, column2) %>%
  summarise(meancorr = mean(estimate), std = pracma::std_err(estimate))

Wall6 %>% 
  group_by(ID) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  select(-FR) %>%
  group_modify(~tidy(rcorr(as.matrix(.)))) %>%
  ungroup() %>%
  group_by(column1, column2) %>%
  summarise(meancorr = mean(estimate), std = pracma::std_err(estimate))