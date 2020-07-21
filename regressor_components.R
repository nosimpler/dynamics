
regressors <- wdemo %>% select_if(~ !any(is.na(.))) %>%
    select_if(~is.numeric(.)) %>%
    select_if(~all(.>=0))
  
  regressed <- tibble()
  for (clmn in colnames(select_if(regressors, is.numeric))){