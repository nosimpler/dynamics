# stats on nmf factor correlations etc.

Hcor <- Hall6 %>% 
  group_by(ID) %>%
  pivot_wider(names_from=component, values_from=value) %>%
  select(starts_with('V')) %>%
  group_modify(~tidy(cor(as.matrix(.)))) %>% 
  pivot_longer(cols=starts_with('V'), names_to='component', values_to='value') %>%
  ungroup() %>% group_by(component, .rownames) %>%
  summarise(mean=mean(value), sd=pracma::std_err(value))

ggplot(Hcor, aes(x=component, y=.rownames, size=abs(mean), fill=mean))+
  geom_tile()+
  scale_fill_distiller(palette='Spectral')