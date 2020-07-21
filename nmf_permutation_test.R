


##### permutation testing (under construction)
baseline_followup_diff <- function(W){
  W_in <- separate(W, ID, 
                   into = c("DATASET", "COND", "ID"), sep='-') %>%
    mutate(ID = as.numeric(ID))
  
  # compare within- to between-individual differences
  # permutation testing framework
  
  W_in <- pivot_wider(W_in, names_from=COND, values_from=value) %>%
    mutate(diff = baseline-followup)
}

#differences <- baseline_followup_diff(W_all)

# for (i in seq(10)){
#    IDlist_shuffle <- sample(IDlist$ID)
#    shufflemap <- tibble(ID=IDlist$ID, newID=IDlist_shuffle)
#    with_shuffle <- left_join(W_all, shufflemap)
#    W_permute <- with_shuffle %>%
#      select(-ID) %>%
#      rename(ID=newID)
#    shuffle_diff <- baseline_followup_diff(W_permute)
#    colname <- paste('Shuffle',i)
#    differences[[colname]] <- shuffle_diff$diff
# }
# differences2 <- pivot_longer(differences, starts_with('Shuffle '), names_to='Shuffle')
# ggplot(differences2, aes(x=FR, y=value))+geom_smooth()+
#   geom_point(aes(x=FR, y=diff))+facet_wrap(~component)
# demoW <- demo %>% rename(ID=nsrrid)