library(broom)
library(purrr)
# get correlation for 
cor_sessions_W <- function(W){
  W %>% split_id %>%
    pivot_wider(names_from='session', values_from = 'value') %>%
    group_by(component) %>%
    group_modify(~tidy(cor.test(.$baseline,.$followup)))
}

cor_sessions_indiv_W <- function(nmf_fit){
  
    nmf_fit$W %>% split_id %>%
      pivot_wider(names_from='session', values_from = 'value') %>%
      group_by(nsrrid, component) %>%
      drop_na() %>%
      group_modify(~tidy(cor.test(.$baseline,.$followup)))
}

permute_ID_W <- function(nmf_fit){
  W <- nmf_fit$W %>% 
    pivot_wider(names_from=c('component','FR'), values_from=value) %>%
    mutate(ID=sample(ID)) %>%
    pivot_longer(-ID,names_to=c('component','FR'), names_sep='_') %>%
    mutate(FR=as.numeric(FR))
}
null_W_cor <- function(nmf_fit, n){
  correl <- NULL
  for (i in seq(n)){
    print(i)
    cor <- permute_ID_W(nmf_fit) %>% cor_sessions_W()
    cor$i <- i
    correl <- rbind(correl, cor)
  }
  correl
}

# reorder factors (sigh)
rf <- function(df) {
  labels <- c("Slow", 
              "Delta", 
              "Theta", 
              "Sigma", 
              "Beta", 
              "Gamma")  
  df %>% mutate(component = factor(component)) %>% 
    mutate(component=ordered(component,labels))
}

# need to change how demo is loaded!
shuffle_demo_W <- function(nmf_fit,demo_var){
  var <- names(select(demo_var, -nsrrid))
  W <- nmf_fit$W %>% 
    pivot_wider(names_from=c('component','FR'), values_from=value) %>%
    split_id() %>%
    filter(session=='baseline') %>%  # restrict to baseline for now
    left_join(demo_var) %>%
    mutate(var=sample(!!(sym(var)))) %>% # shuffle
    unite_id() %>%
    pivot_longer(c(-ID, -var),names_to=c('component','FR'), names_sep='_') %>%
    mutate(FR=as.numeric(FR))
}

join_var <- function(W, demo, var){
  demo <- demo %>% select(nsrrid, !!sym(var)) 
   left_join(W %>% split_id(), demo) #%>%
     #filter(session == 'baseline')
}

null_W <- function(nmf_fit, demo, var, n){
  samples <- NULL
  demo_var <- demo %>% select(!!sym(var), nsrrid)
  for (i in seq(n)){
    print(i)
    samp <- shuffle_demo_W(nmf_fit, demo_var)
    samp$i <- i
    samples <- rbind(samples, samp)
  }
  samples
}
#nulW <- null_W(refit, demo, 'male', 3)
# Wd <- join_var(refit, demo, 'age_category') %>% filter(age_category < 10)
# Wd15 <- join_var(refit, demo, 'EMPLOY') %>% filter(FR==30)
# ggplot(Wd15, aes(y=value, x=EMPLOY))+
#   geom_point()+
#   #geom_smooth()+
#   facet_wrap(~component)

#nmf_fit2 <- reorder_factors6(nmf_fit)
#refit2 <- reorder_factors6(refit)


