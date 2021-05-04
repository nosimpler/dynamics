library(broom)
library(purrr)
# get correlation for 

permute_ID_W <- function(W){
  W <- W %>% 
    pivot_wider(names_from=c('component','FR'), values_from=value) %>%
    split_id() %>%
    group_by(session) %>%
    mutate(nsrrid=sample(nsrrid)) %>%
    unite_id() %>%
    pivot_longer(-ID,names_to=c('component','FR'), names_sep='_') %>%
    mutate(FR=as.numeric(FR))
}

null_W_dist <- function(W, n){
  distance <- NULL
  for (i in seq(n)){
    print(i)
    d <- permute_ID_W(W) %>% test_retest_distance()
    d$i <- i
    distance <- rbind(distance, d)
  }
  distance %>% group_by(i, component) %>% 
    summarize(shuffled_mean = mean(dist))
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
manh.dist <- function(x1, x2) sum(abs(x1-x2))

test_retest_distance <- function(W){
  W %>% split_id %>%
    pivot_wider(names_from='session', values_from = 'value') %>%
    group_by(nsrrid, component) %>%
    drop_na() %>%
    summarize(dist = manh.dist(baseline, followup))
}

W_dist_z <- function(W_chat, nul){
  actual <- W_chat %>% 
    test_retest_distance() %>% rf %>%
    group_by(component) %>% 
    summarize(actual_mean = mean(dist))
  z <- nul %>% rf() %>% group_by(component) %>%
    summarize(null_mean = mean(shuffled_mean), null_std = sd(shuffled_mean)) %>%
    mutate(actual_mean = actual$actual_mean) %>%
    mutate(zscore = (actual_mean - null_mean)/null_std)
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

peaks_indiv <- function(W){
  W %>% group_by(ID,component) %>% filter(value==max(value))
}




# peaks <- peaks_indiv(refit$W)
# peaks_demo <- left_join(peaks %>% split_id(), demo)
# ggplot(peaks_demo, 
#        aes(y=age, x=factor(FR)))+
#   geom_boxplot()+
#   geom_smooth(method='lm')+
#   facet_wrap(~component, scales='free')
# regress_all(peaks_demo %>% 
#               filter(component=="V2") %>% 
#               drop_na(value,FR) %>% 
#               ungroup(), 
#            "FR")
#nulW <- null_W(refit, demo, 'male', 3)
# Wd <- join_var(refit, demo, 'age_category') %>% filter(age_category < 10)
# Wd15 <- join_var(refit, demo, 'EMPLOY') %>% filter(FR==30)
# ggplot(Wd15, aes(y=value, x=EMPLOY))+
#   geom_point()+
#   #geom_smooth()+
#   facet_wrap(~component)

#nmf_fit2 <- reorder_factors6(nmf_fit)
#refit2 <- reorder_factors6(refit)


