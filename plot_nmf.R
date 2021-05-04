# plot factors and reconstructed spectrogram
# highlight factors
library(tidyverse)
library(patchwork)
library(zeallot)
source('~/dyn/src/dynamics/cleandata.R')

labels <- c(V1="Slow", 
            V2="Delta", 
            V3="Theta", 
            V4="Sigma", 
            V5="Beta", 
            V6="Gamma")

labeller <- function(variable,value){
  return(labels[value])
}

stylize <- function(gg){
    gg+
      ggsci::scale_color_uchicago()+
      ggsci::scale_fill_uchicago()+
      theme_classic(base_size=20)
}

plot_W_all <- function(W){
  gg <- ggplot(W, aes(x=FR,y=value, color=component))+
    #geom_point(size=0.5, alpha=0.1)+
    geom_point(size=2, alpha=0.4)+
    stat_summary(geom='path', size=2)+
    #geom_ribbon(aes(ymin = quantile.0.05, ymax = quantile.0.95, fill = "05%-95%"), alpha = .25)+
    ylab('w (normalized)')+
    xlab('Frequency (Hz)')
  stylize(gg)+theme(legend.position=c(0.8,0.7))
}

plot_W_session <- function(W){
  gg <- ggplot(W %>% split_id(), aes(x=FR,y=value, color=session))+
    stat_summary(geom='path')+
    stat_summary(geom='pointrange', size=0.2)+
    ylab('w (normalized)')+
    xlab('Frequency (Hz)')+
    facet_wrap(~component, nrow=1)
  stylize(gg)+theme(legend.position=c(0.8,0.7))+ggtitle('')
}

plot_H_stages <- function(H){
  ggplot(H, aes(x=E, y=value, color=factor(STAGE_N)))+
    geom_point(size=0.5)+
   # ggsci::scale_color_uchicago()+
    scale_color_brewer(palette='Set1')+
    facet_wrap(~component, scales='free', nrow=7)+
    theme_classic(base_size=20)
}

plot_factors_spectrogram <- function(W,H){
  WH <- left_join(W,H, by=c('component', 'ID'), suffix=c('W','H')) %>%
    mutate(valueWH = valueW*valueH) %>%
    group_by(E) %>% mutate(valueWH=normalize(valueWH)) %>%
    mutate(FR=replace(FR, FR ==0, -0.5)) # rastering breaks otherwise
  #p_fr <- ggplot(W)
  #p_time <- ggplot(H)
  #p_hypno <- ggplot(H)
  #p_spectrogram <- 
  ggplot(WH, aes(x=E, y=FR, fill=valueWH))+
    geom_tile()+
    geom_line(aes(y=(STAGE_N+3)*10), color='green', alpha=0.5)+
    facet_wrap(~component, nrow=6)+
    scico::scale_fill_scico(palette='vik')+
    theme_classic(base_size = 20)
}

plot_factor_reconstruct <- function(W,H){
  WH <- left_join(W,H, by=c('component', 'ID'), suffix=c('W','H')) %>%
    mutate(valueWH = valueW*valueH) %>%
    group_by(E, FR) %>% 
    summarize(valueWH=sum(valueWH)) %>%
    #mutate(valueWH=normalize(valueWH)) %>%
    mutate(FR=replace(FR, FR ==0, -0.5))
    
  
  ggplot(WH, aes(x=E, y=FR, fill=valueWH))+
    geom_tile()+
    #geom_line(aes(y=(STAGE_N+3)*10), alpha=0.3)+
    scico::scale_fill_scico(palette='vik') +
    theme_void()#+
    #xlim(1,max(WH$E))
}

plot_spectrogram <- function(Y){
  
  p1 <- ggplot(Y, aes(x=E, y=FR, fill=log(PSD)))+
    geom_tile()+
    scico::scale_fill_scico(palette='vik')+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"))
}

plot_factors <- function(results){
  p1 <- ggplot(results$H, aes(x=E, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')
  p2 <- ggplot(results$W, aes(x=FR, y=value, color=component))+
    geom_path()+scale_color_brewer(palette='Set1')
  p2/p1
}

plot_hilo <- function(H, wdemo, V='V1'){
  maxID <- wdemo %>% filter(!!sym(V)==max(!!sym(V)))
  minID <- wdemo %>% filter(!!sym(V)==min(!!sym(V)))
  H1 <- filter(H, ID==maxID)
  H0 <- filter(H, ID==minID)
  p1 <- ggplot(H1, aes(x=E, y=value, color=component))+geom_point()
  p0 <- ggplot(H0, aes(x=E, y=value, color=component))+geom_point()
  p1/p0
}

plot_range <- function(wdemo, data_segment, var, comp) {
  wdemo <- wdemo %>% arrange(!!sym(var))
  IDlist <- wdemo$nsrrid[seq(1, length(wdemo$nsrrid), 20)]
  ts <- filter(data_segment %>% 
                 split_id() %>% 
                 filter(session=='baseline'), 
               nsrrid %in% IDlist) %>%
    mutate(nsrrid = factor(nsrrid, levels = IDlist)) %>%
    group_by(nsrrid, component) %>%
    mutate(E = E - min(E)) %>%
    filter(component==comp)
  
  ggplot(ts, aes(x=E, y=value,color=nsrrid))+
    geom_point()+
    facet_wrap(~nsrrid, nrow=1)+
    theme_void()
}

# plot spectrogram, factors
plot_factorization <- function(Y, H, W){
  
  Y <- Y %>%
    mutate(FR=replace(FR, FR ==0, -0.5)) %>%
    filter(CYCLE>=1)
  
  p1 <- ggplot(H, aes(x=E, y=value, color=component))+
    geom_path()+ggsci::scale_color_uchicago()+
    theme_void()
  p2 <- ggplot(W, aes(x=FR, y=value, color=component))+
    geom_path(size=1)+ggsci::scale_color_uchicago()+coord_flip()+
    scale_x_reverse()+
    ylim(0,0.15)+
    theme_void()
  p3 <- plot_spectrogram(Y)+scale_y_reverse()+theme_void()
  p4 <- plot_factor_reconstruct(W,H)+scale_y_reverse()
  (p3 | p2) / (p1 | p4) + 
    plot_layout(guides='collect')
}

# plot factors, tiled
plot_factorization_tile <- function(Y, H, W){
  
  Y <- Y %>%
    mutate(FR=replace(FR, FR ==0, -0.5)) %>%
    filter(CYCLE>=1)
  
  p1 <- ggplot(H, aes(x=E, y=component, fill=value))+
    geom_tile()+
    scico::scale_fill_scico(palette='vik')+
    theme_void()
    
  
  p2 <- ggplot(W, aes(x=component, y=FR, fill=value))+
    geom_tile()+
    scico::scale_fill_scico(palette='vik')+
    theme_void()
  
  p3 <- plot_spectrogram(Y)+theme_void()
  p4 <- plot_factor_reconstruct(W,H)#+scale_y_reverse()
  (p3 | p2) / (p1 | p4) + 
    plot_layout(guides='collect')
}

# classical frequency bands vs. group fit
plot_bands_cf <- function(groupfit){
  groupfit <- as_tibble(groupfit$W)
  bands <- tibble()
  bandlimits <- tibble(lb=c(0,1,4,10,16,30), ub = c(1,4,10,16,30,45))
  FR <- seq(from=0.5, to=45, by=0.5)
  bands <- tibble(FR=FR)
  i <- 0
  for (band in c('Slow', 'Delta', 'Theta', 'Sigma', 'Beta', "Gamma")){
    i <- i+1
    bands <- bands %>% mutate(!!band := as.numeric((FR > bandlimits$lb[i]) & 
                                (FR <= bandlimits$ub[i]))*0.125) # 
  }
  bands <- bands %>% pivot_longer(-FR, 
                                  names_to='component', 
                                  values_to = 'value') %>%
    mutate(component=factor(component)) %>%
    mutate(component=fct_relevel(component,
                                 c('Slow', 
                                   'Delta',
                                   'Theta',
                                   'Sigma',
                                   'Beta',
                                   'Gamma')))
  bands$type <- 'classical bands'
  # chat only
  names(groupfit) <- c('Beta', 'Slow', 'Delta', 'Theta',"Sigma","Gamma")
  groupfit$type <- 'NMF components'
  groupfit$FR <- FR
  groupfit <- groupfit %>% pivot_longer(1:6, 
                                        names_to='component', 
                                        values_to='value') %>% 
    mutate(component=factor(component)) %>%
    mutate(component=fct_relevel(component,
                                 c('Slow', 
                                          'Delta',
                                          'Theta',
                                          'Sigma',
                                          'Beta',
                                          'Gamma')))
  mat <- rbind(bands, groupfit) 
  p1 <- ggplot(mat, aes(y=FR, 
                        fill=value, 
                        x=component))+
    geom_tile()+facet_wrap(~type,nrow=1)+
  scico::scale_fill_scico(palette='vik')+
    theme_classic(base_size=14)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.position='none')+
    ylab('Frequency (Hz)')
  p2 <- ggplot(mat, aes(x=FR, 
                  y=value, 
                  color=component,
                  linetype=type))+
    geom_line()+facet_wrap(~component,nrow=2)+
    theme_classic(base_size=14)+
    xlab('Frequency (Hz)')
    
  p1 + stylize(p2) +theme(legend.position='none')+ plot_layout(guides='collect', widths=c(1,2))
}

plot_baseline_followup_W <- function(df, id){

  splitfit <- split_id(df)
  df2 <- filter(splitfit, nsrrid %in% id)
  gg <- ggplot(df2, aes(x=FR, 
                  y=value, 
                  color=component, 
                  linetype=session))+
    geom_path(size=0.75)+
    facet_grid(component~nsrrid)+
    xlab("Frequency (Hz)")+
    ylab("w (normalized)")
  stylize(gg)+theme_void()
}

order_nmf_factors <- function(H,W){
  
}

get_yhw <- function(db, nmffit, id) {
  Y <- db %>% filter(ID==id) %>% collect() %>%
    rename(FR=F)
  H <- nmffit$H %>% filter(ID==id)
  W <- nmffit$W %>% filter(ID==id)
  list(Y=Y, H=H, W=W)
}

plot_shuffle_W <- function(actual, nul){
  gg <- ggplot(nul, aes(x=estimate, fill=component))+
    geom_histogram(bins=50)+
    geom_vline(data=actual, aes(xintercept=estimate), linetype='dashed')+
    facet_wrap(~component, nrow=1, scales='free')+
    xlab('r')+
    guides(fill=FALSE)
  stylize(gg)+theme(axis.text.x=element_text(angle = 45, hjust=1, size=12))
}

plot_session_histogram <- function(corW){
  gg <- ggplot(corW, aes(x=estimate, fill=component))+
  geom_histogram()+
    geom_vline(aes(xintercept=mean(estimate)), 
               color='black', linetype='dashed')+
  facet_wrap(~component, nrow=1)+
    xlab('r (baseline vs. followup)')+
    guides(fill=FALSE)
  stylize(gg)+theme(axis.text.x=element_text(angle = 45, hjust=1, size=12))
}

plot_z <- function(z){
  gg <- ggplot(z, aes(x=component, xend=component, y=zscore,yend=0, color=component))+
    geom_segment(size=5)+
    geom_point(size=10)+
    coord_flip()+
    ylab("z score (actual vs. shuffled)")+
    scale_x_discrete(limits = rev(levels(z$component)))
    
  stylize(gg)+theme(legend.position='none')
}

plot_err <- function(err) {
  err <- mutate(err, pos = (err_diff>0))
  gg <- ggplot(err, aes(x=err_diff, fill=pos))+
    geom_histogram(bins=300)+
    geom_vline(aes(xintercept=0))+
    xlim(-1250,1250)+
    xlab("Random seed - fixed seed reconstruction error")
  stylize(gg)
}

plot_zoom_spectrogram <- function(Y,Emin,Emax){
  plot_spectrogram(Y)+
     ggforce::facet_zoom(xlim=c(Emin, Emax))
}

plot_cfs_chat <- function(){
  load("~/dyn/rdata/chat-final6.Rdata")
  source("~/dyn/src/dynamics/chat0.2.R")
  W_chat <- (refit %>% reorder_factors6())$W
  
  rm(datahyp)
  load("~/dyn/rdata/cfs-finalish6.Rdata")
  source("~/dyn/src/dynamics/cfs.R")
  W_cfs <- (refit %>% reorder_factors6())$W
  
  W <- rbind(W_cfs, W_chat)
  
  W <- W %>% 
    split_id()
  
  gg <- ggplot(W, aes(x=FR, y=value, color=study))+
    stat_summary(geom='line', size=1)+
    geom_point(alpha=0.001)+
    facet_wrap(~component)
  stylize(gg)
}

plot_demo_classes <- function(W, demo, var){
  df <- join_var(W, demo, var) %>% drop_na()
  gg <- ggplot(df, aes(x=FR, y=value, color=as.factor(get(var))))+
    stat_summary(geom='line', size=1)+
    facet_wrap(~component)
  stylize(gg)+scale_color_grey()
}

plot_demo_fr <- function(W, fr, component, demo, var){
  df <- join_var(W, demo, var) %>%
    filter(component == sym(!!component)) %>%
    drop_na()
  p1 <- ggplot(df, aes(x=FR, y=value, color=as.factor(get(var))))+
    stat_summary(geom='linerange')+
    geom_vline(xintercept=fr, alpha=0.1)
  fixed_f <- filter(df, FR==fr)

  p2 <- ggplot(fixed_f, aes(x=as.factor(get(var)), y=value))+
    ggbeeswarm::geom_beeswarm(size=0.5, alpha=0.2)+
    stat_summary(color='blue', size=0.1)
  stylize(p1)/stylize(p2)
}

plot_age_fr <- function(W, fr, component, demo){
  df <- left_join( demo,W %>% split_id()) %>%
    filter(component == sym(!!component)) #%>%
    #drop_na()
  p1 <- ggplot(df, aes(x=FR, y=value, color=age))+
    #stat_summary(geom='line')+
    geom_point()+
    geom_vline(xintercept=fr)
  fixed_f <- filter(df, FR==fr)
  
  p2 <- ggplot(fixed_f, aes(x=age, y=value, color=age))+
    geom_point(size=0.5)+
    geom_smooth(method='lm')+
    ggpmisc::stat_fit_glance(method = "lm", 
                             method.args = list(formula = y~x),
                             label.x = "right",
                             label.y = "bottom",
                             aes(label = paste("italic(P)*\"-value = \"*", 
                                               signif(stat(p.value)*600, digits = 4),
                                               sep = "")),
                             parse = TRUE)
    #stat_summary(formula = age ~ value)+
    #ggpmisc::stat_poly_eq(formula = age ~ value)
  (stylize(p1)+scale_color_continuous())/(stylize(p2)+scale_color_continuous())
}

plot_contin_fr <- function(W, fr, component, demo, var){
  df <- left_join( demo,W %>% split_id()) %>%
    filter(component == sym(!!component)) #%>%
    #mutate(var_category = cut(get(!!var), 3))
  p1 <- ggplot(df, aes(x=FR, y=value)) +#color=get(var_category)))+
    stat_summary(geom='line')+
    geom_vline(xintercept=fr, alpha=0.1)
  fixed_f <- filter(df, FR==fr)
  formula <- y~x
  p2 <- ggplot(fixed_f, aes(x=get(var), y=value))+
    geom_point(size=0.5)+
    #geom_smooth(method='lm')+
  #stat_summary(formula = age ~ value)+
  ggpmisc::stat_fit_glance(method = "lm", 
                           method.args = list(formula = y~x),
                           label.x = "right",
                           label.y = "bottom",
                           aes(label = paste("italic(P)*\"-value = \"*", 
                                             signif(stat(p.value)*600, digits = 4),
                                             sep = "")),
                           parse = TRUE)+
    xlab(var)
  stylize(p1)/stylize(p2)
}

plot_W_corr <- function(W_chat) {
  corr <- W_chat %>% split_id() %>%
    pivot_wider(names_from = session, values_from = value) %>%
    group_by(component, FR) %>%
    #mutate(baseline = outliers(baseline), followup = outliers(followup)) %>%
    mutate(baseline = outliers(DarkCycle), followup = outliers(LightCycle)) %>%
    mutate(variance = 
             var(baseline, na.rm=TRUE)+
             var(followup, na.rm=TRUE)) %>%
    filter(variance > 0.000002) %>%
    #group_modify(~tidy(cor.test(.$baseline, .$followup)))%>%
    group_modify(~tidy(cor.test(.$DarkCycle, .$LightCycle)))
  
  p <- ggplot(corr, aes(x=FR, 
                        y=estimate, 
                        ymin=conf.low, 
                        ymax=conf.high, 
                        color=factor(component)))+
    geom_linerange()+
    geom_point(size=0.5)+ 
    facet_wrap(~component)
  stylize(p)+theme(legend.position='none')
}

plot_coupling_z <- function(spso, W_cfs){
  stylize(ggplot(spso %>%
                   rename(FR=F) %>%
                   filter(FR == 15) %>%
                   left_join(W_cfs %>% filter(component=='Delta')), 
                 aes(x=value, y=COUPL_ANGLE))+
            geom_point()+geom_smooth(method='lm')+
            ggpmisc::stat_fit_glance(method = "lm", 
                                     method.args = list(formula = y~x),
                                     label.x = "right",
                                     label.y = "bottom",
                                     aes(label = paste("italic(P)*\"-value = \"*", 
                                                       signif(stat(p.value)*600, digits = 4),
                                                       sep = "")),
                                     parse = TRUE))
}

plot_H_corr <- function(H) {
  Hcorr <- H %>% 
    group_by(nsrrid, component, STAGE_N) %>%
    pivot_wider(names_from=component, values_from=value) %>%
    group_modify(~tidy(cor(.[,4:9]))) %>%
    pivot_longer(4:9, names_to='component', values_to='value') %>%
    group_by(component, STAGE_N, .rownames) %>%
    summarize(meancorr = mean(value, na.rm=TRUE)) %>%
    filter(STAGE_N %in% c(-3,-2,-1,0,1)) %>% 
    pivot_wider(names_from=.rownames, values_from=meancorr) %>%
    rf()
  for (stage in seq(-3,1)){
    Hc <- Hcorr %>% filter(STAGE_N == stage) %>% as.data.frame()
    rownames(Hc) <- Hc$component
    p <- ggcorrplot::ggcorrplot(Hc[,3:8], 
                                type='lower',
                                lab='true')
    print(p+ggtitle(stage,))
  }
    
    
    summarize(meanH = mean(value)) %>%
    group_by(component,STAGE_N) %>%
    pivot_wider(names_from=component, values_from=meanH) %>%
    group_modify(~tidy(cor(.))) %>%
    round(2)
  
}

plot_factor_band_corr <- function(H, bandpower){
  
}

#chatbase <- W_chat %>% 
#  split_id() %>% 
 # filter(session == 'baseline') %>%
#  unite_id()
 
 
 # anxiety, depression, insomnia, medication
 # plot_demo_classes(W_cfs, demo2, 'ANXDIAG')
 # plot_demo_fr(W_cfs, 10.5, 'Slow', demo2, 'ANXDIAG')
 # plot_demo_fr(W_cfs, 8, 'Theta', demo2, 'ANXDIAG')
 # plot_demo_fr(W_cfs, 4, 'Delta', demo2, 'ANXDIAG')
 

# #plot_bands_cf(groupfit)
# #plot_z(z)
# #plot_err(err_diff_refit)
# #plot_W_all(refit2$W)
# #plot_baseline_followup_W(refit2$W, 
#                   #       c(300004, 300007, 300037, 300041,300043,300051))
# f <- 11
# component2 <- 'V3'
# var <- 'age'
# demo_cfs2 <- demo #%>% mutate(cystatinc = cut_number(cystatinc, 2))
# 
# #plot_demo_fr(refit$W, f, component2, demo_cfs2, var)
# demo2 <- demo_cfs2 #%>%
#  #mutate(age = cut_number(age,2))
# plot_demo_classes(W_trt %>% unite_id(),
#                   demo %>% filter(race3 < 3), 
#                   'race3')
# plot_demo_fr(W_trt %>% unite_id(), 
#              10, 
#              'Delta', 
#              demo %>% filter(race3 < 3), 
#              'ageyear_at_meas')
# stats <- list()
# for (fr in seq(from=0.5,to=45, by=0.5)){
#   for (band in c('Slow', 
#                 'Delta', 
#                 'Theta', 
#                 'Sigma', 
#                 'Beta',
#                 'Gamma')){
#     
#       st <- W_trt_demo %>% 
#       rename(age = ageyear_at_meas, SEX = male, race = race3) %>% 
#       filter(FR == fr, component == band) %>% 
#       regress_all('value')
#       print(st)
#       print(paste("^", band, fr))
#       stats[[paste(band, fr)]] <- st
# }}
# stats_mean <- list()
# for (stage in unique(H_mean_demo$STAGE_N)){
#   for (band in c('Slow', 
#                  'Delta', 
#                  'Theta', 
#                  'Sigma', 
#                  'Beta',
#                  'Gamma')){
#     
#     st <- H_mean_demo %>% 
#       filter(STAGE_N == stage, component == band) %>%
#       ungroup() %>%
#       drop_na(age) %>%
#       regress_all('meanH')
#     print(st)
#     print(paste("^", stage, band))
#     stats_mean[[paste(band, fr)]] <- st
#   }}
# 
 df <- join_var(refit$W, demo, 'm1lbenzo') %>% drop_na() %>%
   filter(component=="V2", FR==3)
coin::independence_test(formula=value~m1lbenzo, data=df)
# kruskal.test(formula=value~race3, data=df)
# plot_demo_fr(W_cfs, 10.5, 'Slow', demo2, 'ess')
# plot_demo_fr(W_cfs, 12.5, 'Sigma', demo2, 'ess')
#plot_demo_fr(chatbase, 20, 'Beta', demo2, 'das5a')
# 
# plot_demo_classes(W_cfs, demo2, 'be')
# plot_demo_fr(W_cfs, 8, 'Theta', demo2, 'be')


# refit2 <- reorder_factors6(refit)
# c(Y,H,W) %<-% get_yhw(datahyp, nmf_fit, 'chat-baseline-300021')
# plot_factorization(Y,H,W)
# c(Y,H,W) %<-% get_yhw(datahyp, refit2, 'chat-baseline-300021')
# plot_factorization(Y,H,W)
#plot_zoom_spectrogram(Y %>% filter(CYCLE >= 1), 500,700)
#plot_factorization_tile(Y,H,W)
#band_H <- psd_bands_hyp %>% 
  #filter(ID=='chat-baseline-300021', CYCLE >= 1) %>%
  #mutate(value=log(value/min(value)))
#plot_factors_spectrogram(W,H)
#plot_H_stages(H)
#plot_H_stages(band_H)
# c(Y,H,W) %<-% get_yhw(datahyp, refit, 'chat-followup-300352')
# plot_factorization(Y,H,W)

# p1 <- plot_session_histogram(corW)
# p2 <- plot_session_histogram(corW2)
# (p1+ggtitle("Individuals", 
#             subtitle= "NMF1"))/(p2+ggtitle('',subtitle='NMF2'))
# p3 <- plot_shuffle_W(actual, nul)
# p4 <- plot_shuffle_W(actual2, nul2)
# (p3+ggtitle("Shuffled mean", 
#             subtitle="NMF1"))/(p4+ggtitle('',subtitle='NMF2'))

#id <- sample(unique(Wall6$ID),1)
#H <- filter(Hall6, ID==id)
#W <- filter(Wall6, ID==id)
#plot_spectrogram(results$orig)
#plot_factor_reconstruct(results$W,results$H)
#plot_factor_spectrogram(results$W,results$H)