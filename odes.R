# ODE fitting
library(tidyverse)
library(sindyr)
library(tvR)
library(splus2R)
library(patchwork)
library(broom)
dnx <- function(x) {denoise1(log10(x/max(x)), 1e30)}

# idmat <- datahyp %>%
#   filter(CH == 'C3',
#          #ID == id,
#          # STAGE=='NREM2',
#          CYCLE >= 1) %>%
#   select(E, F, PSD, STAGE_N) %>%
#   arrange(E, F) %>% collect()


figdir <- '/Users/rl422/dyn/figs/'

# Cross-comparison (takes lots of time)
#####
# r -- regularized
# dr -- differentiated regularized
L=1000
Btib <- NULL
loes <- function(x) {
  lowess(x)[['y']]
}

sindify <- function(Hall) {
  Hall %>% 
    group_by(ID, component) %>%
  #filter(length(E) >=50) %>%
  pivot_wider(names_from='component') %>%
  group_by(ID) %>%
  arrange(ID,E) %>%
  
  #select(-V1,-V6) %>%
  drop_na() %>%
  #mutate(across(starts_with('V'), ~pow10(.x), .names='{col}')) #%>%
  mutate(across(starts_with('V'), ~tv(.x, 100), .names="r{col}")) %>%
    # replace argument with 'delta' component (rV3 in 6-component, rV2 in 4-component)
  #mutate(peak=splus2R::peaks(rV2, span=60)) %>%
  #filter(row_number()<= first(which(peak==TRUE))) %>%
  
  #filter(n()>30) %>%
  #mutate(percent_bin = cut((E-min(E))/(max(E)-min(E)), 15)) %>%
  #group_by(ID,percent_bin) %>%
  #summarise(across(starts_with('V'), mean, .names='{col}')) %>%
    #switch back to {col}
  #mutate(across(starts_with('V'), exp, .names='{col}')) %>%
  mutate(E = normalize(E)) %>%
  mutate(across(starts_with('V'), normalize, .names='{col}')) %>%
  mutate(across(starts_with('V'), ~tv(.x,100000), .names='r{col}')) %>%
 #normalization hack (should be able to use across())
 #  mutate(rsum =
 #           rV1+
 #           rV2+
 #           rV3+
 #           rV4#+
 #           #rV5+
 #           #rV6
 #   ) %>%
 #  mutate(rV1=rV1/rsum,
 #         rV2=rV2/rsum,
 #         rV3=rV3/rsum,
 #         rV4=rV4/rsum#,
 #         #rV5=rV5/rsum,
 #         #rV6=rV6/rsum
 # ) %>%
 #    mutate(V1=V1/rsum,
 #           V2=V2/rsum,
 #           V3=V3/rsum,
 #           V4=V4/rsum#,
 #           #V5=V5/rsum,
 #           #V6=V6/rsum
 #           ) %>%
 # select(-rsum) %>%
  mutate(across(starts_with('r'), diffx, .names='d{col}'))
}

  
run_sindy_all <- function(H_sindy, W){
  IDs <- unique(H_sindy$ID)
  B <- list()
  eigs <- list()
  r_pl <- list()
  H_plot <- pivot_longer(H_sindy, contains('V'), names_to = 'V', values_to = 'value') 
  for (id in IDs){
    print(id)
    H_pl <- H_plot %>%
      filter(ID==id)
    W_pl <- filter(refit$W, ID==id)
    # for plotting: r is regularized
    r_pl[[id]] <- filter(H_pl, str_locate(V, 'r')==1)
    # d is differentiated
    d_pl <- filter(H_pl, str_locate(V, 'd')==1)
    # s -- signal
    s_pl <- filter(H_pl, str_locate(V, 'V')==1)
    
    
    
    mat_sindy <- filter(H_sindy, ID==id) %>% ungroup() %>%
      select(starts_with('r')) %>%
      as.matrix()
    pts <- as.matrix(select(filter(H_sindy, ID==id), starts_with('d')))
    #mat_sindy <- apply(mat_sindy, 2, normalize)
    print(mat_sindy)
    feat <- features(mat_sindy, polyorder=1, intercept=FALSE)
    sindy_fit <- sindy(mat_sindy,
                       lambda=0.0,
                       dx=apply(mat_sindy, 2, diffx),
                       verbose=TRUE,
                       Theta=feat,
                       dt = 1/(nrow(mat_sindy)),
                       fit.its=5000)
    print(sindy_fit$B)
    B[[id]] <- sindy_fit$B
    reconst <- as.matrix(sindy_fit$Theta) %*% as.matrix(sindy_fit$B)
    reconst <- as.tibble(reconst)
    
    reconst <- pivot_longer(reconst, everything(), names_to = 'V')
    d_pl$reconst <- reconst$value
    ps <- ggplot(s_pl, aes(x = E,
                           y=value,
                           color=V))+
      geom_line()+
      geom_line(data=r_pl[[id]], aes(x=E,
                                     y=value,
                                     group=V), color='grey')+
      facet_wrap(~V, nrow=2)+ scale_color_brewer(palette='Set1')
    pr <- ggplot(r_pl[[id]], aes(x = E,
                                 y=value,
                                 color=V))+
      geom_line() + scale_color_brewer(palette='Set1')
    pd <- ggplot(d_pl, aes(x = as.numeric(row.names(d_pl)),
                           y=value,
                           color=V))+
      geom_line()+ scale_color_brewer(palette='Set1')
    pre <- ggplot(d_pl, aes(x=E,
                            y=reconst,
                            color=V))+
      geom_line()+ scale_color_brewer(palette='Set1')
    pw <- ggplot(W_pl, aes(x=FR, y=value, color=component))+
      geom_line()+ scale_color_brewer(palette='Set1')
    #gc()
    #plot(pw)
    # diagnostic plot
    plot(pw/ps/(pd+pre)) 
    ggsave(paste(figdir, id, 'rem6_cfs.pdf',sep=''))
    #eigs[[id]] <- eig(as.matrix(sindy_fit$B))
    Bt <- as.tibble(B[[id]])
    Bt$ID <- id
    Bt$RHS <- rownames(B[[id]])
    Bt <- Bt %>% pivot_longer(c(-ID, -RHS), names_to='LHS')
    Btib <- bind_rows(Btib, Bt)
  }
list(H = H_plot, W = refit$W, B = Btib)
}
# compare baseline to followup
compare_sessions <- function(df) {
  df %>% pivot_wider(names_from=session, values_from=value)
}





# plot rescaled
ggplot(filter(H_plot %>% mutate(V = recode(V, V1='I', V2='II', V3='III')), str_detect(V, 'I'), 
              !str_detect(V,'rV')), 
       aes(x=E,y=value, color=V))+
  stat_summary_bin(bins=70)+scale_color_brewer(palette='Set1')+
  geom_smooth()+
  geom_vline(xintercept=5/7)
plot_W_all(refit$W %>% 
             mutate(component = recode(component,
                                       V1 = "I",
                                       V2 = "II",
                                       V3 = "III"))) + scale_color_brewer(palette='Set1')

Bout <- Btib %>% 
  #filter(abs(value) < 0.3) %>% 
  group_by(RHS, LHS) %>%
  mutate(LHS = str_replace(LHS, 'r','d')) %>%
  split_id()



ggplot(Bout, aes(x=outliers(value), fill=session))+
  geom_histogram(position='identity', alpha=0.5)+facet_grid(LHS~RHS)+
  geom_vline(aes(xintercept=0), color='red', linetype='dashed')


# test for significant difference from zero
wtest <- Bout %>% group_by(LHS, RHS, session) %>%
  group_modify(~tidy(wilcox.test(outliers(.$value))))

# "manhattan plots"
ggplot(wtest, aes(x=RHS, y=-log10(p.value), fill=RHS, color=RHS,alpha=session))+
  geom_col(position='dodge')+
  facet_wrap(~LHS, scales='free')+
  geom_hline(aes(yintercept=-log10(0.05/18)), color='black', linetype='dashed')+
  theme_minimal()+
  scale_fill_brewer(palette='Set1')+scale_color_brewer(palette='Set1')

normed <- Bout %>% group_by(nsrrid, session) %>%
  mutate(value=value/sqrt(sum(value^2)))

# pairwise night
bf <- normed %>% #filter(abs(value)<0.25) %>%
  compare_sessions() %>%
  mutate(baseline = outliers(outliers(baseline))) %>%
  mutate(followup = outliers(outliers(followup)))

night_test <- bf %>% group_by(LHS, RHS) %>%
  group_modify(~tidy(cor.test(.$baseline,
                              .$followup)))

bw <- normed %>% #filter(abs(value)<0.25) %>% 
  group_by(LHS,RHS) %>%
  #filter(abs(value) < 0.3) %>%
  #mutate(value=outliers(value)) %>%
  pivot_wider(names_from=c(LHS, RHS), values_from=value)


#demo <- load_demo_cfs()
wdemo <- left_join(bw, demo) %>% 
  ungroup() %>% 
  select(-session, -study) %>%
  drop_na(starts_with('dV'))
colnames(wdemo) <- gsub(":",'',colnames(wdemo))
source('~/dyn/src/dynamics/regress.R')

#ggplot(wdemo_groupH, aes(x=slh5j_ba, y=V3_V4))+ggbeeswarm::geom_beeswarm()
