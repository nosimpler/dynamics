# ODE fitting
library(tidyverse)
library(sindyr)
library(tvR)
library(splus2R)
library(patchwork)
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
  pivot_wider(names_from='component') %>%
  group_by(ID) %>%
  arrange(ID,E) %>%
  #select(-V1,-V6) %>%
  filter(CYCLE>=1) %>%
    drop_na() %>%
  mutate(across(starts_with('V'), ~tv(.x, 10), .names="r{col}")) %>%
    # replace argument with 'delta' component (rV3 in 6-component, rV2 in 4-component)
  mutate(peak=splus2R::peaks(-rV3, span=30)) %>%
  filter(row_number()<= first(which(peak==TRUE))) %>%

  filter(n()>15) %>%
  #mutate(percent_bin = cut((E-min(E))/(max(E)-min(E)), 15)) %>%
  #group_by(ID,percent_bin) %>%
  #summarise(across(starts_with('V'), mean, .names='{col}')) %>%
    #switch back to {col}
  mutate(across(starts_with('V'), normalize, .names='{col}')) %>%
  mutate(across(starts_with('V'), ~tv(.x,100000), .names='r{col}')) %>%
    #normalization hack (should be able to use across())
  mutate(rsum = 
           rV1+
           rV2+
           rV3+
           rV4+
           rV5+
           rV6
   ) %>%
  mutate(rV1=rV1/rsum,
         rV2=rV2/rsum,
         rV3=rV3/rsum,
         rV4=rV4/rsum,
         rV5=rV5/rsum,
         rV6=rV6/rsum
 ) %>%
    mutate(V1=V1/rsum,
           V2=V2/rsum,
           V3=V3/rsum,
           V4=V4/rsum,
           V5=V5/rsum,
           V6=V6/rsum
           ) %>%
  select(-rsum) %>%
  mutate(across(starts_with('r'), diffx, .names='d{col}'))
}
hypnoH <- left_join(Hall6, hypno, copy=TRUE)
H_sindy <- sindify(hypnoH)
IDs <- unique(H_sindy$ID)
B <- list()
eigs <- list()
r_pl <- list()
for (id in IDs){
  print(id)
  H_pl <- pivot_longer(H_sindy, contains('V'), names_to = 'V') %>%
  filter(ID==id)
  W_pl <- filter(Wall6, ID==id)
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
                       fit.its=5000)
    print(sindy_fit$B)
    B[[id]] <- sindy_fit$B
    reconst <- as.matrix(sindy_fit$Theta) %*% as.matrix(sindy_fit$B)
    reconst <- as.tibble(reconst)

    reconst <- pivot_longer(reconst, everything(), names_to = 'V')
    d_pl$reconst <- reconst$value
    ps <- ggplot(s_pl, aes(x = as.numeric(row.names(s_pl)),
                           y=value,
                           color=V))+
      geom_line()+
      geom_line(data=r_pl[[id]], aes(x=as.numeric(row.names(r_pl[[id]])),
                                  y=value,
                                  group=V), color='grey')
    pr <- ggplot(r_pl[[id]], aes(x = as.numeric(row.names(r_pl[[id]])),
                           y=value,
                           color=V))+
      geom_line()
    pd <- ggplot(d_pl, aes(x = as.numeric(row.names(d_pl)),
                           y=value,
                           color=V))+
      geom_line()
    pre <- ggplot(d_pl, aes(x=as.numeric(row.names(d_pl)),
                               y=reconst,
                               color=V))+
      geom_line()
    pw <- ggplot(W_pl, aes(x=FR, y=value, color=component))+
      geom_line()
    gc()
    #plot(pw)
    plot((ps+pd/pre))
    ggsave(paste(figdir, id, 'cycle2_dynamics.pdf',sep=''))
    eigs[[id]] <- eig(as.matrix(sindy_fit$B))
    Bt <- as.tibble(B[[id]])
    Bt$ID <- id
    Bt$RHS <- rownames(B[[id]])
    Bt <- Bt %>% pivot_longer(c(-ID, -RHS), names_to='LHS')
    Btib <- bind_rows(Btib, Bt)
}


compare_sessions <- function(df) {
  df %>% separate(ID, c('study', 'session', 'nsrrid')) %>%
    pivot_wider(names_from=session, values_from=value)
}


B4 <- Btib
Bz <- Btib %>% group_by(RHS, LHS) %>%
  summarise(zeros = sum(value==0))
Bg <- compare_sessions(filter(Btib, abs(value)<0.2))
ggplot(Bg, aes(x=baseline, y=followup))+
         geom_point()+
         facet_grid(RHS~LHS)
print(Bz)
ggplot(Bz, aes(x=interaction(LHS, RHS), y=zeros))+geom_col()

Bout <- Btib %>% 
  #filter(abs(value) < 0.2) %>% 
  group_by(RHS, LHS)

grp_avg = Btib %>%
  group_by(RHS, LHS) %>%
  summarise(med = median(value), mean=mean(value))

ggplot(Bout, aes(x=outliers(value)))+
  geom_histogram()+facet_grid(LHS~RHS)+
  geom_vline(aes(xintercept=0), color='black', linetype='dashed')+
  geom_vline(data=grp_avg, aes(xintercept=med),color='red')+
  geom_vline(data=grp_avg, aes(xintercept=mean), color='blue')

ggplot(Bout, aes(x=LHS,y=value))+
  ggbeeswarm::geom_beeswarm(size=0.5)+
  facet_wrap(~RHS, nrow=5)

wtest <- Bout %>% group_by(LHS, RHS) %>%
  group_modify(~tidy(wilcox.test(outliers(.$value))))


ggplot(wtest, aes(x=RHS, y=LHS, fill=log10(p.value)))+geom_tile()
ggplot(wtest, aes(x=LHS, y=log10(p.value)))+
  geom_col()+
  facet_wrap(~RHS, scales='free')+
  geom_hline(aes(yintercept=log10(0.05/36)), color='red')

# pairwise night
bf <- compare_sessions(Bout)
night_test <- bf %>% group_by(LHS, RHS) %>%
  group_modify(~tidy(cor.test(outliers(.$baseline), outliers(.$followup))))

bw <- Btib %>% #filter(abs(value)<0.25) %>% 
  group_by(LHS,RHS) %>%
  mutate(value=outliers(value)) %>%
  pivot_wider(names_from=c(LHS, RHS), values_from=value)
bwbase <- bw %>%
  separate(ID, c('study', 'session', 'nsrrid')) %>%
  filter(session=='followup') %>%
  mutate(nsrrid = as.numeric(nsrrid)) %>% drop_na()
wdemo <- left_join(bwbase, demo) %>% ungroup() %>% select(-session, -study)
source('~/dyn/src/dynamics/regress.R')
