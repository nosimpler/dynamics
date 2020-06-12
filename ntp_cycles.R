# code for NTP cycle
library(patchwork)

ntp1 <- dataset %>%
  select(ID,E,B,CH,PSD,COND, STAGE,STAGE_N,CYCLE, CYCLE_POS_REL) %>%
  group_by(CH, B, COND, ID) %>%
  filter(B %in% c('BETA', 'SIGMA', 'DELTA')) %>% 
  arrange(E) %>%
  filter(row_number()>=first(which(CYCLE >= 1))) %>%
  mutate(SLEEP = factor(STAGE,
                        levels=c('wake',
                        'NREM1', 'NREM2', 'NREM3','NREM4'),
                        labels=c(0,1,1,1,1))) %>%
  mutate(PSD = (PSD-min(PSD))/(max(PSD)-min(PSD))) %>%
  #drop_na(PSD) %>%
  mutate(smoothed=tvr(PSD))




# for (id in head(unique(ntp1$ID), n=5)){
#   data <- filter(ntp1,ID==id, B=='DELTA', CH=='C3')
#   p1 <- ggplot(data, aes(x=E,y=log(smoothed)))+
#     #stat_summary_bin(bins=80, size=0.1, shape=1)+
#     geom_point(color='black', size=0.1, shape=1)+
#     #stat_smooth(method='gam')+
#     geom_point(aes(x=E, 
#                    y=log(PSD),
#                    color=as.factor(CYCLE), 
#                    alpha=as.factor(SLEEP),
#                    shape=as.factor(STAGE)))+
#     facet_wrap(~COND, nrow=2)+
#     stat_valleys(span=20, geom='vline', linetype='dashed')+
#     scale_color_brewer(palette='Set1', na.value='black')+
#     theme_minimal()
#   plot(p1)
# }

# for (id in head(unique(ntp1$ID), n=20)){
#   data <- filter(ntp1,ID==id, CH=='C3', COND=='baseline', B=='DELTA')
#   p1 <- ggplot(data, aes(x=E,y=log(smoothed)))+
#     #stat_summary_bin(bins=80, size=0.1, shape=1)+
# 
#     #stat_smooth(method='gam')+
#     geom_point(aes(x=E,
#                    y=log(PSD),
#                    color=as.factor(CYCLE),
#                    alpha=as.factor(SLEEP),
#                    shape=as.factor(STAGE)))+
#     geom_point(color='black', size=0.1, shape=1)+
#     facet_wrap(~B, nrow=3)+
#     stat_valleys(span=60, geom='vline', linetype='dashed')+
#     scale_color_brewer(palette='Set1', na.value='black')+
#     theme_minimal()
#   p2 <- ggplot(data, aes(x=E, y=STAGE_N))+geom_line()+theme_minimal()
#   plot(p1/p2+plot_layout(heights=c(8,2)))
# }

# get first delta episode
data1 <- ntp1 %>% 
  mutate(peaks=peaks(-smoothed, span=60)) %>%
  drop_na() %>%
  filter(CYCLE == 1) %>%
  group_by(ID,COND,B,CH) %>%
  arrange(E) %>%
  filter(row_number()<= first(which(peaks==TRUE))) %>%
  mutate(Eshift = E-min(E)+1) %>%
  mutate(Epercent = Eshift/(max(Eshift))*100) %>%
  mutate(dPdt = c(diff(smoothed)[1],diff(smoothed))) %>%
  ungroup() %>%
  mutate(norm=dPdt/max(dPdt))


data <- data1 %>% filter(CH=='C3', COND=='followup')
ggplot(filter(data, B=='BETA'), aes(x=Epercent, y=dPdt))+
  geom_point(size = 0.1)+
  facet_wrap(~CYCLE, scales='free')
ggplot(filter(data, B=='SIGMA'), aes(x=Epercent, y=dPdt))+
  geom_point(size = 0.1)+
  facet_wrap(~CYCLE, scales='free')
ggplot(filter(data, B=='DELTA'), aes(x=Epercent, y=dPdt))+
  geom_point(size = 0.1)+
  facet_wrap(~CYCLE, scales='free')
ggplot(data, aes(x=Epercent, y=norm))+geom_point(size=1, alpha=0.05)+
  facet_wrap(~B, nrow=3)+
  geom_smooth(size=0.5)+
  ylim(-1, 1)

ggplot(data, aes(x=Epercent, y=dPdt))+geom_point(size=0.1, alpha=0.05)+
  facet_wrap(~B, nrow=3)+
  geom_smooth(size=0.5)+
  ylim(-0.025, 0.025)

# n normalized time bins for comparisons/NMF
cut_by <- c(0,seq(20)*5)
data2 <- data1 %>% 
  mutate(bin=cut(Epercent, breaks=cut_by)) %>%
  group_by(bin, ID,COND,B,CH) %>%
  summarize(mean_der=mean(dPdt), mean_sm=mean(smoothed), mean=mean(PSD))

ggplot(filter(data2,CH=='C3', COND=='followup'), aes(x=bin, y=mean))+
  stat_summary()+
  facet_wrap(~B, nrow=3)

make_tensor <- function(df, var){
  dfa <- array(data = df[[var]], 
               dim=c(length(unique(df$bin)),
                     length(unique(df$B)), 
                     length(unique(df$CH)),
                     length(unique(df$ID))),
               dimnames=list(unique(df$bin), unique(df$B), unique(df$CH), unique(df$ID)))
  as.tensor(dfa)
}

make_matrix <- function(df){
  
}

# one channel, one band

blna <- drop_na(filter(data2, COND=='followup', B=='BETA', CH=='C3'))
b <- blna %>% ungroup() %>% select(ID, bin, mean_der) %>%
  pivot_wider(names_from = bin, values_from = mean_der) %>%
  drop_na()
b2 <- b[,-1]
b2 <- matrix(as.numeric(unlist(b2)),nrow=nrow(b2))
rownames(b2) <- b[,1]

nm <- prcomp(b2,3)
IDlist <- unique(b$ID)
tens <- make_tensor(blna, 'mean_der')



canfac <- cp(tens,3, max_iter=1000,tol=1e-16)

idcomp <- tibble(nsrrid=as.numeric(IDlist), 
                 factor1 = canfac$U[[4]][,1], 
                 factor2 = canfac$U[[4]][,2], 
                 factor3 = canfac$U[[4]][,3]
                 )

canfac_demo <- left_join(idcomp, demo)
#canfac_demo$mp <- mp$U[[1]]

p1 <- qplot(canfac_demo$factor1)
p2 <- qplot(canfac_demo$factor2)
p3 <- qplot(canfac_demo$factor3)
p1/p2/p3
p4 <- qplot(1:20,canfac$U[[2]][,1])
p5 <- qplot(1:20,canfac$U[[2]][,2])
p6 <- qplot(1:20,canfac$U[[2]][,3])
p4/p5/p6



nm <- blna %>% ungroup() %>% select(ID, bin, mean) %>%
  pivot_wider(names_from = bin, values_from = mean) %>%
  drop_na()
IDlist_nm <- unique(nm$ID)
nm2 <- nm[,-1]
nm2 <- matrix(as.numeric(unlist(nm2)),nrow=nrow(nm2))


nmffit <- nmf(nm2,2, method='lee')
print(nmffit@residuals)
plot(nmffit@fit@H[1,], type='l', col='red')
lines(nmffit@fit@H[2,], col='green')
lines(nmffit@fit@H[3,], col='blue')
lines(nmffit@fit@H[4,], col='orange')
lines(nmffit@fit@H[5,], col='violet')

w <- nmffit@fit@W %>% as.tibble()
w$nsrrid <- as.numeric(IDlist_nm)
wdemo <- left_join(w, demo)
# use nnTensor library to make nonnegative tensor for mean
tens_nonneg <- make_tensor(drop_na(filter(data2, 
                                          B=='DELTA',
                                          CH=='C3',
                                          COND=='baseline')), 'norm')
IDlist_nonneg <- unique(drop_na(filter(data2, COND=='baseline'))$ID)
nntf <- NTF(tens_nonneg, rank=4, algorithm='KL',init='NMF', num.iter=1000, verbose=TRUE, thr=1e-30)
idcomp_nonneg <- tibble(nsrrid=as.numeric(IDlist_nonneg), 
                 factor1 = nntf$A[[1]][1,], 
                 factor2 = nntf$A[[1]][2,], 
                 factor3 = nntf$A[[1]][3,],
                 factor4 = nntf$A[[1]][4,]
)
nonneg_demo <- left_join(idcomp_nonneg, demo)
p1 <- qplot(log(nonneg_demo$factor1))
p2 <- qplot(log(nonneg_demo$factor2))
p3 <- qplot(log(nonneg_demo$factor3))
p8 <- qplot(log(nonneg_demo$factor4))
p1/p2/p3/p8
p4 <- qplot(1:20,nntf$A[[2]][1,])
p5 <- qplot(1:20,nntf$A[[2]][2,])
p6<- qplot(1:20,nntf$A[[2]][3,])
p7<- qplot(1:20,nntf$A[[2]][4,])
p4/p5/p6/p7
