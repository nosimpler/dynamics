# nmf for 
library(NMF)
library(patchwork)
library(tidyverse)
# select ID
id <- idlist[3]
condition <- 'followup'
data <- dataset %>% 
  filter(COND==sym(!!condition), CH=='C3', ID==id, CYCLE>=1)



hypno <- hyp %>% filter(COND==sym(!!condition), ID==id, CYCLE>=1)

idmat <- dataset %>% 
  filter(COND==sym(!!condition), 
         CH=='C3', 
         ID==id, CYCLE >=1) %>%
  select(E, F, PSD) %>%
  arrange(E,F) %>%
  pivot_wider(names_from = E, values_from = PSD) %>%
  drop_na()
# remove F column
sansf <- idmat[,-1]
nmat <- matrix(as.numeric(unlist(sansf)),nrow=nrow(sansf))
w0 <- cbind(rep(1.0,46), rep(1.0, 46), rep(1.0, 46))
w0[1,1] <- 10
w0[2,2] <- 10
w0[4,3] <- 10
h0 <- rbind(rep(1.0,987), rep(1.0,987), rep(1.0,987)) 
init <- nmfModel(3, nmat, W=w0, H=h0)
#nmffit <- nmf(nmat, 3, method='lee', seed='nndsvd')
nmffit <- nmf(log(nmat/min(nmat)), 3, method='lee', seed=init)
print(nmffit@residuals)
resultsH <- as.tibble(t(nmffit@fit@H))
resultsH$E <- hypno$E
resultsH$STAGE_N <- hypno$STAGE_N
resultsH$N2_WGT <- hypno$N2_WGT

ggplot(resultsH, 
       aes(x=tvr(V2),
           y=tvr(V3),
           color=as.factor(STAGE_N)))+
  geom_point(size=0.5)
p1 <- ggplot(resultsH, aes(x=V1, 
                     color=as.factor(STAGE_N)))+
  geom_freqpoly()
p2 <- ggplot(resultsH, aes(x=V2, 
                     color=as.factor(STAGE_N)))+
  geom_freqpoly()
p3<- ggplot(resultsH, aes(x=V3, 
                     color=as.factor(STAGE_N)))+
  geom_freqpoly()
p1/p2/p3

resultsH <- resultsH %>% 
  pivot_longer(c(-STAGE_N, -E, -N2_WGT), names_to='COMPONENT') %>%
  group_by(COMPONENT) %>%
  arrange(E) %>%
  mutate(smoothed = predict(loess(log(value)~E, span=0.1)))


resultsW <- as.tibble(nmffit@fit@W)
resultsW$FREQ <- unique(data$F)
resultsW <- resultsW %>% pivot_longer(-FREQ, names_to='COMPONENT')

ggplot(resultsW, aes(x=FREQ, y=value, color=COMPONENT))+geom_path()
p1 <- ggplot(resultsH, aes(x=E, y=value, color=COMPONENT))+
  geom_point(size=0.5)#+geom_path(aes(x=E, y=smoothed))
p2 <- ggplot(resultsH, aes(x=E, y=STAGE_N))+geom_path()
plot(p1/p2+plot_layout(heights=c(8,2)))
#ggplot(resultsH, aes(x=E, y=log(value), color=COMPONENT, shape=STAGE))+geom_point()