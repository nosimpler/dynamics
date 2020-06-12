

ntp1 <- dataset %>%
  select(ID,E,B,CH,PSD,COND, STAGE,STAGE_N,CYCLE, CYCLE_POS_REL) %>%
  group_by(CH, B, COND, ID) %>%
  #filter(B %in% c('BETA', 'SIGMA', 'DELTA')) %>% 
  arrange(E) %>%
  filter(row_number()>=first(which(CYCLE >= 1))) %>%
  mutate(SLEEP = factor(STAGE,
                        levels=c('wake',
                                 'NREM1', 'NREM2', 'NREM3','NREM4'),
                        labels=c(0,1,1,1,1))) %>%
  mutate(PSD = (PSD-min(PSD))/(max(PSD)-min(PSD))) %>%
  #drop_na(PSD) %>%
  mutate(smoothed=tvr(PSD))

data1 <- ntp1 %>% 
  mutate(peaks=splus2R::peaks(-smoothed,span=30)) %>%
  drop_na() %>%
  filter(CYCLE == 1) %>%
  group_by(ID,COND,B,CH) %>%
  arrange(E) %>%
  #filter(row_number()<= nth(which(peaks==TRUE), n=2)) %>%
  mutate(Eshift = E-min(E)+1) %>%
  mutate(Epercent = Eshift/(max(Eshift))*100) %>%
  mutate(dPdt = c(diff(smoothed)[1],diff(smoothed))) %>%
  ungroup() %>%
  mutate(norm=dPdt/max(dPdt))

#### nmf
cut_by <- c(0,seq(10)*10)
data2 <- data1 %>% 
  mutate(bin=cut(Epercent, breaks=cut_by)) %>%
  group_by(bin, ID,COND,B,CH) %>%
  summarize(mean_der=mean(dPdt), mean_sm=mean(smoothed), mean=mean(PSD))


blmn <- drop_na(filter(data2, COND=='followup', B=='GAMMA', CH=='F3')) %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(mean_der_nonneg = (mean_der-min(mean_der))/(max(mean_der)-min(mean_der)),
         mean_norm = (mean-min(mean))/(max(mean)-min(mean)))
nm <- blmn %>% ungroup() %>% select(ID, bin, mean) %>%
  pivot_wider(names_from = bin, values_from = mean) %>%
  drop_na()
IDlist_nm <- unique(nm$ID)
nm2 <- nm[,-1]
nm2 <- matrix(as.numeric(unlist(nm2)),nrow=nrow(nm2))


nmffit <- nmf(nm2,3, method='lee')
print(nmffit@residuals)
plot(nmffit@fit@H[1,], type='l', col='red')
lines(nmffit@fit@H[2,], col='green')
lines(nmffit@fit@H[3,], col='blue')
lines(nmffit@fit@H[4,], col='orange')
lines(nmffit@fit@H[5,], col='violet')

w <- nmffit@fit@W %>% as.tibble()
w$nsrrid <- as.numeric(IDlist_nm)
wdemo <- left_join(w, demo)
for (v in names(wdemo)[startsWith(names(wdemo), 'V')]){
  regress_all(wdemo, v)
}

ggplot(wdemo, aes(x=ageyear_at_meas, y=V1))+stat_summary_bin(color='blue')+geom_point()
