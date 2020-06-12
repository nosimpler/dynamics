# nmf permutations


demographic <- 'pedsql_total_scale_score_par'

# preprocess (see ntp_cycles.R)
nm <- blna %>% ungroup() %>% select(ID, bin, mean) %>%
  pivot_wider(names_from = bin, values_from = mean) %>%
  drop_na()
nm2 <- nm[,-1]
nm2 <- matrix(as.numeric(unlist(nm2)),nrow=nrow(nm2))
IDlist_nm <- unique(nm$ID)
demo2 <- filter(demo, nsrrid %in% IDlist_nm)
#### initialize to demographic warm-start

# first vector is all ones (uniform assumption)
v1 <- ones(length(IDlist_nm),1)
v2 <- demo2[[demographic]]
w0 <- as.matrix(cbind(v1, v2))
# 20 timepoints
h0 <- ones(20,2)
init <- nmfModel(2, nm2, W=w0, H=t(h0))
#### Run NMF on warm start and obtain LSE
res <- nmf(nm2, 2, method='lee', seed=init)
res_actual <- res@residuals
print(res_actual)
plot(res@fit@H[1,])
plot(demo2[[demographic]], log(res@fit@W[,1]))


#### Permute and obtain LSE
res_permute <- zeros(100, 1)
for (i in seq(100)){
  print(i)
  w <- w0[sample(nrow(w0)),]
  init <- nmfModel(2, nm2, W=w, H=t(h0))
  res_permute[i] <- nmf(nm2, 2, method='lee',seed=init)@residuals
  print(res_permute[i])
}
qplot(res_permute)
