# take population nmf factors and do nnls
# one individual to start
# uses Wgroup from nmf_all.R

library(nnls)
library(broom)
idmat <- datahyp %>%
  filter(CH == 'C3',
         ID == 'chat-followup-301060',
         # STAGE=='NREM2',
         CYCLE >= 1) %>%
  select(E, F, PSD) %>% collect()
epochs <- unique(idmat$E)
nn <- idmat %>% group_by(E) %>%
  group_map(~nnls(w0,log(.$PSD / min(.$PSD))))
nn_betas <- tibble(V1=as.numeric(),
                   V2=as.numeric(),
                   V3=as.numeric())
for (i in seq(length(nn))){
  nn_betas <- bind_rows(nn_betas, c(V1=nn[[i]]$x[1], 
            V2=nn[[i]]$x[2], 
            V3=nn[[i]]$x[3]))
}
nn_betas$E <- epochs
beta_pl <- pivot_longer(nn_betas, starts_with('V'), names_to = 'component')

ggplot(beta_pl, aes(x=E,y=value, color=component)) + geom_point()


# compare to original fit
idmat <- idmat %>% pivot_wider(names_from = E, values_from = PSD)

# remove F column
# nmf initial conditions
nH <- ncol(idmat)-1
nW <- nrow(idmat)
w0 <- cbind(rep(1.0, nW), rep(1.0, nW), rep(1.0, nW))
w0[1, 1] <- 10
w0[2, 2] <- 10
w0[3, 3] <- 10

h0 <- rbind(rep(1.0, nH), rep(1.0, nH), rep(1.0, nH))


sansf <- idmat[, -1]
nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
init <- nmfModel(3, nmat, W = Wgroup, H = h0)
#nmffit <- nmf(log(nmat/min(nmat)), 3, method='lee', seed='nndsvd')
nmffit <- nmf(log(nmat / min(nmat)), 3, method = 'lee', seed = init)
print(nmffit@residuals)
resultsH <- as.tibble(t(nmffit@fit@H))
resultsW <- as.tibble(nmffit@fit@W)
resultsH$E <- epochs
resultsW$FR <- seq(0,45)
nn_plH <- pivot_longer(resultsH, starts_with('V'), names_to = 'component')
nn_plW <- pivot_longer(resultsW, starts_with('V'), names_to = 'component')
ggplot(nn_plH, aes(x=E,y=value, color=component)) + geom_point()
ggplot(nn_plW, aes(x=FR,y=value, color=component)) + geom_point()