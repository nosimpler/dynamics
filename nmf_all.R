
library(NMF)
library(tidyverse)
# select ID
library(patchwork)

figdir <- '/Users/rl422/dyn/figs/'




# first pass through nmf
nmf1 <- function(data, 
                 n_components=6, 
                 compare_random_seed=FALSE){

  
  Hall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 E=as.numeric(),
                 #STAGE_N=as.numeric()
                 )
  Wall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 FR=as.numeric())
  
  IDlist <- distinct(select(data, ID)) %>% collect() %>% arrange()
  print(IDlist)
  err <- list()
  #err_random_seed <- list()
  # do nmf for each individual
  

  
  for (id in IDlist$ID) {
    print(id)
    idmat <- data %>%
      filter(ID == id
          # CYCLE >= 1 #comment this out to include waking periods
          ) %>%
      #rename(PSD=MTM) %>%
      #select(E, F, PSD, STAGE_N) %>%
      select(E,F,PSD) %>%
      arrange(E, F) %>% collect()
  epochs <- unique(idmat$E)
  fr <- unique(idmat$F)
  #stage_n <- idmat %>% filter(F==min(F)) %>% select(STAGE_N)
  idmat <- idmat %>% 
    ungroup() %>% #changed
    select(E,F,PSD) %>%
    pivot_wider(id_cols = F, names_from = E, values_from = PSD)

  

  # nmf initial conditions
  nH <- ncol(idmat)
  nW <- nrow(idmat)
  print(nW)
  print(nH)
  #w0 <- cbind(rep(1.0, nW), rep(1.0, nW), rep(1.0, nW))
  #w0[1, 1] <- 10
  #w0[12, 2] <- 10
  #w0[25, 3] <- 10
  
  h0 <- rep(1.0,nH)
  w0 <- rep(1.0,nW)
  for (i in seq(n_components-1)){
    h0 <- rbind(h0, rep(1.0, nH))
    w0 <- cbind(w0, rep(1.0,nW))
    w0[i,i] <- 2.0
  }
  colnames(h0) <- NULL
  colnames(w0) <- NULL
  
  # drop frequency column
  sansf <- idmat[, -1]
  nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
  
  init <- nmfModel(n_components, nmat, W = w0, H = h0)
  
  nmffit <- nmf(log(nmat / min(nmat)), n_components, method = 'lee', seed = init)
  if (compare_random_seed == TRUE){
    nmffit_random <- nmf(log(nmat / min(nmat)), n_components, method = 'lee')
    err[[id]]$random <- nmffit_random@residuals
  }
  #nmffit <- nmf(nmat, n_components, method = 'lee', seed = init)
  err[[id]]$fixed <- nmffit@residuals
  resultsH <- as.tibble(t(nmffit@fit@H))
  resultsW <- as.tibble(nmffit@fit@W)
  resultsH$E <- epochs
  resultsW$FR <- fr
  resultsW$ID <- id
  resultsH$ID <- id
  #resultsH$STAGE_N <- stage_n$STAGE_N
  
  #rename hack
  resultsW <- resultsW %>% 
    rename(V1='h0') %>% 
    pivot_longer(starts_with('V'), 
                          names_to='component', 
                          values_to = 'value')
  
  Wall <- add_row(Wall, resultsW)
  
  resultsH <- resultsH %>% 
    rename(V1='h0') %>% 
    pivot_longer(starts_with('V'), 
                                        names_to='component', 
                                        values_to = 'value')
  Hall <- add_row(Hall, resultsH)
  graphics.off()
  p1 <- ggplot(resultsW, aes(x=FR,y=value,color=component))+
    geom_line() + scale_color_brewer(palette='Set1')
 
  p2 <- ggplot(resultsH, aes(x=E,y=value,color=component))+
    geom_line() + scale_color_brewer(palette='Set1')
  plot(p1/p2)
  ggsave(paste(figdir, id, '100.pdf',sep=''))
  }
  list(H=Hall, W=Wall, err=err)
  }


######
# now do nmf on FR x (ID x V) for class membership
group_nmf <- function(nmf_fits, 
                      n_components=6, 
                      compare_random_seed=FALSE,
                      plot = FALSE){
  err <- list()
  Y_pop <-nmf_fits$W %>%
    pivot_wider(names_from=c(ID,component), values_from=value)




  sansf <- select(Y_pop, -FR)
  Y <-matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
  print(Y)
  nW <- nrow(Y)
  nH <- ncol(Y)
  print(nH)
  
  h0 <- rep(1.0,nH)
  w0 <- rep(1.0,nW)
  #w0 <- Wgroup
  for (i in seq(n_components-1)){
    h0 <- rbind(h0, rep(1.0, nH))
    w0 <- cbind(w0, rep(1.0,nW))
    w0[i,i] <- 2
  }
  
  init <- nmfModel(n_components,Y, W = w0, H = h0)
  nmffit <- nmf(Y, n_components, method = 'lee', seed = init)
  if (compare_random_seed == TRUE){
    nmffit_random <- nmf(Y, n_components)
    err$random = nmffit_random@residuals
  }
  Hgroup <- nmffit@fit@H
  Wgroup <- nmffit@fit@W
  err$fixed <- nmffit@residuals
  colnames(Hgroup) <- colnames(sansf)

# convert to more readable format
  Hpop <- as_tibble(t(Hgroup)) %>% rename(V1 = w0)
  Hpop$IDvec <- rownames(t(Hgroup))
  Hpop <- separate(Hpop, IDvec, 
                 into=c('study','condition','ID','component'))

  matplot(Wgroup)
  pairs(t(Hgroup))
  groupfit <- list()
  groupfit$W <- Wgroup
  groupfit$H <- Hpop
  groupfit$err <- err
  groupfit
}

nmf2 <- function(data, 
                 Wgroup, 
                 n_components=6, 
                 compare_random_seed=FALSE,
                 plot = FALSE){
  

  print(data)
  Hall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 E=as.numeric(),
                 STAGE_N=as.numeric())
  Wall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 FR=as.numeric())
  
  IDlist <- distinct(select(data, ID)) %>% collect()
  err <- list()
  # do nmf for each individual
  for (id in IDlist$ID) {
    print(id)
    idmat <- data %>%
      #filter(str_detect(ID, 'baseline')) %>%
      filter(ID == id,
             # STAGE=='NREM2',
            # CYCLE >= 1
      ) %>%
      select(E, F, PSD) %>%
      arrange(E, F) %>% collect()
    if (nrow(idmat)==0) next
    epochs <- unique(idmat$E)
    fr <- unique(idmat$F)
    #stage_n <- idmat %>% filter(F==min(F)) %>% select(STAGE_N)
    idmat <- idmat %>% 
      ungroup() %>% #changed
      select(E,F,PSD) %>%
      pivot_wider(names_from = E, values_from = PSD)
    
    # remove F column
    # nmf initial conditions
    nH <- ncol(idmat)-1
    nW <- nrow(idmat)
    #w0 <- cbind(rep(1.0, nW), rep(1.0, nW), rep(1.0, nW))
    #w0[1, 1] <- 10
    #w0[12, 2] <- 10
    #w0[25, 3] <- 10
    
    h0 <- rep(1.0,nH)
    w0 <- rep(1.0,nW)
    for (i in seq(n_components-1)){
      h0 <- rbind(h0, rep(1.0, nH))
      w0 <- cbind(w0, rep(1.0,nW))
      w0[i,i] <- 2
    }
    colnames(h0) <- NULL
    colnames(w0) <- NULL
    w0 <- Wgroup
    sansf <- idmat[, -1]
    nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
    init <- nmfModel(n_components, nmat, W = w0, H = h0)
    nmffit <- nmf(log(nmat / min(nmat)), n_components, method = 'lee', seed = init)
    
    if (compare_random_seed == TRUE){
      nmffit_random <- nmf(log(nmat / min(nmat)), n_components, method = 'lee')
      err[[id]]$random <- nmffit_random@residuals
    }
    
    err[[id]]$fixed <- nmffit@residuals
    resultsH <- as.tibble(t(nmffit@fit@H))
    resultsW <- as.tibble(nmffit@fit@W)
    resultsH$E <- epochs
    resultsW$FR <- fr
    resultsW$ID <- id
    resultsH$ID <- id
    #resultsH$STAGE_N <- stage_n$STAGE_N
    
    #rename hack
    resultsW <- resultsW %>% 
      rename(V1='w0') %>% 
      pivot_longer(starts_with('V'), 
                   names_to='component', 
                   values_to = 'value')
    
    Wall <- add_row(Wall, resultsW)
    
    resultsH <- resultsH %>% 
      rename(V1='w0') %>% 
      pivot_longer(starts_with('V'), 
                   names_to='component', 
                   values_to = 'value')
    Hall <- add_row(Hall, resultsH)
    if (plot==TRUE){graphics.off()
    p1 <- ggplot(resultsW, aes(x=FR,y=value,color=component))+
      geom_line()
    
    p2 <- ggplot(resultsH, aes(x=E,y=value,color=component))+
      geom_line()
    plot(p1/p2)
    }
    #ggsave(paste(figdir, id, '_withwake.pdf',sep=''))
  }
  list(H=Hall, W=Wall, err=err)
}

nmf_indiv <- function(datahyp, id, n_components=6){
  
    print(id)
    idmat <- datahyp %>%
      filter(ID == id,
             # STAGE=='NREM2',
             #CYCLE >= 1
      ) %>%
      #rename(PSD=MTM) %>%
      select(E, F, PSD, STAGE_N) %>%
      arrange(E, F) %>% collect()
    epochs <- unique(idmat$E)
    fr <- unique(idmat$F)
    stage_n <- idmat %>% filter(F==min(F)) %>% select(STAGE_N)
    idmat <- idmat %>% 
      select(E,F,PSD) %>%
      pivot_wider(names_from = E, values_from = PSD)
    
    # remove F column
    # nmf initial conditions
    nH <- ncol(idmat)-1
    nW <- nrow(idmat)
    #w0 <- cbind(rep(1.0, nW), rep(1.0, nW), rep(1.0, nW))
    #w0[1, 1] <- 10
    #w0[12, 2] <- 10
    #w0[25, 3] <- 10
    
    h0 <- rep(1.0,nH)
    w0 <- rep(1.0,nW)
    for (i in seq(n_components-1)){
      h0 <- rbind(h0, rep(1.0, nH))
      w0 <- cbind(w0, rep(1.0,nW))
      w0[i,i] <- 2
    }
    h <- as_tibble(t(h0))
    w <- as_tibble(w0)
    h$E <- seq(1,nH)
    w$FR <- seq(0,45)
    h <- pivot_longer(rename(h, V1=h0), 
                      starts_with('V'),
                      names_to = 'component')
    w <- pivot_longer(rename(w, V1=w0), 
                      starts_with('V'),
                      names_to = 'component')
    
    p1 <- ggplot(h, aes(x=E, y=value, color=component))+
      geom_path()+scale_color_brewer(palette='Set1')
    p2 <- ggplot(w, aes(x=FR, y=value, color=component))+
      geom_path()+scale_color_brewer(palette='Set1')
    
    p2/p1
    
    colnames(h0) <- NULL
    colnames(w0) <- NULL
    #w0 <- Wgroup
    sansf <- idmat[, -1]
    nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
    init <- nmfModel(n_components, nmat, W = w0, H = h0)
    #eps <- .Machine$double.eps^2
    print(min(nmat))
    #nmffit <- nmf(log(nmat / eps), n_components, method = 'lee', seed = init)
    nmffit <- nmf(log(nmat /min(nmat)), 
                  n_components, 
                  method = 'lee', 
                  seed = init)
    err <- nmffit@residuals
    resultsH <- as.tibble(t(nmffit@fit@H))
    resultsW <- as.tibble(nmffit@fit@W)
    resultsH$E <- epochs
    resultsW$FR <- fr
    resultsW$ID <- id
    resultsH$ID <- id
    resultsH$STAGE_N <- stage_n$STAGE_N
    
    #rename hack
    resultsW <- resultsW %>% 
      rename(V1='h0') %>% 
      pivot_longer(starts_with('V'), 
                   names_to='component', 
                   values_to = 'value')

    resultsH <- resultsH %>% 
      rename(V1='h0') %>% 
      pivot_longer(starts_with('V'), 
                   names_to='component', 
                   values_to = 'value')
    orig <- idmat %>% pivot_longer(-F, names_to='E') %>% 
      mutate(FR=replace(F, F ==0, -0.5)) %>%
      select(-F) %>%
      mutate(E = as.numeric(E))
    results <- list(W = resultsW, H = resultsH, err = err, orig=orig)
}

unfold_err <- function(err){
  error_diff <- c()
  random_err <- 0
  fixed_err <- 0
  for (i in err){
    print(i)
    error_diff = rbind(error_diff, i$random - i$fixed)
    random_err <- random_err + i$random
    fixed_err <- fixed_err + i$fixed
  }
  print(paste('mean random seed error', random_err/nrow(error_diff))) # mean random-seed error
  print(paste('mean fixed seed error', fixed_err/nrow(error_diff))) # mean fixed-seed error
  tibble(ID=names(err), err_diff=error_diff)
}


n_components <- 6
# 
# #mouse-specific
#  data <- psd %>% 
#    group_by(ID,E) %>% 
#    filter(sum(PSD)>0) %>% 
#    filter(CH == 'EEG_EEG1A-B') %>%
#    left_join(hypno) %>%
#    filter(STAGE_N == -2) %>%
#    ungroup()
# 
# nmf_fit <- nmf1(data %>% filter(ID != "38152_DarkCycle"), 
#                 n_components=n_components, 
#                 compare_random_seed = TRUE)
# groupfit <- group_nmf(nmf_fit,
#                       n_components=n_components,
#                       compare_random_seed=TRUE)
# refit <- nmf2(data %>% filter(ID != "38152_DarkCycle"), groupfit$W,
#              n_components=n_components,
#              compare_random_seed = TRUE)
# save.image(file = 'mouse-6-NREM.RData')


# data <- datahyp %>% 
#   filter(CH == 'C3') %>%
#   filter(STAGE_N < 0) %>%
#   ungroup()
# 
# nmf_fit <- nmf1(data, 
#                 n_components=n_components, 
#                 compare_random_seed = TRUE)
# groupfit <- group_nmf(nmf_fit,
#                       n_components=n_components,
#                       compare_random_seed=TRUE)
# refit <- nmf2(data, groupfit$W,
#               n_components=n_components,
#               compare_random_seed = TRUE)
# save.image(file = 'chat-6-NREM.RData')

# mros
# nmf_fit <- nmf1(psd, 
#                 n_components=n_components, 
#                 compare_random_seed = FALSE)
# groupfit <- group_nmf(nmf_fit,
#                       n_components=n_components,
#                       compare_random_seed=FALSE)
refit <- nmf2(psd, groupfit$W,
              n_components=n_components,
              compare_random_seed = FALSE)
save.image(file = 'mros-6-NREM2.RData')

#
# err_diff <- unfold_err(nmf_fit$err)
# ggplot(err_diff, aes(x=err_diff))+
#   geom_histogram(bins=100)+
#   xlim(-100,100)
# err_diff_refit <- unfold_err(refit$err)
# ggplot(err_diff_refit, aes(x=err_diff))+
#   geom_histogram(bins=100)+
#   xlim(-1000,1000)
# err_diff %>% filter(err_diff > 0) %>% summarize(n = n())



