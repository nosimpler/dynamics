
library(NMF)
library(tidyverse)
# select ID
library(patchwork)

figdir <- '/Users/rl422/dyn/figs/'




# first pass through nmf
nmf1 <- function(data, n_components=6, compare_random_seed=FALSE){

  
  Hall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 E=as.numeric(),
                 STAGE_N=as.numeric())
  Wall <- tibble(component=as.character(),
                 value=as.numeric(),
                 ID=as.character(),
                 FR=as.numeric())
  
  IDlist <- distinct(select(data, ID)) %>% collect() %>% arrange()
  err <- list()
  err_random_seed <- list()
  # do nmf for each individual
  for (id in IDlist$ID) {
    print(id)
    idmat <- data %>%
      filter(CH == 'C3',
           ID == id,
          # STAGE=='NREM2',
           CYCLE >= 1 #comment this out to include waking periods
          ) %>%
      #rename(PSD=MTM) %>%
      #select(E, F, PSD, STAGE_N) %>%
      select(E,F,PSD) %>%
      arrange(E, F) %>% collect()
  epochs <- unique(idmat$E)
  fr <- unique(idmat$F)
  #stage_n <- idmat %>% filter(F==min(F)) %>% select(STAGE_N)
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
    w0[i,i] <- 2.0
  }
  colnames(h0) <- NULL
  colnames(w0) <- NULL
  #w0 <- Wgroup
  sansf <- idmat[, -1]
  nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
  init <- nmfModel(n_components, nmat, W = w0, H = h0)

  nmffit <- nmf(log(nmat / min(nmat)), n_components, method = 'lee', seed = init)
  if (compare_random_seed == TRUE){
    nmffit_random <- nmf(log(nmat / min(nmat)), n_components, method = 'lee')
    err_random[[id]] <- nmffit_random@residuals
  }
  #nmffit <- nmf(nmat, n_components, method = 'lee', seed = init)
  err[[id]] <- nmffit@residuals
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
group_nmf <- function(nmf_fits, n_components=6){

  Y_pop <-nmf_fits$W %>%
    pivot_wider(names_from=c(ID,component), values_from=value)

  nW <- length(unique(nmf_fits$W$FR))
  nH <- length(unique(nmf_fits$H$ID))*n_components


  h0 <- rep(1.0,nH)
  w0 <- rep(1.0,nW)
  #w0 <- Wgroup
  for (i in seq(n_components-1)){
    h0 <- rbind(h0, rep(1.0, nH))
    w0 <- cbind(w0, rep(1.0,nW))
    w0[i,i] <- 2
  }


  sansf <- select(Y_pop, -FR)
  Y <-matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
  init <- nmfModel(n_components,Y, W = w0, H = h0)
  #nmffit <- nmf(log(nmat/min(nmat)), 3, method='lee', seed='nndsvd')
  nmffit <- nmf(Y, n_components, method = 'lee', seed = init)
  Hgroup <- nmffit@fit@H
  Wgroup <- nmffit@fit@W
  colnames(Hgroup) <- colnames(sansf)

# convert to more readable format
  Hpop <- as_tibble(t(Hgroup)) %>% rename(V1 = w0)
  Hpop$IDvec <- rownames(t(Hgroup))
  Hpop <- separate(Hpop, IDvec, 
                 into=c('study','condition','ID','component'))

  # plot(Wgroup[,1],type='l', col='red')
  # lines(Wgroup[,2], col='green')
  # lines(Wgroup[,3], col='blue')
  # lines(Wgroup[,4], col='violet')
  # lines(Wgroup[,5], col='brown')
  # lines(Wgroup[,6], col='pink')
  pairs(t(Hgroup))
  groupfit <- list()
  groupfit$W <- Wgroup
  groupfit$H <- Hpop
  groupfit
}

nmf2 <- function(data, Wgroup, n_components=6, compare_random_seed=FALSE){
  
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
  err_random <- list()
  # do nmf for each individual
  for (id in IDlist$ID) {
    print(id)
    idmat <- datahyp %>%
      #filter(str_detect(ID, 'baseline')) %>%
      filter(CH == 'C3',
             ID == id,
             # STAGE=='NREM2',
             #CYCLE >= 1
      ) %>%
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
    colnames(h0) <- NULL
    colnames(w0) <- NULL
    w0 <- Wgroup
    sansf <- idmat[, -1]
    nmat <- matrix(as.numeric(unlist(sansf)), nrow = nrow(sansf))
    init <- nmfModel(n_components, nmat, W = w0, H = h0)
    nmffit <- nmf(log(nmat / min(nmat)), n_components, method = 'lee', seed = init)
    
    if (compare_random_seed == TRUE){
      nmffit_random <- nmf(log(nmat / min(nmat)), n_components, method = 'lee')
      err_random[[id]] <- nmffit_random@residuals
    }
    
    err[[id]] <- nmffit@residuals
    resultsH <- as.tibble(t(nmffit@fit@H))
    resultsW <- as.tibble(nmffit@fit@W)
    resultsH$E <- epochs
    resultsW$FR <- fr
    resultsW$ID <- id
    resultsH$ID <- id
    resultsH$STAGE_N <- stage_n$STAGE_N
    
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
    graphics.off()
    p1 <- ggplot(resultsW, aes(x=FR,y=value,color=component))+
      geom_line()
    
    p2 <- ggplot(resultsH, aes(x=E,y=value,color=component))+
      geom_line()
    plot(p1/p2)
    #ggsave(paste(figdir, id, '_withwake.pdf',sep=''))
  }
  list(H=Hall, W=Wall, err=err)
}

nmf_indiv <- function(datahyp, id, n_components=6){
  
    print(id)
    idmat <- datahyp %>%
      filter(CH == 'C3',
             ID == id,
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


n_components <- 6
nmf_fit <- nmf1(datahyp, n_components=n_components)
groupfit <- group_nmf(nmf_fit, n_components=n_components)
refit <- nmf2(datahyp, groupfit$W, n_components=n_components)

