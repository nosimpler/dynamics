# transition matrices
# markov stuff
library(markovchain)
library(tidyverse)

#####
# turn N4 to N3
#####
N4toN3 <- function(h){
  h$baseline <- h$baseline %>% 
    mutate(STAGE_N = recode(STAGE_N, `-4` = -3),
           STAGE = recode(STAGE, 'NREM4' = 'NREM3'))
  h$followup <- h$followup %>% 
    mutate(STAGE_N = recode(STAGE_N, `-4` = -3),
           STAGE = recode(STAGE, 'NREM4' = 'NREM3'))
  h
}
htest <- N4toN3(h)

# REM=1,other=0
is_REM <- function(h){
  h$baseline <- h$baseline %>%
    mutate(IS_REM = recode(STAGE, 'NREM4' = 0,
                          'NREM3' = 0,
                          'NREM2' = 0,
                          'NREM1' = 0,
                          'wake' = 0,
                          'REM' = 1))
  h$followup <- h$followup %>% 
    mutate(IS_REM = recode(STAGE, 'NREM4' = 0,
                          'NREM3' = 0,
                          'NREM2' = 0,
                          'NREM1' = 0,
                          'wake' = 0,
                          'REM' = 1))
  h
}

# sleep=1, other=0
is_sleep <- function(h){
  h$baseline <- h$baseline %>%
    mutate(IS_SLEEP = recode(STAGE, 'NREM4' = 0,
                          'NREM3' = 0,
                          'NREM2' = 0,
                          'NREM1' = 0,
                          'wake' = 1,
                          'REM' = 0))
  h$followup <- h$followup %>% 
    mutate(IS_SLEEP = recode(STAGE, 'NREM4' = 0,
                          'NREM3' = 0,
                          'NREM2' = 0,
                          'NREM1' = 0,
                          'wake' = 1,
                          'REM' = 0))
  h
}

# table of pre/post stages
transition_table <- function(df){
  df <- df
  n <- length(df)
  #print(df)
  post <- df[2:n]
  pre <- df[1:(n-1)]
  #print(post)
  tibble(pre=pre, post=post)
}

# group STAGE -> STAGE into blocks
block_table <- function(df) {
  diagonals <- transmute(transition_table(df), 
                       transition = as.integer(pre != post) )
  block_number = as_tibble(cumsum(diagonals)+1)

  blocks <- block_number %>% group_by(transition) %>%
    summarize(n=n())
}

# transition probs for one ID
transition_counts <- function(seq){
  transition_tab <- transition_table(seq)
  n_transitions <- nrow(transition_tab)
  transition_tab %>% 
    group_by(pre, post) %>%
    summarize(n = n(), p = n()/n_transitions)
}

# takes hypno object
transition_counts_all <- function(h){
  t <- list()
  t$baseline <- NULL
  t$followup <- NULL
  for (id in h$IDs){
    hb <- h$baseline %>%
      filter(ID==id) %>%
      arrange(E) %>%
      select(STAGE)
    hf <- h$followup %>%
      filter(ID==id) %>%
      arrange(E) %>%
      select(STAGE)
    newrows_baseline <- transition_counts(hb)
    newrows_followup <- transition_counts(hf)
    newrows_baseline$ID <- id
    newrows_followup$ID <- id
    t$baseline <- rbind(t$baseline, newrows_baseline)
    t$followup <- rbind(t$followup, newrows_followup)
  }
  tca <- list()
  tca$baseline <- t$baseline %>% 
    unite(TRANSITION, pre, post, sep='_to_') %>%
    pivot_wider(names_from=TRANSITION, values_from=c(n,p)) %>%
    mutate_all(~replace(., is.na(.), 0))
  tca$followup <- t$followup %>% 
    unite(TRANSITION, pre, post, sep='_to_') %>%
    pivot_wider(names_from=TRANSITION, values_from=c(n,p)) %>%
    mutate_all(~replace(., is.na(.), 0))
  tca
}

## compute total partition entropy of sleep
partition_entropy <- function(df){
  tt <- transition_table(df)
  block_lengths <- block_table(tt)
  block_areas <- summarize(block_lengths, pent = sum(n^2)/sum(n)^2)
}

# ##### scripts for chat
# pe1 <- m1 %>% recast() %>%
#   group_by(ID) %>% 
#   summarize(par_ent = t(partition_entropy(STAGE)))
# pe2 <- m2 %>% recast %>%
#   group_by(ID) %>% 
#   summarize(par_ent = t(partition_entropy(STAGE)))
# pe <- left_join(pe1, pe2, by='ID') %>% 
#   drop_na()
# 
# hypno_mm1 <- m1 %>% group_by(ID) %>% 
#   arrange(E) %>% 
#   summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
#             tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
#             #stagesum=sum(STAGE_N/sum(PERSISTENT_SLEEP))
#             )
# 
# hypno_mm2 <- m2 %>% group_by(ID) %>% 
#   arrange(E) %>% 
#   summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
#             tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
#             #stagesum=sum(STAGE_N)/sum(PERSISTENT_SLEEP)
#             )
# 
# hypno <- left_join(hypno_mm1, hypno_mm2, by='ID') %>% 
#   drop_na()
#cor(hypno$stagesum.x, hypno$stagesum.y)
#plot(hypno$stagesum.x, hypno$stagesum.y)


##### unpack and summarize all Markov variables
# 
# # (issue with dplyr; some version of this should work in dplyr > 1.0)
# hypno_mm_full1 <- m1 %>% group_by(ID) %>%
#   arrange(E) %>%
#   summarize(!!!unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix))
# 
# hypno_mm_full2 <- m2 %>%
#   arrange(E) %>% group_by(ID) %>%
#   summarize(!!!(unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix)))

# partition entropy (variance of block length)
parent_distance <- function(pe){
  dxY <- NULL
  for (id1 in pe$ID){
    for (id2 in pe$ID){
      x <- filter(pe, ID==id1)$par_ent.x
      y <- filter(pe, ID==id2)$par_ent.y
      row <- tibble(ID1=id1, ID2=id2, PENTD=abs(x-y))
      dxY <- rbind(dxY, row)
    }
  }
  dxY
}
htest <- is_REM(htest)
hpe <- htest$baseline %>% arrange(ID,E) %>% group_by(ID) %>% summarize(pe=t(partition_entropy(IS_REM))) 
