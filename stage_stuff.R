# transition matrices
# markov stuff
library(markovchain)
library(tidyverse)

# table of pre/post stages
transition_table <- function(df){
  n <- length(df)
  post <- df[2:n]
  pre <- df[1:(n-1)]
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

# compute total partition entropy of sleep
partition_entropy <- function(df){
  tt <- transition_table(df)
  block_lengths <- block_table(tt)
  block_areas <- summarize(block_lengths, pent = sum(n^2)/sum(n)^2)
}

##### scripts for chat
pe1 <- m1 %>% recast() %>%
  group_by(ID) %>% 
  summarize(par_ent = t(partition_entropy(STAGE)))
pe2 <- m2 %>% recast %>%
  group_by(ID) %>% 
  summarize(par_ent = t(partition_entropy(STAGE)))
pe <- left_join(pe1, pe2, by='ID') %>% 
  drop_na()

hypno_mm1 <- m1 %>% group_by(ID) %>% 
  arrange(E) %>% 
  summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
            tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
            stagesum=sum(STAGE_N/sum(PERSISTENT_SLEEP)))

hypno_mm2 <- m2 %>% group_by(ID) %>% 
  arrange(E) %>% 
  summarize(det = det(markovchainFit(STAGE)$estimate@transitionMatrix),
            tr = sum(diag(markovchainFit(STAGE)$estimate@transitionMatrix)),
            stagesum=sum(STAGE_N)/sum(PERSISTENT_SLEEP))

hypno <- left_join(hypno_mm1, hypno_mm2, by='ID') %>% 
  drop_na()
cor(hypno$stagesum.x, hypno$stagesum.y)
plot(hypno$stagesum.x, hypno$stagesum.y)
##### unpack and summarize all Markov variables

# why doesn't this work
hypno_mm_full1 <- m1 %>% group_by(ID) %>%
  arrange(E) %>%
  summarize(!!!unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix))

hypno_mm_full2 <- m2 %>%
  arrange(E) %>% group_by(ID) %>%
  summarize(!!!(unmatrix(markovchainFit(.$STAGE)$estimate@transitionMatrix)))


ggplot(hypno, aes(x=log(det.x)-log(det.y)))+geom_freqpoly()
