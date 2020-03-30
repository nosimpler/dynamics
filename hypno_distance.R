##### EDIT DISTANCE ON HYPNOGRAM
library(tidyverse)
library(stringdist)
library(TSdist)
library(pdc)
library(dtw)
# can move to chat.R
# 
# hypno_baseline <- read_table2('~/Desktop/HYPNO-E.txt', guess_max = 1000000) %>%
#   filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
#   separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
#   filter(COND=='baseline') 
# 
# hypno_followup <- read_table2('~/Desktop/HYPNO-E.txt', guess_max = 1000000) %>%
#   filter(!is.na(STAGE), !grepl('nonrandomized', ID)) %>% select(ID, E, STAGE, STAGE_N) %>%
#   separate(ID, into = c("DATASET", "COND", "ID"), sep='-') %>%
#   filter(COND=='followup')

IDlist <- intersect(unique(hypno_followup$ID), unique(hypno_baseline$ID))



edit_distance_table <- function(h1, h2){
  dtab <- NULL
  row <- NULL
  for (id1 in IDlist){
    
    seq1 <- h1 %>% 
      filter(ID==id1) %>%
      arrange(E) %>%
      select(STAGE_N) %>% 
      filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
    
    for (id2 in IDlist){
      seq2 <- h2 %>%
        filter(ID==id2) %>%
        arrange(E) %>%
        select(STAGE_N) %>% 
        filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
      

      row$ID1 <- id1
      row$ID2 <- id2
      #row$D_seqsim <- seq_sim(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300])
      row$D_erp <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='erp', g=0)
      row$D_L2 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='euclidean')
      row$D_L1 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='manhattan')
      #row$D_minkowski <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='minkowski')
      row$D_ccor <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='ccor')
      row$D_sts <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='sts')
      row$D_dtw <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='dtw')
      #row$D_lb.keogh <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], window.size=30, distance='lb.keogh')
      row$D_edr0 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='edr', epsilon=0)
      row$D_edr1 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='edr', epsilon=1)
      row$D_lcss0 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='lcss', epsilon=0)
      row$D_lcss1 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='lcss', epsilon=1)
      row$D_mindist.sax <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], w=10,distance='mindist.sax')
      row$D_ncd <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='ncd')
      row$D_pdc <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='pdc')
      #row$D_frechet <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='frechet')
      row$D_tam <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='tam')
      
      print(id1)
      row <- as_tibble(row)

      dtab <- rbind(dtab, row)
    }
  }
  dtab
}
erdt <- edit_distance_table(hypno_baseline, hypno_followup)

