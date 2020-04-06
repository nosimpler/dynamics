##### EDIT DISTANCE ON HYPNOGRAM
library(tidyverse)
library(stringdist)
library(TSdist)
library(pdc)
library(dtw)

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
      row$D_lb.keogh <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], window.size=30, distance='lb.keogh')
      row$D_edr0 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='edr', epsilon=0)
      row$D_edr1 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='edr', epsilon=1)
      row$D_lcss0 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='lcss', epsilon=0)
      row$D_lcss1 <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='lcss', epsilon=1)
      row$D_mindist.sax <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], w=10,distance='mindist.sax')
      row$D_ncd <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='ncd')
      row$D_pdc <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='pdc')
      #row$D_frechet <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='frechet')
      row$D_tam <- TSDistances(seq1$STAGE_N[1:300], seq2$STAGE_N[1:300], distance='tam')
      
      print(id2)
      row <- as_tibble(row)

      dtab <- rbind(dtab, row)
    }
  }
  dtab
}

#####
# full-night (requires STAGE_N)
# quantity is e.g. hypnogram or sigma power in O1
#####
make_distance_table <- function(ds, quantity){
  h1 <- ds$baseline
  h2 <- ds$followup
  dtab <- NULL
  row <- NULL
  for (id1 in unique(ds$demo$nsrrid)){
    
    seq1 <- h1 %>% 
      filter(ID==id1) %>%
      arrange(E) %>%
      select(STAGE_N) %>% 
      filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
    
    for (id2 in unique(ds$demo$nsrrid)){
      seq2 <- h2 %>%
        filter(ID==id2) %>%
        arrange(E) %>%
        select(STAGE_N) %>% 
        filter(row_number() >= first(which(STAGE_N < 1))) # remove first wake period
      
      
      row$ID1 <- id1
      row$ID2 <- id2
      row$D_erp <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='erp', g=0)
      row$D_ccor <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='ccor')
      row$D_dtw <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='dtw')
      row$D_edr0 <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='edr', epsilon=0)
      row$D_edr1 <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='edr', epsilon=1)
      row$D_lcss0 <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='lcss', epsilon=0)
      row$D_lcss1 <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='lcss', epsilon=1)
      row$D_mindist.sax <- TSDistances(seq1[[quantity]], seq2[[quantity]], w=10,distance='mindist.sax')
      row$D_ncd <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='ncd')
      row$D_pdc <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='pdc')
      row$D_tam <- TSDistances(seq1[[quantity]], seq2[[quantity]], distance='tam')
  
      
      row <- as_tibble(row)
      
      dtab <- rbind(dtab, row)
    }
    print(id1)
  }
  dtab <- mutate(dtab, DIAG=(ID1==ID2))
}






#####
# compare distributions
#####
#dist_diag = mutate(erdt, DIAG=(ID1==ID2))
#dist_diag_l <- pivot_longer(dist_diag, 3:16, names_to="MEASURE", names_prefix="D_")
#ddl_summary <- dist_diag_l %>% 
#  group_by(MEASURE) %>% 
#  summarize(P=t.test(value[DIAG==TRUE],value[DIAG==FALSE])$p.value)

##### 
# plot distances
#####

# compute worst and best match
worst_match <- function(dist_table){
  dist_table %>% 
    ungroup() %>%
    group_by(MEASURE) %>%
    arrange(MEASURE) %>%
    filter(value==max(value)) %>%
    filter(row_number()==1)
}

best_match <- function(dist_table){
  dist_table %>% 
    ungroup() %>%
    group_by(MEASURE) %>%
    arrange(MEASURE) %>%
    filter(value==min(value)) %>%
    filter(row_number()==1)
}


