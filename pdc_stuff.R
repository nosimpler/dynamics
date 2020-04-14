#pdc stuff
##### 
# get minE for all pairs
#####

library(pdc)

get_mine <- function(h){
seqmat <- matrix(nrow=15, ncol=0)
for (cond in c('baseline', 'followup')){
for (id in unique(h$baseline$ID)) {
  
  seq <- h[[cond]] %>% 
    filter(ID==id) %>%
    arrange(E) %>%
    select(STAGE_N) %>% 
    filter(row_number() >= first(which(STAGE_N < 1)))
  seqmat <- cbind(seqmat, seq[1:300,])
  print(id)
}
}

entropyHeuristic(seqmat, m.min=2, m.max=7, t.min=1, t.max=30)
}
get_mine(h)
