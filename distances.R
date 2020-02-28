# DTW
library(dtw)

dnx <- function(x){denoise1(x,1e300)}
dtw_table <- function(m1, m2){
  
dist_tab <- NULL

# builds dynamic-time-warping distance table
for (id in unique(m2$ID)[1:20]){
  rows <- list()
  for (i in 3:98){
    test1 <- m1 %>% filter(ID==id) %>% arrange(E) %>% remove_outliers()
    test2 <- m2 %>% filter(ID==id) %>% arrange(E) %>% remove_outliers()
    rows[[colnames(test1)[i]]] <- dtw(dnx(test1[,i]), dnx(test2[,i]),  
                                      step.pattern=rabinerJuangStepPattern(4, "c", TRUE), 
                                      open.begin=TRUE, open.end=TRUE)$normalizedDistance
  }
  t <- as_tibble(rows)
  t$ID <- id
  print(id)
  dist_tab <- bind_rows(dist_tab,t)
}
}