# dealing with outliers
library(OutlierDetection)
# uses nearest neighbor method to find outliers
flag_outliers <- function(df){
  df <- df %>% mutate(FLAGGED=0)
  idx <- nnk(df,cutoff=0.95)[['Location of Outlier']]
  df$FLAGGED[idx] <- 1
  df
}

# again, NN method
remove_outliers <- function(df){
  idx <- nnk(df,cutoff=0.9)[['Location of Outlier']]
  df[-idx,]
}