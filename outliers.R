# dealing with outliers
library(OutlierDetection)
# find outliers
flag_outliers <- function(df){
  df <- df %>% mutate(FLAGGED=0)
  idx <- nn(df,cutoff=0.9)[['Location of Outlier']]
  df$FLAGGED[idx] <- 1
  df
}

# again, NN method
remove_outliers <- function(df){
  idx <- nn(log10(df[,1:96]),cutoff=0.9)[['Location of Outlier']]
  print(idx)
  if (length(idx) == 0){
    return(df)
  }
  else{
    return(df[-idx,])
  }
}
