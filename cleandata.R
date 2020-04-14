# smoothing, outliers, etc.
library(OutlierDetection)
library(tvR)
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

tvd <- function(x) denoise1(x, lambda=1e30)
xdx_all <- function(x) apply(x,1, xdx)
normalize <- function(x) x/max(x)
xdx <- function(x) tvd(log10(normalize(x)))
