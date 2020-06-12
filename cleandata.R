# smoothing, outliers, etc.
library(OutlierDetection)
library(tvR)
# find outliers
rm_outliers <- function(df, var){
  #df <- df %>% mutate(FLAGGED=0)
  idx <- (abs(df[[var]]-mean(df[[var]]))>2*sd(df[[var]]))
  #print(idx)
  df[[var]][idx] <- NA
  #print(df)
  df
}

# again, NN method
rm_outliers2 <- function(df){
  idx <- nn(df$value,cutoff=0.9)[['Location of Outlier']]
  print(idx)
  if (length(idx) == 0){
    return(df)
  }
  else{
    return(df[-idx,])
  }
}

tvr <- function(x) denoise1(x, lambda=1e30)
tvrd <- function(x) denoise1(diff(x), lambda=1e30)
xdx_all <- function(x) apply(x,1, xdx)
normalize <- function(x) (x-min(x))/(max(x)-min(x))
xdx <- function(x) normalize(tvrd(-log10(x/max(x))))
rx <- function(x) normalize(tvr(-log10(x/max(x))))
rx_all <- function(x) apply(x, 1, rx)
dx <- function(x) denoise1(x, lambda=1e-30)
