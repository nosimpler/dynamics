# smoothing, outliers, etc.

library(tvR)
# find outliers
outlier <- function(x,t=3) { mean(x,na.rm=T) + t * sd(x,na.rm=T) }

outliers <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[ x < lwr ] <- NA
  x[ x > upr ] <- NA
  x
}

outliers.inc <- function(x, inc = rep(T,length(x)) , m =mean(x[inc],na.rm=T) , sdev = sd(x[inc],na.rm=T) ,t=3)
{
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[ x < lwr ] <- NA
  x[ x > upr ] <- NA
  x[ ! inc ]   <- NA
  x
}

is.outlier <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
  is.na( (!is.na(x)) & outliers(x,m,sdev,t))
}

is.outlier.inc <- function(x,inc=inc,t=3)
{
  m = mean(x[inc],na.rm=T) 
  sdev = sd(x[inc],na.rm=T) 
  (!inc) | is.na( (!is.na(x)) & outliers(x,m,sdev,t))
}

tvr <- function(x) denoise1(x, lambda=1e30)
tv <- function(x, l) denoise1(x, lambda=l)
tvrd <- function(x) denoise1(diff(x), lambda=1e30)
xdx_all <- function(x) apply(x,1, xdx)
normalize <- function(x) (x-min(x))/(max(x)-min(x))
xdx <- function(x) normalize(tvrd(-log10(x/max(x))))
rx <- function(x) normalize(tvr(-log10(x/max(x))))
rx_all <- function(x) apply(x, 1, rx)
dx <- function(x) denoise1(x, lambda=1e-30)
tvrd1 <- function(x) denoise1(c(diff(x)[1],diff(x)), lambda=1e30)
diffx <- function(x) c(diff(x),diff(x)[length(diff(x))])
loessx <- function(x,t) predict(loess(x~t, span = 0.1))
