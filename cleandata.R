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

# take first n epochs beginning at the start of cycle cyc
truncate_to_cycle <- function(df, cyc, n_epochs){
  df %>% 
    group_by(ID) %>%
    arrange(E) %>%
    group_by(component, ID) %>%
    filter(row_number() >= which(CYCLE==cyc)) %>%
    mutate(E = E-min(E) + 1) %>%
    filter(E <= n_epochs) %>% 
    mutate(Etot = n()) %>%
    filter(Etot == n_epochs) 
}

truncate_to_delta_episode <- function(df) {
  
}

truncate_to_persistent_sleep <- function(df, n_epochs){
  df %>% 
    group_by(ID) %>%
    arrange(E) %>%
    filter(PERSISTENT_SLEEP==1) %>%
    mutate(E = E-min(E) + 1) %>%
    filter(E <= n_epochs) %>%
    group_by(component, ID) %>%
    mutate(Etot = n()) %>%
    filter(Etot == n_epochs) # remove IDs with not enough epochs
}

truncate_to_persistent_sleep_onesided <- function(df){
  df %>% 
    group_by(B,ID) %>%
    arrange(E) %>%
    filter(PERSISTENT_SLEEP==1) %>%
    mutate(E = E-min(E) + 1)
}

integrate_ode_fit <- function(){
  # 
  
  # fit to get constant
}

# split recording id into study, session, nsrrid
split_id <- function(df){
  df %>% separate(ID, c('study', 'session', 'nsrrid')) %>%
  mutate(nsrrid = as.numeric(nsrrid))
}

subcycles <- function(df){
  subcyc <- df %>% 
    select(B,E,PSD,nsrrid) %>%
    pivot_wider(id_cols=c(nsrrid,E), names_from=B, values_from=PSD) %>%
    group_by(nsrrid) %>%
    select(DELTA,E,nsrrid) %>%
    arrange(nsrrid,E) %>%
    mutate(smoothed_delta=normalize(tvr(DELTA))) %>%
    mutate(minima = splus2R::peaks(-smoothed_delta, span=100)) %>%
    # hack ("which" takes row numbers, not epochs, so have to shift)
    # also force cycle termina to have normalized DELTA < 0.35
    mutate(SUBCYC = cut(E, 
                        breaks=c(min(E), 
                                 which(minima==TRUE & 
                                         smoothed_delta < 0.25) + min(E), 
                                 max(E)
                                 ))) #%>%
    # TODO: index with linear order 
    #mutate(SUBCYC_IDX = refactor)
    left_join(df, subcyc, by=c("nsrrid", "E"))
}


get_rem <- function(df, cycle=1){
  df %>% group_by(ID, component) %>%
    filter(CYCLE == cycle) %>%
    filter(row_number() >= first(which(STAGE_N==0)))
}

get_prerem <- function(df, cycle = 1){
  n_components <- length(unique(df$component))
  df %>% group_by(ID,component) %>%
  filter(CYCLE==cycle) %>%
    filter(row_number() < first(which(STAGE_N==0))) %>%
    filter(E > max(E)-50)
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
