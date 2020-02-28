# basic normalization etc.
# doesn't work yet
range_normalize <- function(df,x){
  x <- sym(!!x)
  mutate(df, x=x/(max(x) - min(x)))
}