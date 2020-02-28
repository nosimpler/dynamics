# functions for partitioning, and sampling data, etc.

# test on first twenty
ids_default <- seq(1,50)
scaledown <- function(df, keep_ids=ids_default){
  tab <- filter(df, ID %in% ids_default)
}
