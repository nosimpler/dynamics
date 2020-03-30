# ODE fitting
library(tidyverse)
library(sindyr)
library(tvR)

dnx <- function(x) {denoise1(log10(x/max(x)), 1e30)}






# Cross-comparison (takes lots of time)
#####
# hard-code your favorite variable selection

sdist <- NULL
for (id in flatten(IDlist)){
  print(id)
  reduced_m1 <- m1c %>% recast() %>% filter(ID==id) %>%
    select('ID','E', 
           'SLOW_O1', 'SLOW_O2',
           'DELTA_O1', 'DELTA_O2',
            'THETA_O1', 'THETA_O2',
            'ALPHA_O1', 'ALPHA_O2',
            'BETA_O1', 'BETA_O2',
            'GAMMA_O1', 'GAMMA_O2') %>%
    arrange(E) %>%
    filter( E < min(E)+360 )
  reduced_m1_reg <- mapply(dnx, reduced_m1[,3:14])
    sindy1 <- sindy(reduced_m1_reg[,1:12])
    
    for (id2 in flatten(IDlist)){
    reduced_m2 <- m2c %>% recast() %>% filter(ID==id2) %>%
      select('ID', 'E', 
             'SLOW_O1', 'SLOW_O2',
             'DELTA_O1', 'DELTA_O2',
             'THETA_O1', 'THETA_O2',
             'ALPHA_O1', 'ALPHA_O2',
             'BETA_O1', 'BETA_O2',
             'GAMMA_O1', 'GAMMA_O2') %>% 
    arrange(E) %>%
      filter( E < min(E)+360 )
    reduced_m2_reg <- mapply(dnx, reduced_m2[,3:14])
    sindy2 <- sindy(reduced_m2_reg[,1:12])
    distance_sindy <- sum(abs(sindy2$B-sindy1$B))
    row <- c(ID1=id, ID2=id2, D=distance_sindy)
    sdist <- rbind(sdist, row)
    print(row)
    print(sindy2$simple.kolmog)
    print(sindy1$simple.kolmog)
    }
}


# Histograms (diagonal only)
#####
sdist <- NULL
for (id in flatten(IDlist)){
  print(id)
  reduced_m1 <- m1c %>% recast() %>% filter(ID==id) %>%
    select('ID','E', 
           'SLOW_O1', 'SLOW_O2',
           'DELTA_O1', 'DELTA_O2',
           'THETA_O1', 'THETA_O2',
           'ALPHA_O1', 'ALPHA_O2',
           'BETA_O1', 'BETA_O2',
           'GAMMA_O1', 'GAMMA_O2') %>%
    arrange(E) %>%
    filter( E < min(E)+360 )
  reduced_m1_reg <- mapply(dnx, reduced_m1[,3:14])
  sindy1 <- sindy(reduced_m1_reg[,1:12])
  
    reduced_m2 <- m2c %>% recast() %>% filter(ID==id) %>%
      select('ID', 'E', 
             'SLOW_O1', 'SLOW_O2',
             'DELTA_O1', 'DELTA_O2',
             'THETA_O1', 'THETA_O2',
             'ALPHA_O1', 'ALPHA_O2',
             'BETA_O1', 'BETA_O2',
             'GAMMA_O1', 'GAMMA_O2') %>% 
      arrange(E) %>%
      filter( E < min(E)+360 )
    reduced_m2_reg <- mapply(dnx, reduced_m2[,3:14])
    sindy2 <- sindy(reduced_m2_reg[,1:12])
    distance_sindy <- sum(mapply(max, abs(sindy1-sindy2))) # L1 matrix norm
    ds <- abs(sindy2$simple.kolmog - sindy1$simple.kolmog)
    row <- c(ID1=id, ID2=id, D=distance_sindy, DS=ds)
    sdist <- rbind(sdist, row)
    print(row)
    print(sindy2$simple.kolmog)
    print(sindy1$simple.kolmog)
}