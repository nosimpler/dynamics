library(tidyverse)
load("~/dyn/rdata/chat-final6.Rdata")
source("~/dyn/src/dynamics/chat0.2.R")
W_chat <- (refit %>% reorder_factors6())$W
H_chat <- (refit %>% reorder_factors6())$H
hypno_chat <- load_hypno()
H_chat <- H_chat %>% left_join(hypno_chat %>% select(ID, E, CYCLE))
rm(datahyp)

load("~/dyn/rdata/cfs-finalish6.Rdata")
source("~/dyn/src/dynamics/cfs.R")
hypno_cfs <- load_hypno_cfs()
W_cfs <- (refit %>% reorder_factors6())$W
H_cfs <- (refit %>% reorder_factors6())$H
H_cfs <- H_cfs %>% left_join(hypno_cfs %>% select(ID, E, CYCLE))
W <- rbind(W_cfs, W_chat)
H <- rbind(H_cfs, H_chat)

W <- W %>% 
  split_id()
H <- H %>% split_id()
# gg <- ggplot(W, aes(x=FR, y=value, color=study))+
#   stat_summary_bin(fun.min = function(z) { quantile(z,0.1) },
#                fun.max = function(z) { quantile(z,0.9) },
#                fun = median)+
#   facet_wrap(~component)
# stylize(gg)