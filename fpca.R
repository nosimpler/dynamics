# # turn data into fpca

library(fdapace)
asym_fpca <- function(ch_asym){
 asym_fp <- ch_asym %>%
  select(ID,E,ends_with("DIFF")) %>%
  arrange(E)
  fi <- MakeFPCAInputs(IDs = asym_fp$ID, asym_fp$E, asym_fp$FDIFF, na.rm = TRUE, sort = TRUE)
  fp <- FPCA(fi$Ly,fi$Lt)
}

fpca_all <- function(df){
  df <- df %>% arrange(E)
  fi <- MakeFPCAInputs(IDs = df$ID, tVec= df$E, yVec= t(df[,3:98]), na.rm = TRUE, sort = TRUE)
  fp <- FPCA(fi$Ly,fi$Lt)
}


fpca_one <- function(df,ID){
  df <- df %>% filter(ID==sym(!!ID)) %>% arrange(E)
  df <- remove_outliers(df)
  print(dim(df$E))
  print(dim(df[,3:98]))
  
  fi <- MakeFPCAInputs(ID=NULL, tVec= df$E, yVec= log(t(df[,3:98])), na.rm = TRUE, sort = TRUE)
  out <- list()
  out$fpca <- FPCA(fi$Ly,fi$Lt,list(useBinnedData="OFF"))
  out$stage <- df$STAGE
  out$E <- df$E
  out$colnames <- colnames(df[,3:98])
  out
}
