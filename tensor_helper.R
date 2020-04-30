# tensor

# make tensor from data

# for spectral data
first300 <- ball$baseline %>%
    group_by(CH, B, COND, ID) %>%
    filter(B != 'TOTAL') %>%
    arrange(CH, B, E) %>%
    filter(row_number() >= first(which(STAGE_N < 1))) %>%
    top_n(n=-300, wt=E) #%>%
    #mutate(RELPSD=rx(RELPSD))


make_tensor <- function(df){
dfa <- array(data = df$RELPSD, 
             dim=c(length(unique(df$ID)), 
                   300,
                   length(unique(df$B)), 
                   length(unique(df$CH))), 
             dimnames=list(unique(df$ID), 1:300, unique(df$B), unique(df$CH)))
as.tensor(dfa)
}

first300t <- make_tensor(first300)

nntf <- NTF(first300t, rank=3, viz=TRUE)

meas$ntf1 <- nntf$A[[1]][1,]
meas$ntf2 <- nntf$A[[1]][2,]
summary(lm(ntf1~age+race+male+bmiz, data=meas))
summary(lm(ntf2~age+race+male+bmiz, data=meas))
