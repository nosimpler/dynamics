# look at T3
ggplot(filter(distp,B=="BETA", CH=='T3', ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) #+ xlim(0,10)

# look at T4
ggplot(filter(distp,B=="BETA", CH=='T4',ID %in% similar_lengthID), aes(x=value))+
  geom_histogram(bins=50) #+ xlim(0,10)

# sum left channels
ggplot(filter(distp,B=="BETA",
              CH %in% c('C3','F3','O1','T3'),
              ID %in% similar_lengthID), 
       aes(x=value)) +
  geom_histogram(bins=200) + xlim(0,10)

# sum right channels
ggplot(filter(distp,B=="BETA",
              CH %in% c('C4','F4','O2','T4'),
              ID %in% similar_lengthID), 
       aes(x=value)) +
  geom_histogram(bins=200) + xlim(0,10)