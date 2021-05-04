# fine spectrogram

library(luna)
library(signal)
library(tidyverse)
library(NMF)
library(matlab)
edf <- "~/dyn/data/chat/edfs/chat-baseline-300007.edf"

n_components <- 3
ledf(edf)
lchs()
data <- ldata(301:450, chs = c("F3"))

f3spec <- specgram(data$F3)
aspec <- abs(f3spec$S)
plot(f3spec)
h0 <- ones(size(aspec))
init <- nmfModel(n_components, nmat, H = h0)


fit <- nmf(log(aspec/min(aspec)), n_components)

matplot(fit@fit@W)