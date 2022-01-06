library(luna)
library(tidyverse)
source("~/dyn/src/dynamics/cleandata.R")
#source("~/dyn/src/dynamics/chat0.2.R")


edf <- "~/dyn/data/chat/edfs/chat-baseline-300807.edf"

hyp <- load_hypno() %>% filter(ID=='chat-baseline-300807')

ledf(edf)
ne = lepoch()
chs <- c("C3","C4","O1","O2","F3","F4","T3","T4")
data <- ldata(1:ne,chs)

unitvec_fft <- function(x) exp(1i*Arg(fft(x)))
unitvec_hilbert <- function(x, band =c(35,45), fs=200) exp(1i*Arg(seewave::hilbert(bp(x, band, fs), fs)))
bp <- function(x, band, fs) signal::filtfilt(signal::butter(3,band*2/fs), x)
freq_fft <- function(L,Fs=200) Fs*(seq(0,L-1))/L


glob_coh_fourier <- data %>%
  group_by(E) %>%
  mutate(across(chs, unitvec_fft)) %>%
  mutate(GLOB_COH = abs(rowMeans(across(chs)))) %>%
#  mutate(GLOB_COH = GLOB_COH - min(GLOB_COH)) %>%
  mutate(FR = freq_fft(n())) %>%
  filter(FR < 100) %>%
  select(E,FR,GLOB_COH)

glob_coh_hilbert_minima <- function(data) {
  gch <- data %>%
    mutate(across(chs, unitvec_hilbert)) %>%
    mutate(GLOB_COH = abs(rowMeans(across(chs))))
  idxmin <- as_tibble(pracma::findpeaks(-gch$GLOB_COH)) %>%
    transmute(GLOB_COH_AT_MIN = -V1, INDEX = V2) %>%
    filter(GLOB_COH_AT_MIN < 0.9)
  gch[idxmin$INDEX,] %>%
    pivot_longer(4:11, names_to = "CH", values_to = 'PHASE') %>%
    mutate(PHASE = Arg(PHASE))
}



#coh_hyp <- left_join(hyp, glob_coh)
gch <- glob_coh_hilbert_minima(data)

# plot individual
ggplot(gch %>% filter(GLOB_COH == min(GLOB_COH)), aes(x=PHASE, fill = CH))+
  scale_x_continuous(expand = c(0,0))+
  geom_histogram(binwidth = pi/12)+
  coord_polar()

ggplot(gch %>% filter(GLOB_COH < 0.005), aes(x=PHASE))+
  scale_x_continuous(expand = c(0,0))+
  geom_histogram(binwidth = pi/12)+
  #coord_polar()+
  facet_wrap(~CH)


# ggplot(coh_hyp %>% filter(FR != 0,FR<45), aes(x=E,y=FR,fill=GLOB_COH))+
#   geom_tile()+
#   scico::scale_fill_scico(palette='vik')
#
# ggplot(coh_hyp %>% filter(FR < 45), aes(x=FR,y=GLOB_COH,color=STAGE))+
#   stat_summary_bin(bins=46, geom='line')+
#   #geom_point(alpha=0.01)+#
#  scale_color_brewer(palette='Set1')
