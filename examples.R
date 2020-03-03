# examples
library(psd)
library(pracma)
# kronecker delta
delta <- function(n_samples, loc){
  zeros <- numeric(n_samples)
  zeros[loc] <- 1
  zeros
}

comb <- function(n_samples, loc, n_repeats){
  rep(delta(n_samples, loc), n_repeats)
}

x <- -10:10
pulse <- diff(exp(-x^2/10))
plot(pulse)
func <- conv(pulse, comb(100, 25, 100))
plot(func, type='l')
plot(fftshift(abs(fft(func))), type='l')