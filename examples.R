# examples
library(psd)
# kronecker delta
delta <- function(n_samples, loc){
  zeros <- numeric(n_samples)
  zeros[loc] <- 1
  zeros
}

comb <- function(n_samples, loc, n_repeats){
  rep(delta(n_samples, loc), n_repeats)
}

tri <- c(1, 2, 3, 2, 1, 0, -1, -2, -3, -2, -1, 0)
func <- conv(tri, comb(20, 10, 10))

plot(fftshift(abs(fft(func))), type='l')