library(sindyr)
library(deSolve)

parameters <- c(a = -8/3,
                b = -10,
                c = 28)

state <- c(X = 1,
           Y = 1,
           Z = 1)

Lorenz<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
      dX <- a*X + Y*Z
      dY <- b * (Y-Z)
      dZ <- -X*Y + c*Y - Z
    
        # return the rate of change
        list(c(dX, dY, dZ))
      }) # end with(as.list ...
}

SHO <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- Y
    dY <- -(k/m)*X
    list(c(dX,dY))
  })
}

# X is beta, Y is sigma, Z is delta 
NTP <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dX <- -a*X
    dY <- a*X - b*Y
    dZ <- b*Y
    list(c(dX,dY,dZ))
  })
}

# times <- seq(0, 100, by = 0.01)
# out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
# head(out)
# 
# par(oma = c(0, 0, 3, 0))
# plot(out, xlab = "time", ylab = "-")
# plot(out[, "X"], out[, "Z"], pch = ".")
# mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)

parameters <- c(
  a=0.1,
  b=0.5
)
state <- c(
  X=1,
  Y=0,
  Z=0
)
times <- seq(0, 50, by = 0.01)
out <- ode(y = state, times = times, func = NTP, parms = parameters)
head(out)
plot(out, xlab='time', ylab='-')
feat <- features(as.matrix(out[,2:4]), polyorder=1, intercept=TRUE)
head(feat)
fit <- sindy(xs=as.matrix(out[,2:4]), dt=0.01, Theta=feat, lambda=0)
print(fit$B)
