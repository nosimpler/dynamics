library(sindyr)
library(simecol)
#d <- sindy(bl1$RELPSD)

#gl <- 0.2
gl <- 0.1
C <- 1
t <- c(from=0, to=199, by=1)
#I <- c(zeros(50,m=1),ones(50, n=1),zeros(50,m=1), ones(50,m=1))

IAF <- odeModel(
  main = function(time, init, parms, inputs){
    with(as.list(c(init, parms)),{
    I <- approxTime1(inputs, time, rule = 2)["I"]
    dVdt <- (-gl*V + I)/C
    return(list(dVdt,I))
})
},
parms = c(C=C, gl=gl),
times = 1:700,
init=c(V=0),
solver="lsoda",
inputs=as.matrix(data.frame(
  time=1:700, 
  I = c(zeros(100,m=1),
        ones(100, m=1),
        zeros(100,m=1), 
        ones(100,m=1),
        zeros(50,m=1),
        ones(50,m=1),
        zeros(100,m=1),
        ones(100,m=1))#+0.2*rpois(n = 700, lambda = 2)
))
)
IAF=sim(IAF)
plot(IAF)
Arima(normalize(IAF@out[201:700,2]), c(2,0,0), method='CSS')
stats::spectrum(IAF@out[201:700,2], method='ar', order=2)