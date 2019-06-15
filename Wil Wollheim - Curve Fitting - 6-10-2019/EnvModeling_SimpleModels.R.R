library(deSolve)

x.in <- c(N = 0.1)
t0 <- 1
tf <- 1000
times <- seq(t0, tf, 1)



# Model 1

model.ExpGrowth<-function(t, x, parms){
  dN = parms[1] * x[1]
  list(c(dN))
}

parms <- c(r = 0.001)
out <- as.data.frame(lsoda(x.in, times, model.ExpGrowth, parms))
plot(out$time, out$N, type = "l", xlab=c("time"), ylab=c("N"))






# Model 2

model.BathTub<-function(t, x, parms){with(as.list(parms),{
  dN = g - r * x[1]
  list(c(dN))
})
}
parms <- c(r = 0.002, g=1.0)
out <- as.data.frame(lsoda(x.in, times, model.BathTub, parms))
plot(out$time, out$N, type = "l", xlab=c("time"), ylab=c("N"))





# Model 3
parms <- c(r = 0.01, K = 0.6)
model.DensityDependant<-function(t, x, parms){
  dN = parms[1] * x[1] * ( 1 - x[1] / parms[2])
  list(c(dN))
}

out <- as.data.frame(lsoda(x.in, times, model.DensityDependant, parms))
plot(out$time, out$N, type = "l", xlab=c("time"), ylab=c("N"))
