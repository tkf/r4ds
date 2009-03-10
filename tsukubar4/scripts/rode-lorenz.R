library(odesolve)
library(scatterplot3d)

dydt <- function(t, y, p){
  dl <- p[['dl']]
  ro <- p[['ro']]
  bt <- p[['bt']]
  
  list(c(dl * (y[2] - y[1]),
         y[1] * (ro - y[3]) - y[2],
         y[1] * y[2] - bt * y[3] ))
}

params <- list(dl = 10,
               bt = 8/3,
               ro = 28)
times <- c(0, 0.01*(1:10000))
y <- lsoda(c(-1,0,0), times, dydt, params)

#png(filename="lorenz.png")
scatterplot3d(y[,2:4], type='l')
#dev.off()
