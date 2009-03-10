library(odesolve)
library(scatterplot3d)

sigmoid <- function(x){
  (tanh(x/2) + 1) / 2
}

dydt <- function(t, y, p){
  wt <- p[['wt']]
  bs <- p[['bs']]
  tu <- p[['tu']]
  
  list(( - y + wt %*% sigmoid( y +  bs ) ) / tu)
}

params <- list(wt = matrix(
                 c( 5.422 , -0.24 , 0.535 ,
                   -0.018 , 4.59  , -2.25 ,
                   2.75   , 1.21  , 3.885 ), 3,3),
               bs = c(-4.108, -2.787, -1.114),
               tu = c(1,2.5,1))
len0 <- 10000
len  <- 100000
#len0 <- 0
#len <- 50000
times <- c(0, 0.1*(1:len))
y <- lsoda(c(1,0,0), times, dydt, params)

ind <- 0
for (tu2 in list(1, 1.9, 2.0, 4.0)) for (tu1 in list(1, 2, 3))
{
#for (c3 in list(50)){
  params[['tu']][2] <- tu2
  params[['tu']][3] <- tu1

  y1 <- c()
  y2 <- c()
  y3 <- c()
  for (y0 in list(c(2,3,1), c(8,0,0))) {
    y <- lsoda(y0, times, dydt, params)
    y1 <- c(y1, y[len0:len,2] )
    y2 <- c(y2, y[len0:len,3] )
    y3 <- c(y3, y[len0:len,4] )
  }
  png(filename=paste("ctrnn-",ind,".png",sep=""))
  scatterplot3d( y1, y2, y3,
                type='l')
  dev.off()
  ind <- ind + 1
}
