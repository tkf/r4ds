library(odesolve)
library(scatterplot3d)

dydt <- function(t, y, p){
  a <- p[['a']]
  b <- p[['b']]
  c <- p[['c']]
  
  list(c(- y[2] - y[3],
         y[1] + a * y[2],
         b + (y[1] - c) * y[3] ))
}

params <- list(a = 0.1,
               b = 0.1,
               c = 4.0)
len <- 50000
times <- c(0, 0.01*(1:len))

ind <- 0
for (c0 in list(4, 6, 8.5, 8.7, 9, 12, 12.8, 13, 18)){
#for (c3 in list(50)){
  params[['c']] <- c0
  y <- lsoda(c(1,0,0), times, dydt, params)
  y1 <-  y[10000:len,2]
  y2 <-  y[10000:len,3]
  y3 <-  y[10000:len,4]
  #clr <- c(clr, rep(cls[(i-1)*dcls+1], length(times)))
  png(filename=paste("rosser-",ind,".png",sep=""))
  scatterplot3d( y1, y2, y3,
                type='l', angle=80)
  dev.off()
  ind <- ind + 1
}

#png(filename="rosser.png")
scatterplot3d(y[,2:4], type='l')
#dev.off()
