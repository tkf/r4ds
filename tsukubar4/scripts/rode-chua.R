library(odesolve)
library(scatterplot3d)

dydt <- function(t, y, p){
  c1 <- p[['c1']]
  c2 <- p[['c2']]
  c3 <- p[['c3']]
  m0 <- p[['m0']]
  m1 <- p[['m1']]
  
  list(c(c1*( y[2] - y[1]
             - (m1*y[1] + (m0-m1)/2.0*(  abs(y[1]+1)
                                       - abs(y[1]-1) )
                )),
         c2*( y[1] - y[2] + y[3]),
         -c3*y[2] ))

}

params <- list(c1 = 15.6, c2 = 1, c3 = 50,
               m0 = -8/7, m1 = -5/7)
times <- c(0, 0.01*(1:10000))

ind <- 0
for (c3 in list(50, 35, 33.8, 33.6, 33, 25.58)){
#for (c3 in list(50)){
  y1 <- c()
  y2 <- c()
  y3 <- c()
  clr <- c()
  cls <- colors()
  #dcls <- round(length(cls)/(len*len))
  params[['c3']] <- c3
  for (y0 in list(c(1,0,0), c(-1,0,0))) {
    y <- lsoda(y0, times, dydt, params)
    y1 <-  c(y1, y[5000:10000,2])
    y2 <-  c(y2, y[5000:10000,3])
    y3 <-  c(y3, y[5000:10000,4])
  #clr <- c(clr, rep(cls[(i-1)*dcls+1], length(times)))
  }
  png(filename=paste("chua-",ind,".png",sep=""))
  scatterplot3d( y3, y2, y1,
                type='l', angle=20,
                zlim=c(-4,4), ylim=c(-1,1), xlim=c(-6,6))
  dev.off()
  ind <- ind + 1
}
