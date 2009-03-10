library(odesolve)

dydt <- function(t, y, p){
  c <- p[['c']]
  F <- p[['F']]

  list( c(y[2],
          - c *y[2] - y[1] + F * cos(t) ))
}

params <- list(c=0, F=0)
times <- c(0, 0.05*(1:1000))

#epg = expand.grid(seq(-3,3,length=6),seq(-3,3,length=6))
#y0s = matrix(c(epg[,1],epg[,2]), 36,2)
len1 = 10
len2 = 3
epg = expand.grid(
  seq(-5,5,length=len1),
  seq(-4,4,length=len2))
y0s = matrix(c(epg[,1],epg[,2]), len1*len2, 2)


png(filename="l1.png")
plot(0, type='l', xlim=c(-5,5), ylim=c(-5,5))
apply(y0s, 1,
      function(y0){
        print(y0)
        y <- lsoda(y0, times, dydt, params)
        lines(y[,2],y[,3], type='l')
      })
dev.off()

### 
params <- list(c=1.0, F=0)
png(filename="l2.png")
plot(0, type='l', xlim=c(-5,5), ylim=c(-5,5))
apply(y0s, 1,
      function(y0){
        print(y0)
        y <- lsoda(y0, times, dydt, params)
        lines(y[,2],y[,3], type='l')
      })
dev.off()
