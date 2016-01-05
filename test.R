graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
source("DBDA2E-utilities.R")

pointNo = 200
gx = runif(pointNo, 0, 1.5)

cp1 = 1.750193e-06
cp2 = 8.476438e-05
s = cp1 + cp2*gx

M0 = -8.940413e-07
S0 = cp1
M1 = 1.096418e-04
S1 = 2.741046e-05
beta0 = rnorm(1, M0, S0)
beta1 = rnorm(1, M1, S1)
y_hat = beta0 + beta1*gx

para = gammaShRaFromMeanSD(mean = y_hat, sd = s)
# print(para)
gy = c()
for(i in 1:pointNo){
  gamma_shape = para$shape;
  gamma_rate = para$rate;
  v = rgamma(1, shape = gamma_shape[i], rate = gamma_rate[i])
  gy = c(gy, v)
}
# print(gx)
# print(gy)


# input data
myData = read.csv( file="64pDpoints.csv" )
xName = "speed_Mbps" ; yName = "price_NTD"
y = myData[,yName]
x = myData[,xName]

# plot point
#graphing

xRang = max(x)-min(x)
yRang = max(y)-min(y)
xLimMult = 0.25
yLimMult = 0.45
xLim= c(0 , max(x)+xLimMult*xRang )
yLim= c(0 , max(y)+yLimMult*yRang )

openGraph()
plot(x, y, lwd=2 , col="black",cex=1.5, 
     xlim=xLim , ylim=yLim ,
     xlab = xName, ylab = yName
)
# hierarchical linear regression
xComb = seq(xLim[1],xLim[2],length=length(x))
lines( xComb , beta0 + beta1*xComb , col="skyblue", lwd=2)
points(gx, gy, col='red', cex=1, pch = 19)




