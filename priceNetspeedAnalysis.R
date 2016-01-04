graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
source("DBDA2E-utilities.R")
source("findParameters.R")

# input data
myData = read.csv( file="64pDpoints.csv" )
xName = "speed_Mbps" ; yName = "price_NTD"
y = myData[,yName]
x = myData[,xName]
# plot(x, y, xlab="net speed(Mbps)", ylab="price(NTD)"
#      , main="Price for Net Speed")



# standardlize the given data
uy = y
ym = mean(uy)
ysd = sd(uy)
zy = (uy - ym)/ysd

ux = x
xm = mean(ux)
xsd = sd(ux)
zx = (ux - xm)/xsd


# 1. test linear regression
zetaLinear = SimpleLinearReg(zx, zy)
# print("zeta0 and zeta1")
# print(zetaLinear)


# 2.determine slope beta1 distribution whose
# mean is close to 0.63 and intercept is almost 0
line_no = 20
M1 = 0.63
S1 = 0.15
zbeta1 = rnorm( line_no , M1, S1 )
# Transform to original scale:
beta1 = zbeta1 * ysd / xsd
# print(ysd)
# print(xsd)


# 3.build gamma distribution
cPar = sdConstant(x, y)
print(cPar)
gx = seq(0.1, 1.5 ,length = 4)
# print(gx)
i = runif(1,1,line_no)
gy = beta1[i]*gx
# print(beta1[i])
# print(gy)
gsd = cPar[1] + cPar[2]*gx

k = c(4,3,2,1)
for(i in k){
  gamma_x = seq(0, 0.0006, length = 100)
  gamma_y = findGamma(gamma_x, gy[i], gsd[i])
  openGraph()
  title = c("Price Distribution to Netspeed at", round(gx[i],2), "Mbps")
  plot(gamma_x, gamma_y, type = 'l', col = 'skyblue',lwd = 8,
       xlab="price_NTD" , ylab="prob. density", main = title)
}


# plot result-------------------------------------------
#from example
xRang = max(x)-min(x)
yRang = max(y)-min(y)
xLimMult = 0.25
yLimMult = 0.45
xLim= c(0 , max(x)+xLimMult*xRang )
yLim= c(0 , max(y)+yLimMult*yRang )


#graphing
openGraph()
plot(x, y, lwd=2 , col="black",cex=1.5, 
     xlim=xLim , ylim=yLim ,
     xlab = xName, ylab = yName
)

# hierarchical linear regression
xComb = seq(xLim[1],xLim[2],length=length(x))
for ( i in 1:length(beta1)) {
  lines( xComb , beta1[i]*xComb , col="skyblue" )
}

# simple linear regression
beta00 = zetaLinear[1] * ysd  + ym - zetaLinear[2] * xm * ysd / xsd
beta11 = zetaLinear[2] * ysd / xsd
lines( xComb , beta00 + beta11*xComb , col="red" )


