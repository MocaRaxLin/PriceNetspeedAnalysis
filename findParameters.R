# All needed function for final project are bellow
# R's working memory, at R's command line type:
# source("findParameters.R")

#------------------------------------------------------------------------------

SimpleLinearReg = function(x, y){
  model = lm(formula = y ~ x)
  beta0 = model$coefficients[[1]]
  beta1 = model$coefficients[[2]]
  plot(x, y)
  abline(model)
  return(c(beta0, beta1))
}

sdConstant = function(x, y){
  # classify data into diverse net speed
  y01 = y[seq(1, length(y), 5)]
  y05 = y[seq(3, length(y), 5)]
  y10 = y[seq(4, length(y), 5)]
  y15 = y[seq(5, length(y), 5)]
  
  # evaluate sd respectively
  sigma01 = sd(y01)
  sigma05 = sd(y05)
  sigma10 = sd(y10)
  sigma15 = sd(y15)
  
  # line as data
  ssxx = c(0.1, 0.5, 1.0, 1.5)
  ssdd = c(sigma01, sigma05, sigma10, sigma15)
  # print(ssdd)
  
  par = SimpleLinearReg(ssxx, ssdd)
  return(par)
}

findGamma = function(gamma_x, m, s){
  para = gammaShRaFromMeanSD(mean = m, sd = s)
  d = dgamma(gamma_x, shape = para$shape, rate = para$rate)
  return (d)
}












