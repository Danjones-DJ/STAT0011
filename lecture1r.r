rm(list=ls())

#returns params of beta distribtuion in terms of mean/variance
betaParams <- function(mu, sigma2) {
  alpha <- ( (1-mu)/sigma2 - 1/mu)*mu^2
  beta <- alpha*(1/mu - 1)
  return(c(alpha,beta))
}

mujohn <- 0.5
varjohn <- 0.03^2
temp  <- betaParams(mujohn,varjohn)
alphajohn <- temp[1]; betajohn <- temp[2]


musarah <- 0.5
varsarah <- 0.15^2
temp  <- betaParams(musarah,varsarah)
alphasarah <- temp[1]; betasarah <- temp[2]

alphaunif <- 1
betaunif <- 1

xaxis <- seq(0,1,length=1000)

plot(xaxis, dbeta(xaxis, alphajohn, betajohn),main='John Prior: Beta(138.4, 138.4)',type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,')')))
plot(xaxis, dbeta(xaxis, alphasarah, betasarah),main='Sarah Prior: Beta(5.01, 5.01)',type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,')')))
plot(xaxis, dbeta(xaxis, alphaunif, betaunif),main='Uniform Prior: Beta(1,1)', type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,')')))



#now the posteior
N <- 100
Y <- 48

gammajohn <- Y + alphajohn; lambdajohn <- N-Y+betajohn
gammasarah <- Y + alphasarah; lambdasarah <- N-Y+betasarah
gammaunif <- Y + alphaunif; lambdaunif <- N-Y+betaunif


plot(xaxis, dbeta(xaxis, gammajohn, lambdajohn),main='John Prior (black) and Beta(186.4, 190.4) Posterior (red)',col='red',type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,'|Y)')))
lines(xaxis,  dbeta(xaxis, alphajohn, betajohn),col='black')


plot(xaxis, dbeta(xaxis, gammasarah, lambdasarah),main='Sarah Prior (black) and  Beta(53.1, 57.1) Posterior (red)',col='red',type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,'|Y)')))
lines(xaxis,  dbeta(xaxis, alphasarah, betasarah),col='black')


plot(xaxis, dbeta(xaxis, gammaunif, lambdaunif),main='Uniform Prior (black) and Beta(49, 53) Posterior (red)',col='red',type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,'|Y)')))
lines(xaxis,  dbeta(xaxis, alphaunif, betaunif),col='black')


plot(xaxis, dbeta(xaxis, gammajohn, lambdajohn),main="Posteriors: John (black), Sarah (red), Uniform (blue)", type='l',xlab=expression(theta), ylab=expression(paste('p(',theta,')')))
lines(xaxis,  dbeta(xaxis, gammasarah, lambdasarah),col='red')
lines(xaxis,  dbeta(xaxis, gammaunif, lambdaunif),col='blue')
