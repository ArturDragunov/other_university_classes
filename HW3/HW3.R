install.packages('EnvStats')
library('EnvStats')
rm(list=ls())
# Selected arbitrarily
alpha = 4
theta =2
x=seq(2.1,10,by=0.01)
fx=(alpha*theta^alpha)/(x^(alpha+1)) #density
Fx=1-(theta/x)^alpha #CDF


#Bullet 2: Calculate and highlight expected value and median in the plots above. Calculate standard deviation.
(mean = alpha*theta/(alpha-1)) #E(x) 
sqrt(alpha*theta^2/(((alpha-1)^2)*(alpha-2))) #standard deviation 0.942809
(median=32^(1/alpha)) #median: F(Xp)=p, 1-(2/x)^4=0.5; 1-16/x^4 = 0.5|*x^4; x^4-16=0.5*x^4;0.5x^4=16; x^4=32; x=32^(1/4)

#Bullet 1: Plot the density and the cumulative distribution function of your selected distribution
plot(x,fx,col='blue')
abline(v=median, col='red')
abline(v=mean, col='green')

plot(x,Fx,col='blue')
abline(v=median, col='red')
abline(v=mean, col='green')

#Bullet 3: Plot densities of the minimum values (Y1 ) and maximum values (Yn ) in samples of sizes 10, 20, 30, 40 and 50.

#for minimas

n=10
dens1=n*fx*(1-Fx)^(n-1)
plot(x,dens1,type='l',col='red')

n=20
dens2=n*fx*(1-Fx)^(n-1)
lines(x,dens2,type='l',col='orange')

n=30
dens3=n*fx*(1-Fx)^(n-1)
lines(x,dens3,type='l',col='blue')

n=40
dens4=n*fx*(1-Fx)^(n-1)
lines(x,dens4,type='l',col='green')

n=50
dens5=n*fx*(1-Fx)^(n-1)
lines(x,dens5,type='l',col='purple')

#for maximas

n=10
dens1=n*fx*(Fx)^(n-1)
plot(x,dens1,type='l',col='red')

n=20
dens2=n*fx*(Fx)^(n-1)
lines(x,dens2,type='l',col='orange')

n=30
dens3=n*fx*(Fx)^(n-1)
lines(x,dens3,type='l',col='blue')

n=40
dens4=n*fx*(Fx)^(n-1)
lines(x,dens4,type='l',col='green')

n=50
dens5=n*fx*(Fx)^(n-1)
lines(x,dens5,type='l',col='purple')

#bullet 4: Compare the medians of X (selected distribution), Y1 and Yn (both for sample sizes from previous points ??? arrange in a table).
#you can calculate it in 2 ways. a) manually get Q function -> inverse of CDF. and then calculate by hand. See 1 example in PDF. 
#b) another way is to use quantile function from package EnvStats qpareto(p,theta,alpha) and get the same results.
n=10
med1min = 2*(1-(1-0.5^(1/n)))^(-1/4)
med1max = 2*(1-0.5^(1/n))^(-1/4)
n=20
med2min = qpareto(1-0.5^(1/n),2,4)
med2max = qpareto(0.5^(1/n),2,4)

n=30
med3min = qpareto(1-0.5^(1/n),2,4)
med3max = qpareto(0.5^(1/n),2,4)

n=40
med4min = qpareto(1-0.5^(1/n),2,4)
med4max = qpareto(0.5^(1/n),2,4)

n=50
med5min = qpareto(1-0.5^(1/n),2,4)
med5max = qpareto(0.5^(1/n),2,4)

df = data.frame(Sample_size = c(10,20,30,40,50),X=rep(median,5),Y1=c(med1min,med2min,med3min,med4min,med5min),Yn=c(med1max,med2max,med3max,med4max,med5max))
df
#Bullet 5: Suppose ?? to be known (and equal to your selected value).
n=length(x)
#Evaluate maximum likelihood estimate of ??. 
(mle = n/(sum(log(x))-n*log(theta)))
#Evaluate the moment estimate of ??
(moment = mean(x)/(mean(x)-theta))
#Evaluate the quantile estimate of ??
(qe = -log(1-0.5)/(log(median(x)/theta)))






