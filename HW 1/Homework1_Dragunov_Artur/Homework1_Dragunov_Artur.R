###Exercise 1###
rm(list=ls()) # cleaned global environment

set.seed(1)
mu = runif(1, 1, 3.5) # generated a random number between 1 and 3.5
set.seed(80)
sigma = sqrt(runif(1, 1, 3.5)) # generated random nr between 1:3.5 and took square root to get sigma
#Draw the density, the CDF and the quantile function. 
x <- 0:10
density <- dlnorm(x, mu, sigma)
plot(x,density,type = 'l') # density function plotted

cdf = plnorm(x,mu,sigma)
plot(x,cdf,type='l') # CDF function plotted

p = seq(0,1,0.01) # probabilities
Qp = qlnorm(p,mu,sigma) # quantiles based on probabilities
x=0:100
plot(x,Qp, type='l') # quantile function plotted

#Evaluate median, quartiles, expected value, mode, variance and standard deviation. 
(median = qlnorm(0.5,mu,sigma)) # 5.2792 exp(mu) -> another way of getting median 
(quartile025 = qlnorm(0.25,mu,sigma)) # 25% quartile = 1.9869
(quartile075 = qlnorm(0.75,mu,sigma)) # 75% quartile = 14.0269
(E_x = exp(mu+ sigma^2/2)) # expected value 15.07876
(Mod_x= exp(mu-sigma^2)) # mode = 0.647
(Var_x = exp(2*mu+sigma^2)*(exp(sigma^2)-1)) # variance = 1627.565
(stdev = sqrt(Var_x)) # standard deviation 40.343

#Generate 50, 100 and 200 independent random numbers from selected distributions (hence 3 samples). For every sample:
r_50=rlnorm(50,mu,sigma)
r_100=rlnorm(100,mu,sigma)
r_200=rlnorm(200,mu,sigma)

#Find the mean, median, sample (!) variance, sample standard deviation and sample quartiles.
#for 50 numbers
mean(r_50) # mean
median(r_50) # median
var(r_50) # sample variance
sd(r_50) # sample stdev
quantile(r_50,p=0.25) # sample quartile 0.25
quantile(r_50,p=0.75) # sample quartile 0.75

# for 100 numbers
mean(r_100) # mean
median(r_100) # median
var(r_100) # sample variance
sd(r_100) # sample stdev
quantile(r_100,p=0.25) # sample quartile 0.25
quantile(r_100,p=0.75) # sample quartile 0.75

# for 200 numbers

mean(r_200) # mean
median(r_200) # median
var(r_200) # sample variance
sd(r_200) # sample stdev
quantile(r_200,0.25) # sample quartile 0.25
quantile(r_200,0.75) # sample quartile 0.75

#Construct a table of theoretical and sample values (sample counterparts) in order to illustrate
#relationship between theoretical and sample characteristics (they should be comparableâ but not equivalent).

df_frame = data.frame(Number = c('Theoretical',50,100,200),Mean = c(E_x, mean(r_50),mean(r_100),mean(r_200)),
           Median = c(median,median(r_50),median(r_100),median(r_200)),Varience = c(Var_x,var(r_50),var(r_100),var(r_200)),
           SD = c(stdev,sd(r_50),sd(r_100),sd(r_200)),Low_quartile = c(quartile025,quantile(r_50,p=0.25),quantile(r_100,p=0.25),quantile(r_200,p=0.25)),
           Up_quartile = c(quartile075,quantile(r_50,p=0.75),quantile(r_100,p=0.75),quantile(r_200,p=0.75)))
df_frame # constructed table. 

#Draw histograms and add the theoretical (log-normal) density function into them. 
#for 50
hist(r_50,freq=FALSE,breaks=10)
lines(0:max(r_50),dlnorm(0:max(r_50),mu,sigma),col='Blue') #where 0:max(r_50) = x axis, dlnorm(0:max(r_50),mu,sigma) - y axis. Blue line is log-normal density function

#for 100
hist(r_100,freq=FALSE,breaks=10)
lines(0:max(r_100),dlnorm(0:max(r_100),mu,sigma),col='Blue')


#for 200

hist(r_200,freq=FALSE,breaks=10)
lines(0:max(r_200),dlnorm(0:max(r_200),mu,sigma),col='Blue')
#########################################################################################################################################################################



###Exercise 2###



rm(list=ls()) # cleaned global environment


#Beta distribution is very flexible in the shape of its density function, we can model well very 
#different distributions. Select 5 pairs of parameters:
#alfa,beta>1
alfa1 = 1.2
beta1=1.5
#alfa<1,beta>1
alfa2=0.8
beta2=1.5
#alfa=1,beta>1
alfa3=1
beta3=1.5
#alfa,beta<1
alfa4=0.8
beta4=0.6
#alfa=1,beta<1
alfa5=1
beta5=0.8
#Draw densities and cumulative distribution functions of these distributions. 
x=seq(0,1,0.001)

#densities functions plotted in 1 plot
plot(x,dbeta(x,alfa1,beta1),type='l')
lines(x,dbeta(x,alfa2,beta2),col='red')
lines(x,dbeta(x,alfa3,beta3),col='blue')
lines(x,dbeta(x,alfa4,beta4),col='green')
lines(x,dbeta(x,alfa5,beta5),col='purple')

#CDF functions plotted in 1 plot
plot(x,pbeta(x,alfa1,beta1),type='l')
lines(x,pbeta(x,alfa2,beta2),col='red')
lines(x,pbeta(x,alfa3,beta3),col='blue')
lines(x,pbeta(x,alfa4,beta4),col='green')
lines(x,pbeta(x,alfa5,beta5),col='purple')

#Evaluate the expected values, medians and modes (or antimodes).
#for alfa,beta>1
alfa1/(alfa1+beta1) # expected value 0.444
qbeta(0.5,alfa1,beta1)#median 0.4290
(alfa1-1)/(alfa1+beta1-2) #mode 0.2857
#alfa<1,beta>1
alfa2/(alfa2+beta2) # expected value 0.3478
qbeta(0.5,alfa2,beta2)#median 0.2987
(alfa2-1)/(alfa2+beta2-2) #mode -0.6667
#alfa=1,beta>1
alfa3/(alfa3+beta3) # expected value 0.4
qbeta(0.5,alfa3,beta3)#median 0.3700
(alfa3-1)/(alfa3+beta3-2) #mode 0
#alfa,beta<1
alfa4/(alfa4+beta4) # expected value 0.5714
qbeta(0.5,alfa4,beta4)#median 0.6119
(alfa4-1)/(alfa4+beta4-2) #antimode 0.3333
#alfa=1,beta<1
alfa5/(alfa5+beta5) # expected value 0.5555
qbeta(0.5,alfa5,beta5)#median 0.5796
(alfa5-1)/(alfa5+beta5-2) #antimode 0 

#############################################################################################################################################################################################

###Exercise 3###


rm(list=ls()) # cleaned global environment




#Gamma distr. (k=2, theta = 5)
k=2
theta = 5
#n=25,50,100
#Convolution - summation of independent random variables. For Gamma(k1,theta)+Gamma(k2,theta)=Gamma(k1+k2,theta)
#in other words Y=X1+X2...Xn. Theta is the same, we sum only k.
k1 = k*25 # Gamma~(50,5)
k2=k*50 # Gamma~(100,5)
k3 = k*100 # Gamma~(200,5)

#the expected value, median, mode, lower and upper quartiles, variance, standard deviation,
#quartile deviation and the coefficient of skewness. 

###for Gamma~(50,5)###
(E_x1=k1*theta) # expected value = 250
(median_x1=qgamma(0.5,k1,1/theta)) #median 248.3353
(mod_x1=(k1-1)*theta) # mod 245
(lowq_x1=qgamma(0.25,k1,1/theta))#lower quartile 225.33
(upq_x1=qgamma(0.75,k1,1/theta))#upper quartile 272.8531
(var_x1=k1*theta^2) #variance 1250
(stdev_x1=sqrt(var_x1)) # standard deviation 35.355
(q_dev_x1=(upq_x1-lowq_x1)/2)#quartile deviation 23.76003
(c_v_x1=2/sqrt(k1))#coefficient of skewness 0.283. 2/sqrt(k)

###for Gamma~(100,5)###
(E_x2=k2*theta) # expected value = 500
(median_x2=qgamma(0.5,k2,1/theta)) #median 498.3343
(mod_x2=(k2-1)*theta) # mod 495
(lowq_x2=qgamma(0.25,k2,1/theta))#lower quartile  465.4292
(upq_x2=qgamma(0.75,k2,1/theta))#upper quartile 532.7555
(var_x2=k2*theta^2) #variance 2500
(stdev_x2=sqrt(var_x2)) # standard deviation 50
(q_dev_x2=(upq_x2-lowq_x2)/2)#quartile deviation 33.66315
(c_v_x2=2/sqrt(k2))#coefficient of skewness 0.2

###for Gamma~(200,5)###
(E_x3=k3*theta) # expected value = 1000
(median_x3=qgamma(0.5,k3,1/theta)) #median 998.3338
(mod_x3=(k3-1)*theta) # mod 995
(lowq_x3=qgamma(0.25,k3,1/theta))#lower quartile 951.4417
(upq_x3=qgamma(0.75,k3,1/theta))#upper quartile 1046.742
(var_x3=k3*theta^2) #variance 5000
(stdev_x3=sqrt(var_x3)) # standard deviation 70.71068
(q_dev_x3=(upq_x3-lowq_x3)/2)#quartile deviation 47.65026
(c_v_x3=2/sqrt(k3))#coefficient of skewness 0.1414

"Select (small) epsilon>0 and evaluate P(M<=E(M)),P(M<=mode(M)) and P(|M-E(M)|<=epsilon)"
epsilon = 0.25

# P(M<=E(M)). CDF
(P1.1=pgamma(E_x1,k1,1/theta)) # 0.5188 for Gamma~(50,5)
(P2.1=pgamma(E_x2,k2,1/theta)) # 0.5133 for Gamma~(100,5)
(P3.1=pgamma(E_x3,k3,1/theta)) # 0.5094 for Gamma~(200,5)

#P(M<=mode(M)) CDF
(P1.2=pgamma(mod_x1,k1,1/theta)) # 0.4621 for Gamma~(50,5)
(P2.2=pgamma(mod_x2,k2,1/theta)) # 0.4733 for Gamma~(100,5)
(P3.2=pgamma(mod_x3,k3,1/theta)) # 0.4812 for Gamma~(200,5)

#P(|M-E(M)|<=epsilon)
(P1.3=pgamma(E_x1+epsilon,k1,1/theta)-pgamma(E_x1-epsilon,k1,1/theta)) # 0.005632456 for Gamma~(50,5)
(P2.3=pgamma(E_x2+epsilon,k2,1/theta)-pgamma(E_x2-epsilon,k2,1/theta)) # 0.003986083 for Gamma~(100,5)
(P3.3=pgamma(E_x3+epsilon,k3,1/theta)-pgamma(E_x3-epsilon,k3,1/theta)) # 0.002819767  for Gamma~(200,5)

#Use the central limit theorem to approximate the distribution of M (for n = 50 and 100). 
#Evaluate same characteristics and probabilities as in previous two points.


#for n=50
k1#corresponds to n=50
(Ex_norm1=k1*1/theta) #10. in normal distribution Mu is the point of symmetry, thus median and mode are equal to E(X)
(var_norm1=k1*1/theta^2) # 2 variance
(stdev_norm1=sqrt(var_norm1))#1.414214 standard deviation
(lowq_norm1=qnorm(0.25,Ex_norm1,stdev_norm1)) #9.046127 lower quantile
(upq_norm1=qnorm(0.75,Ex_norm1,stdev_norm1))#10.95387 upper quantile
(q_dev_norm1=(upq_norm1-lowq_norm1)/2) #0.9538726 quartile deviation
(c_v_norm1=0)#normal distribution has 0 skewness


#for n=100
k2#corresponds to n=100

(Ex_norm2=k2*1/theta) #20. in normal distribution Mu is the point of symmetry, thus median and mode are equal to E(X)
(var_norm2=k2*1/theta^2) # 4 variance
(stdev_norm2=sqrt(var_norm2))#2 standard deviation
(lowq_norm2=qnorm(0.25,Ex_norm2,stdev_norm2)) # 18.65102 lower quantile
(upq_norm2=qnorm(0.75,Ex_norm2,stdev_norm2))#21.34898 upper quantile
(q_dev_norm2=(upq_norm2-lowq_norm2)/2) #1.34898 quartile deviation
(c_v_norm2=0)#normal distribution has 0 skewness




#for n=50
# P(M<=E(M)) and #P(M<=mode(M)) the same CDF
(P_clt1.1=pnorm(Ex_norm1,Ex_norm1,stdev_norm1)) # 0.5 for E(x) and for Mode(X)

#P(|M-E(M)|<=epsilon)
(P_clt1.3=pnorm(Ex_norm1+epsilon,Ex_norm1,stdev_norm1)-pnorm(Ex_norm1-epsilon,Ex_norm1,stdev_norm1)) #0.005641849

#for n=100
# P(M<=E(M)) and #P(M<=mode(M)) the same CDF
(P_clt2.1=pnorm(Ex_norm2,Ex_norm2,stdev_norm2)) # 0.5 for E(x) and for Mode(X)

#P(|M-E(M)|<=epsilon)
(P_clt2.3=pnorm(Ex_norm2+epsilon,Ex_norm2,stdev_norm2)-pnorm(Ex_norm2-epsilon,Ex_norm2,stdev_norm2)) # 0.003989406

#Compare results in a table.
df_frame_ex3=data.frame(Calculations=c('expected value','median', 'mode', 'lower quartile','upper quartile',
'variance', 'standard deviation', 'quartile deviation','coefficient of skewness','P(M<=E(M))','P(M<=mode(M))',
'P(|M-E(M)|<=epsilon)'),Gamma50=c(round(E_x1,4),round(median_x1,4),round(mod_x1,4),round(lowq_x1,4),round(upq_x1,4),round(var_x1,4),round(stdev_x1,4),round(q_dev_x1,4),
                                  round(c_v_x1,4),round(P1.1,4),round(P1.2,4),round(P1.3,4)),
Gamma100=c(round(E_x2,4),round(median_x2,4),round(mod_x2,4),round(lowq_x2,4),round(upq_x2,4),round(var_x2,4),round(stdev_x2,4),round(q_dev_x2,4),round(c_v_x2,4),round(P2.1,4),
           round(P2.2,4),round(P2.3,4)),
Gamma200=c(round(E_x3,4),round(median_x3,4),round(mod_x3,4),round(lowq_x3,4),round(upq_x3,4),round(var_x3,4),round(stdev_x3,4),round(q_dev_x3,4),round(c_v_x3,4),
           round(P3.1,4),round(P3.2,4),round(P3.3,4)),
CLT50=c(round(Ex_norm1,4),round(Ex_norm1,4),round(Ex_norm1,4),round(var_norm1,4),round(stdev_norm1,4),round(lowq_norm1,4),round(upq_norm1,4),round(q_dev_norm1,4),
        round(c_v_norm1,4),round(P_clt1.1,4),round(P_clt1.1,4),round(P_clt1.3,4)),
CLT100=c(round(Ex_norm2,4),round(Ex_norm2,4),round(Ex_norm2,4),round(var_norm2,4),round(stdev_norm2,4),round(lowq_norm2,4),round(upq_norm2,4),round(q_dev_norm2,4),round(c_v_norm2,4),
         round(P_clt2.1,4),round(P_clt2.1,4),round(P_clt2.3,4)))

df_frame_ex3 # prepared table
########################################################################################################################################################################################






###Exercise 4###


rm(list=ls()) # cleaned global environment


n=1000
p = 0.03 # probability of a faulty product
(e_x=p*n) # expected value = 30
(var_x= (p*(1-p))/n) # 2.91e-05

#Find out (using Chebyshev inequality and then CLT) the probability, that the relative frequency P
#of faulty products in the sample will diverge from its expected value 0.03 by 0.01 at max.
#P(|X-E(X)|<=0.01)
1-var_x/(0.01^2) # 0.709 Chebyshev lower bound


#for CLT is just prob for normal dist

#P(0.03 + 0.01) - P(0.03 - 0.01) = F(40)-F(20)

pnorm(40, 30, sqrt(30))-pnorm(20,30, sqrt(30)) #0.9321108

#Use Chebyshev inequality and CLT to find out the sample sizes (n1 and n2) needed for this to hold true: 
#P(|P-0.03|<=0.01)>=0.8

#solution by using Chebyshew inequality

#1-var_x/0.01^2 = 0.8. var_x=p*(1-p)/n. 1-p*(1-p)/n/0.01^2=0.8. 1-0.03*0.97/n/0.01^2=0.8. -0.2=-0.03*0.97/n/0.01^2. 0.2/0.03*0.97=0.01^2/n.

#0.2=var_x/0.01^2; 0.2n=n*var_x/0.01^2. 0.2n=0.03*0.97/0.01^2. n=0.03*0.97/0.01^2/0.2

n=(0.03*0.97/0.01^2)/0.2
n#n=1455-to have 0.8 as the lower bound, n should be chosen to be 1455

#solution by using CLT

n=470:500
mu_normal = n*0.03
sigma_normal = sqrt(n*0.03*(1-0.03))

pnorm(0.03*n+0.01*n,mu_normal,sigma_normal)-pnorm(0.03*n-0.01*n,mu_normal,sigma_normal)
#n=478 with CLT


#Find out the sample size (n3) needed for the standard deviation of relative frequency of faulty products to be 0.01 at max

"SOLUTION: Var(x)=p*(1-p)/n. Standard deviation=var(x)^(1/2). Let's increase both sides by power 2-> 0.03*0.97/n=0.0001 -> n=0.03*0.97/0.0001
n=291 is the sample size needed for standard deviation to be 0.01 at max"



#Find out the sample size (n4) needed for the sample to contain at least 1,000 non-faulty products with the probability at least 0.9
pbinom(1000,1000:1025,1-p) # n >= 1025 

#Find out the sample size (n5) needed for the expected value of non-faulty products to be at least 1,000.
"SOLUTION:  E(X) >=1000. E(X) = n*p, where n = ?, and p of non-faulty = 1-p of faulty, thus 0.97. So, n*0.97 >=1000. n>= 1000/0.97. n >= 1031
ANSWER: n should be at least 1031, so that expected value of non-faulty products were >=1000."


