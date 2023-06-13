rm(list=ls())
data = read.csv('diabetes.csv',sep = ';')
dim(data)
data=data[apply(data[,1:8],1,function(x) all(x!=0)),] # removed 0 from all columns except outcome
str(data) # checked types of data columns
data = data.matrix(data,rownames.force = NA)

(means = as.vector(colMeans(data))) #vector of Means

#Wishart matrix
install.packages('mvtnorm')
library(mvtnorm)

Qmat <- (as.vector(data[1,])-means)%*%t(as.vector(data[1,])-means)

for(i in 2:nrow(data))
{Qmat <- Qmat + (data[i,]-means)%*%t(data[i,]-means) }
Qmat # Wishart Matrix

# Sample covariance matrix
s= cov(data) # sample covariance
cor = cor(data) # sample correlation
eigen(cor) # eigenvalues of sample correlation matrix. They are all positive.

MuX = c(3, 130, 70, 25, 150, 30, 0.3725, 30, 0.5)

n=nrow(data) # nr of observations

#Find out a value of quadratic form Q, its distribution and the probability, that it will reach the value or higher value.
#Quadratic form Q (with unknown Sigma matrix - real)
(q = n*t(means - MuX)%*%solve(s)%*%(means-MuX)) # 286.3559

# F = (n-p)*q/(p*(n-1)), where n is the number of observations, and p is the number of variables (here 9)
p = dim(data)[2]

(f_val = (n-p)*q/(p*(n-1))) # 31.0575

1-pf(f_val,p,n-p) #~F(p;n-p), where p is still 9. Probability is 0

#Let's recheck it with beta distribution v/(q+v)~beta((v-p+1)/2,p/2)
pbeta((n-1)/(q+n-1),(n-p)/2,p/2) # 4.671264e-39 what is almost 0. So, we can state that the probability of Q to be higher than itself is 0.
########################################################################################################################################################################
#Choose three of the variables (but not the variable Outcome) and
#Find out medians, trimeans, trimmed means, windsorized means (use 5 % observations to be trimmed).
#variables picked are Glucose, Blood Pressure and Skin Thickness (columns 2,3,4)


MED_gl=median(data[,2]) # median for Glucose is 119
MED_bp=median(data[,3]) # median for Blood Pressure is 70
MED_st = median(data[,4]) # median for Skin Thickness is 28.5

#type 2 Trimeans
######################Glucose
(LQ = quantile(data[,2], probs = 0.25, type=2)) #99
(UQ = quantile(data[,2], probs = 0.75, type=2)) # 144
(trimean = (LQ+2*MED_gl+UQ)/4)#120.25

####################Blood Pressure
(LQ = quantile(data[,3], probs = 0.25, type=2)) #62
(UQ = quantile(data[,3], probs = 0.75, type=2)) #78
(trimean = (LQ+2*MED_bp+UQ)/4)#70

###################Skin Thickness
(LQ = quantile(data[,4], probs = 0.25, type=2)) #21
(UQ = quantile(data[,4], probs = 0.75, type=2)) # 36
(trimean = (LQ+2*MED_st+UQ)/4)#28.5

#trimmed means (use 5 % observations to be trimmed)

0.05*n # around 8 from one side and from another
#################Glucose
mean(data[,2],trim = 16.8/n) # 121.2796
###############Blood Pressure
mean(data[,3],trim = 16.8/n) # 70.34211
##############Skin Thickness
mean(data[,4],trim = 16.8/n) # 28.54276

#Windsorized means (use 5 % observations to be trimmed).
install.packages('psych')
library(psych)
####################Glucose
0.05*(n-1) # nr of observations to be trimmed from bottom and from top. 
winsor.mean(data[,2],trim=16.75/(n-1)) # 122.2054
###################Blood Pressure
winsor.mean(data[,3],trim=16.75/(n-1)) # 70.23363
##################Skin Thickness
winsor.mean(data[,4],trim=16.75/(n-1)) # 28.63393


#Find out IQRs, MADs and Gini indices.
#IQR
#####################Glucose
(LQ = quantile(data[,2], probs = 0.25, type=2)) #99
(UQ = quantile(data[,2], probs = 0.75, type=2)) # 144
(IQR = UQ-LQ) # 45

####################Blood Pressure
(LQ = quantile(data[,3], probs = 0.25, type=2)) #62
(UQ = quantile(data[,3], probs = 0.75, type=2)) #78
(IQR = UQ-LQ) # 16

###################Skin Thickness
(LQ = quantile(data[,4], probs = 0.25, type=2)) #21
(UQ = quantile(data[,4], probs = 0.75, type=2)) # 36
(IQR = UQ-LQ) # 15

#MADs  median absolute deviation
###############Glucose
mad(data[,2],constant=1) # 22
###############Blood Pressure
mad(data[,3],constant=1) # 8
###############Skin Thickness
mad(data[,4],constant=1) # 7.5

#GINI
install.packages('ineq')
library(ineq)
############Glucose
Gini(data[,2]) # 0.1421796
###########Blood Pressure
Gini(data[,3]) # 0.09737749
###########Skin Thickness
Gini(data[,4]) # 0.2045246


#################### Use sample means and sample standard deviations as estimates of ?? and ?? of normal distributions.
(MuGL=mean(data[,2])) # 122.2798  # estimate of mu
(stdevGL = sd(data[,2])) # 30.78465 # estimate of sigma

(MuBP=mean(data[,3])) #  70.24405
(stdevBP = sd(data[,3])) # 12.3634

(MuST=mean(data[,4])) # 28.66369
(stdevST = sd(data[,4])) # 10.24986

#Draw a sample quantile functions, empirical distribution functions and kernel density estimates with histograms
#and compare them (graphically) to the theoretical functions of estimated normal distributions (from previous point). 

###############################Glucose
# sample quantile function
x=seq(0.1,1,0.01)

plot(x,qnorm(x,MuGL,stdevGL),col='red',type='l')# estimated assuming normal distribution
lines(x,quantile(data[,2], probs =x, type=2),col='blue') # real

#Plotting empirical distribution functions CDFs
plot(ecdf(data[,2])) # empirical CDF -> follows the values
x=seq(40,210,0.01)
lines(x,pnorm(x,MuGL,stdevGL),col='red')  # estimated N(estimated mean and estimated sigma). I say here that the data is following normal distribution. Give me the best estimate


#plotting densities
hist(data[,2],breaks=15,freq=FALSE,main = 'Histogram of Glucose')#we chose 15 bins. Recalibrate the y axis, so the total area =1 

#kernel density estimates with histograms
lines(density(data[,2]),col='orange',lwd=2) # we get density in the smooth way. Kernel density of the estimate
lines(x,dnorm(x,MuGL,stdevGL) ,col='blue') # estimated density with parameters
###############################Blood Pressure
# sample quantile function
x=seq(0.1,1,0.01)

plot(x,qnorm(x,MuBP,stdevBP),col='red',type='l')# estimated assuming normal distribution
lines(x,quantile(data[,3], probs =x, type=2),col='blue') # real

#Plotting empirical distribution functions CDFs
plot(ecdf(data[,3])) # empirical CDF -> follows the values
x=seq(20,120,0.01)
lines(x,pnorm(x,MuBP,stdevBP),col='red')  # estimated N(estimated mean and estimated sigma). I say here that the data is following normal distribution. Give me the best estimate


#plotting densities
hist(data[,3],breaks=15,freq=FALSE,main = 'Histogram of Blood Pressure')#we chose 15 bins. Recalibrate the y axis, so the total area =1 

#kernel density estimates with histograms
lines(density(data[,3]),col='orange',lwd=2) # we get density in the smooth way. Kernel density of the estimate
lines(x,dnorm(x,MuBP,stdevBP) ,col='blue') # estimated density with parameters

###############################Skin Thickness
# sample quantile function
x=seq(0.1,1,0.01)

plot(x,qnorm(x,MuST,stdevST),col='red',type='l')# estimated assuming normal distribution
lines(x,quantile(data[,4], probs =x, type=2),col='blue') # real

#Plotting empirical distribution functions CDFs
plot(ecdf(data[,4])) # empirical CDF -> follows the values
x=seq(0,60,0.01)
lines(x,pnorm(x,MuST,stdevST),col='red')  # estimated N(estimated mean and estimated sigma). I say here that the data is following normal distribution. Give me the best estimate


#plotting densities
hist(data[,4],breaks=15,freq=FALSE,main = 'Histogram of Skin Thickness')#we chose 15 bins. Recalibrate the y axis, so the total area =1 

#kernel density estimates with histograms
lines(density(data[,4]),col='orange',lwd=2) # we get density in the smooth way. Kernel density of the estimate
lines(x,dnorm(x,MuST,stdevST) ,col='blue') # estimated density with parameters

##################################################################################################################################################################

#Use the Kolmogorov-Smirnov and X2 GOF test in order to find out whether the distributions are drawn from the estimated normal distributions.

###############################Kolmogorov-Smirnov
#H0: Random sample is from random variable with the pre-specified continuous CDF.
#H1: non H0

########################################Glucose

ks.test(data[,2],'pnorm',MuGL,stdevGL) # p-value <0.05. We assume that Glucose doesn't come from ~N(MuGL,stdevGL). In other words,
#Glucose doesn't come from Normal distribution with these particular parameters

#######################################Blood Pressure

ks.test(data[,3],'pnorm',MuBP,stdevBP) # p-value =0.1325 > 0.05. We assume that Blood Pressure comes from ~N(MuBP,stdevBP).
#In other words, Blood Pressure comes from Normal distribution with these particular parameters

########################################Skin Thickness

ks.test(data[,4],'pnorm',MuST,stdevST) # p-value=0.2377 > 0.05. We assume that Skin Thickness comes from ~N(MuST,stdevST).
#In other words, Skin Thickness comes from Normal distribution with these particular parameters

################################################X2 GOF test

##Testing Chi-squared test
#Divide H0 distribution into categories 
# best fit
#H0:X~N
#H1:non H0
1+3.3*log(n,base = 10) # 10 categories
#10 categories from N(Mu, Sigma) -> empirical
#######################Glucose

B1 = qnorm(1/10,MuGL,stdevGL)
B2 = qnorm(2/10,MuGL,stdevGL) 
B3 = qnorm(3/10,MuGL,stdevGL) 
B4 = qnorm(4/10,MuGL,stdevGL) 
B5 = qnorm(5/10,MuGL,stdevGL) 
B6 = qnorm(6/10,MuGL,stdevGL) 
B7 = qnorm(7/10,MuGL,stdevGL) 
B8 = qnorm(8/10,MuGL,stdevGL) 
B9 = qnorm(9/10,MuGL,stdevGL) 


cat_data=cut(data[,2], breaks=c(-Inf,B1,B2,B3, B4, B5, B6,B7,B8,B9, Inf))
#H0=P1=1/10,P2=1/10...P10=1/10 -> probability is 1/10 to be in the cut
table(cat_data)

(CHSQ = chisq.test(table(cat_data)))
# test crit = 37.75. P(X^2>=37.75) = 1-F(37.75)~X^2(K-1-2), where K=10 -> number of categories obtained above
#1-F(37.75) = 3.380441e-06 < 0.05. We reject H0. We reject the hypothesis that Glucose comes from Normal distribution with these particular parameters
1-pchisq(CHSQ$statistic,(10-1-2)) #3.380441e-06 . We are recalibrating. We needed to change degrees of freedom from k-1 to k-1-2 (2 is the nr of parameters (mu, sigma))


#######################Blood Pressure

B1 = qnorm(1/10,MuBP,stdevBP)
B2 = qnorm(2/10,MuBP,stdevBP) 
B3 = qnorm(3/10,MuBP,stdevBP) 
B4 = qnorm(4/10,MuBP,stdevBP) 
B5 = qnorm(5/10,MuBP,stdevBP) 
B6 = qnorm(6/10,MuBP,stdevBP) 
B7 = qnorm(7/10,MuBP,stdevBP) 
B8 = qnorm(8/10,MuBP,stdevBP) 
B9 = qnorm(9/10,MuBP,stdevBP) 


cat_data=cut(data[,3], breaks=c(-Inf,B1,B2,B3, B4, B5, B6,B7,B8,B9, Inf))
#H0=P1=1/10,P2=1/10...P10=1/10 -> probability is 1/10 to be in the cut
table(cat_data)

(CHSQ = chisq.test(table(cat_data)))
# test crit = 18.405. P(X^2>=18.405) = 1-F(18.405)~X^2(K-1-2), where K=10 -> number of categories obtained above
#1-F(18.405) = 0.01027133 <0.05 . We reject H0. We reject the hypothesis that Blood Pressure comes from Normal distribution with these particular parameters
1-pchisq(CHSQ$statistic,(10-1-2)) #0.01027133 . We are recalibrating. We needed to change degrees of freedom from k-1 to k-1-2 (2 is the nr of parameters (mu, sigma))

#######################Skin Thickness

B1 = qnorm(1/10,MuST,stdevST)
B2 = qnorm(2/10,MuST,stdevST) 
B3 = qnorm(3/10,MuST,stdevST) 
B4 = qnorm(4/10,MuST,stdevST) 
B5 = qnorm(5/10,MuST,stdevST) 
B6 = qnorm(6/10,MuST,stdevST) 
B7 = qnorm(7/10,MuST,stdevST) 
B8 = qnorm(8/10,MuST,stdevST) 
B9 = qnorm(9/10,MuST,stdevST) 


cat_data=cut(data[,4], breaks=c(-Inf,B1,B2,B3, B4, B5, B6,B7,B8,B9, Inf))
#H0=P1=1/10,P2=1/10...P10=1/10 -> probability is 1/10 to be in the cut
table(cat_data)

(CHSQ = chisq.test(table(cat_data)))
# test crit = 12.452. P(X^2>=12.452) = 1-F(12.452)~X^2(K-1-2), where K=10 -> number of categories obtained above
#1-F(12.452) = 0.08662958>0.05 . We can't reject H0. We can't reject the hypothesis that Skin Thickness comes from Normal distribution with these particular parameters
1-pchisq(CHSQ$statistic,(10-1-2)) #0.08662958. We are recalibrating. We needed to change degrees of freedom from k-1 to k-1-2 (2 is the nr of parameters (mu, sigma))
