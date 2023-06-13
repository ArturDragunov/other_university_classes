#install.packages('ISLR2')
#install.packages('class')
#install.packages('randomForest')
library(ISLR2) # data
library(MASS) #lda(), qda()
library(randomForest) #classification trees
library(class) # knn
data(College) 
summary(College)
nr = dim(College)[1] # number of observations


hitratknn<-function(observed,predicted)
{tab<-table(observed,predicted)
hitratknn<-sum(diag(tab))/sum(tab)
return(hitratknn)
}

confusionknn<-function(observed,predicted)
{tab<-table(observed,predicted)
return(tab)
}


##########################Datasets preperation
skewed_columns = c('Apps','Accept','Enroll','Top10perc','F.Undergrad','P.Undergrad','Books','Personal','Expend')#skewed variables
#Scenario A: Centering of all variables 
cCollege=as.data.frame(scale(College[,2:18],center=TRUE,scale=FALSE))
cCollege$Private = as.factor(College$Private)
head(cCollege)
#Scenario B: Log-transformation of skewed variables followed by centering of all variables 
logcCollege=College # new df
logcCollege[skewed_columns] = log(logcCollege[skewed_columns]) # log transformed skewed columns
logcCollege=as.data.frame(scale(logcCollege[,2:18],center=TRUE,scale=FALSE)) # scaled everything 
logcCollege$Private = as.factor(College$Private) # added a boolean column
head(logcCollege)
#Scenario C: Standardization of all variables 
sCollege=as.data.frame(scale(College[,2:18],center=TRUE,scale=TRUE))
sCollege$Private = as.factor(College$Private)
head(sCollege)
#Scenario D: Log-transformation of skewed variables followed by standardization of all variables 
logsCollege=College # new df
logsCollege[skewed_columns] = log(logsCollege[skewed_columns]) # log transformed skewed columns
logsCollege=as.data.frame(scale(logsCollege[,2:18],center=TRUE,scale=TRUE)) # scaled everything 
logsCollege$Private = as.factor(College$Private) # added a boolean column
head(logsCollege)

#data sets to work with are cCollege, logcCollege, sCollege, logsCollege

#Model specifications:
#for KNN we are going to pick the best model based on the hit rate and then report also the sensitivity and specificity rates
#for KNN we will limit k up to 100
# for bagging we will build 5000 bootstraps
# for randomforests we will use 5 randomly picked explanatory variables and set number of bootstraps to 5000 

#for confusion matrix it's important to mention that rows are observed values, columns are predicted

# seed is necessary for knn.cv, random forests and bagging to achieve the same results, as last two methods create n bootstrapped models
#randomly + randomforests pick each time random set of X. Knn.cv also has some element of randomness in the script.
#So, each time we execute the command, the result might be slightly different. Thus, to prevent it, we use seed()
knnmax=100 # maximum of 100 knn is set

###########################################Scenario A##########################################
#############
#bagging
############
set.seed(2) 
bag.mod_c=randomForest(Private~.,data=cCollege,mtry=17,ntree=5000,importance=TRUE)

(bag.mod_c_hit = (bag.mod_c$confusion[1,1]+ bag.mod_c$confusion[2,2])/nr) # hit rate 0.9395
(bag.mod_c_sens = bag.mod_c$confusion[2,2]/(bag.mod_c$confusion[2,2]+bag.mod_c$confusion[2,1])) # sensitivity  0.9664
(bag.mod_c_spec = bag.mod_c$confusion[1,1]/(bag.mod_c$confusion[1,1]+bag.mod_c$confusion[1,2])) # specificity 0.8679

#################
#random Forests
################
set.seed(2)
rf.mod_c=randomForest(Private~.,data=cCollege,mtry=5,ntree=5000,importance=TRUE) 

(rf.mod_c_hit = (rf.mod_c$confusion[1,1]+ rf.mod_c$confusion[2,2])/nr) # hit rate 0.9447
(rf.mod_c_sens = rf.mod_c$confusion[2,2]/(rf.mod_c$confusion[2,2]+rf.mod_c$confusion[2,1])) # sensitivity  0.9717
(rf.mod_c_spec = rf.mod_c$confusion[1,1]/(rf.mod_c$confusion[1,1]+rf.mod_c$confusion[1,2])) # specificity 0.8726

#################
#KNN
################
knn_x = cCollege[,1:17]
knn_y = cCollege[,18]
hitrat<-matrix(rep(0,knnmax*2),nrow=knnmax)
set.seed(2)
for (j in 1:knnmax){
  predknn.train<- knn.cv(knn_x,knn_y,k=j)
  hitrat[j,1]<-hitratknn(knn_y,predknn.train)
  hitrat[j,2]<-j}
max = which(hitrat[,1] == max(hitrat[,1]), arr.ind = TRUE)
hitrat[max[1]]

set.seed(2)
(confusion=confusionknn(knn_y,knn.cv(knn_x,knn_y,k=max[1])))
(knn.mod_c_hit=sum(diag(confusion))/sum(confusion)) # 0.9382
(knn.mod_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9699
(knn.mod_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8538

#################
#QDA
################
qda.mod<-qda(Private~.,data=cCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(cCollege$Private,qda.mod$class))
(qda.mod_c_hit=sum(diag(confusion))/sum(confusion)) # 0.9009
(qda.mod_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9699
(qda.mod_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.7170

#################
#LDA
################
lda.mod<-lda(Private~.,data=cCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(cCollege$Private,lda.mod$class))
(lda.mod_c_hit=sum(diag(confusion))/sum(confusion)) # 0.9344
(lda.mod_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9699
(lda.mod_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8396

###########################################Scenario B##########################################
#############
#bagging
############
set.seed(2) 
bag.mod_log_c=randomForest(Private~.,data=logcCollege,mtry=17,ntree=5000,importance=TRUE) 

(bag.mod_log_c_hit = (bag.mod_log_c$confusion[1,1]+ bag.mod_log_c$confusion[2,2])/nr) # hit rate 0.9421
(bag.mod_log_c_sens = bag.mod_log_c$confusion[2,2]/(bag.mod_log_c$confusion[2,2]+bag.mod_log_c$confusion[2,1])) # sensitivity  0.9681
(bag.mod_log_c_spec = bag.mod_log_c$confusion[1,1]/(bag.mod_log_c$confusion[1,1]+bag.mod_log_c$confusion[1,2])) # specificity 0.8726

#################
#random Forests
################
set.seed(2)
rf.mod_log_c=randomForest(Private~.,data=logcCollege,mtry=5,ntree=5000,importance=TRUE)

(rf.mod_log_c_hit = (rf.mod_log_c$confusion[1,1]+ rf.mod_log_c$confusion[2,2])/nr) # hit rate 0.9472
(rf.mod_log_c_sens = rf.mod_log_c$confusion[2,2]/(rf.mod_log_c$confusion[2,2]+rf.mod_log_c$confusion[2,1])) # sensitivity  0.9752
(rf.mod_log_c_spec = rf.mod_log_c$confusion[1,1]/(rf.mod_log_c$confusion[1,1]+rf.mod_log_c$confusion[1,2])) # specificity 0.8726

#################
#KNN
################
set.seed(2)
knn_x = logcCollege[,1:17]
knn_y = logcCollege[,18]
hitrat<-matrix(rep(0,knnmax*2),nrow=knnmax)
set.seed(2)
for (j in 1:knnmax){
  predknn.train<- knn.cv(knn_x,knn_y,k=j)
  hitrat[j,1]<-hitratknn(knn_y,predknn.train)
  hitrat[j,2]<-j}
max = which(hitrat[,1] == max(hitrat[,1]), arr.ind = TRUE)
hitrat[max[1]]

set.seed(2)
(confusion=confusionknn(knn_y,knn.cv(knn_x,knn_y,k=max[1])))
(knn.mod_log_c_hit=sum(diag(confusion))/sum(confusion)) # 0.8520
(knn.mod_log_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9001
(knn.mod_log_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.7217

#################
#QDA
################
qda.mod<-qda(Private~.,data=logcCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(logcCollege$Private,qda.mod$class))
(qda.mod_log_c_hit=sum(diag(confusion))/sum(confusion)) # 0.9395
(qda.mod_log_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9628
(qda.mod_log_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8774

#################
#LDA
################
lda.mod<-lda(Private~.,data=logcCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(logcCollege$Private,lda.mod$class))
(lda.mod_log_c_hit=sum(diag(confusion))/sum(confusion)) # 0.9511
(lda.mod_log_c_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9717
(lda.mod_log_c_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8962


###########################################Scenario C##########################################
#############
#bagging
############
set.seed(2) 
bag.mod_s=randomForest(Private~.,data=sCollege,mtry=17,ntree=5000,importance=TRUE) 

(bag.mod_s_hit = (bag.mod_s$confusion[1,1]+ bag.mod_s$confusion[2,2])/nr) # hit rate 0.9382
(bag.mod_s_sens = bag.mod_s$confusion[2,2]/(bag.mod_s$confusion[2,2]+bag.mod_s$confusion[2,1])) # sensitivity  0.9646
(bag.mod_s_spec = bag.mod_s$confusion[1,1]/(bag.mod_s$confusion[1,1]+bag.mod_s$confusion[1,2])) # specificity 0.8679

#################
#random Forests
################
set.seed(2)
rf.mod_s=randomForest(Private~.,data=sCollege,mtry=5,ntree=5000,importance=TRUE)

(rf.mod_s_hit = (rf.mod_s$confusion[1,1]+ rf.mod_s$confusion[2,2])/nr) # hit rate 0.9434
(rf.mod_s_sens = rf.mod_s$confusion[2,2]/(rf.mod_s$confusion[2,2]+rf.mod_s$confusion[2,1])) # sensitivity  0.9699
(rf.mod_s_spec = rf.mod_s$confusion[1,1]/(rf.mod_s$confusion[1,1]+rf.mod_s$confusion[1,2])) # specificity 0.8726

#################
#KNN
################
set.seed(2)
knn_x = sCollege[,1:17]
knn_y = sCollege[,18]
hitrat<-matrix(rep(0,knnmax*2),nrow=knnmax)
set.seed(2)
for (j in 1:knnmax){
  predknn.train<- knn.cv(knn_x,knn_y,k=j)
  hitrat[j,1]<-hitratknn(knn_y,predknn.train)
  hitrat[j,2]<-j}
max = which(hitrat[,1] == max(hitrat[,1]), arr.ind = TRUE)
hitrat[max[1]]

set.seed(2)
(confusion=confusionknn(knn_y,knn.cv(knn_x,knn_y,k=max[1])))
(knn.mod_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9292
(knn.mod_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9611
(knn.mod_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8443

#################
#QDA
################

qda.mod<-qda(Private~.,data=sCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(sCollege$Private,qda.mod$class))
(qda.mod_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9009
(qda.mod_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9699
(qda.mod_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.7170

#################
#LDA
################

lda.mod<-lda(Private~.,data=sCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(sCollege$Private,lda.mod$class))
(lda.mod_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9344
(lda.mod_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9699
(lda.mod_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8396

###########################################Scenario D##########################################
#############
#bagging
############
set.seed(2) 
bag.mod_log_s=randomForest(Private~.,data=logsCollege,mtry=17,ntree=5000,importance=TRUE) 

(bag.mod_log_s_hit = (bag.mod_log_s$confusion[1,1]+ bag.mod_log_s$confusion[2,2])/nr) # hit rate 0.9395
(bag.mod_log_s_sens = bag.mod_log_s$confusion[2,2]/(bag.mod_log_s$confusion[2,2]+bag.mod_log_s$confusion[2,1])) # sensitivity  0.9664
(bag.mod_log_s_spec = bag.mod_log_s$confusion[1,1]/(bag.mod_log_s$confusion[1,1]+bag.mod_log_s$confusion[1,2])) # specificity 0.8679

#################
#random Forests
################
set.seed(2)
rf.mod_log_s=randomForest(Private~.,data=logsCollege,mtry=5,ntree=5000,importance=TRUE)
(rf.mod_log_s_hit = (rf.mod_log_s$confusion[1,1]+ rf.mod_log_s$confusion[2,2])/nr) # hit rate 0.9447
(rf.mod_log_s_sens = rf.mod_log_s$confusion[2,2]/(rf.mod_log_s$confusion[2,2]+rf.mod_log_s$confusion[2,1])) # sensitivity  0.9717
(rf.mod_log_s_spec = rf.mod_log_s$confusion[1,1]/(rf.mod_log_s$confusion[1,1]+rf.mod_log_s$confusion[1,2])) # specificity 0.8726

#################
#KNN
#################
set.seed(2)
knn_x = logsCollege[,1:17]
knn_y = logsCollege[,18]
hitrat<-matrix(rep(0,knnmax*2),nrow=knnmax)
set.seed(2)
for (j in 1:knnmax){
  predknn.train<- knn.cv(knn_x,knn_y,k=j)
  hitrat[j,1]<-hitratknn(knn_y,predknn.train)
  hitrat[j,2]<-j}
max = which(hitrat[,1] == max(hitrat[,1]), arr.ind = TRUE)
hitrat[max[1]]

set.seed(2)
(confusion=confusionknn(knn_y,knn.cv(knn_x,knn_y,k=max[1])))
(knn.mod_log_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9408
(knn.mod_log_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9770
(knn.mod_log_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8443

#################
#QDA
################

qda.mod<-qda(Private~.,data=logsCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(logsCollege$Private,qda.mod$class))
(qda.mod_log_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9395
(qda.mod_log_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9628
(qda.mod_log_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8774

#################
#LDA
################

lda.mod<-lda(Private~.,data=logsCollege, CV=TRUE) #CV=true makes automatically LOOCV
(confusion<-table(logsCollege$Private,lda.mod$class))
(lda.mod_log_s_hit=sum(diag(confusion))/sum(confusion)) # 0.9511
(lda.mod_log_s_sens=confusion[2,2]/(confusion[2,2]+confusion[2,1])) # 0.9717
(lda.mod_log_s_spec=confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.8962


##################################Compare results in a table###########################



df_frame=data.frame(Model=c('bagg_c','rf_c','knn_c','QDA_c','LDA_c',
                            'bagg_logc','rf_logc','knn_logc','QDA_logc','LDA_logc',
                            'bagg_s','rf_s','knn_s','QDA_s','LDA_s',
                            'bagg_logs','rf_logs','knn_logs','QDA_logs','LDA_logs'),
                    Hit_rate = c(round(bag.mod_c_hit,4),round(rf.mod_c_hit,4),round(knn.mod_c_hit,4),
                                 round(qda.mod_c_hit,4),round(lda.mod_c_hit,4),
                                 
                                 round(bag.mod_log_c_hit,4),round(rf.mod_log_c_hit,4),round(knn.mod_log_c_hit,4),
                                 round(qda.mod_log_c_hit,4),round(lda.mod_log_c_hit,4),
                                 
                                 round(bag.mod_s_hit,4),round(rf.mod_s_hit,4),round(knn.mod_s_hit,4),
                                 round(qda.mod_s_hit,4),round(lda.mod_s_hit,4),
                                 
                                 round(bag.mod_log_s_hit,4),round(rf.mod_log_s_hit,4),round(knn.mod_log_s_hit,4),
                                 round(qda.mod_log_s_hit,4),round(lda.mod_log_s_hit,4)),
                                  
                    Sensitivity = c(round(bag.mod_c_sens,4),round(rf.mod_c_sens,4),round(knn.mod_c_sens,4),
                                    round(qda.mod_c_sens,4),round(lda.mod_c_sens,4),
                                    
                                    round(bag.mod_log_c_sens,4),round(rf.mod_log_c_sens,4),round(knn.mod_log_c_sens,4),
                                    round(qda.mod_log_c_sens,4),round(lda.mod_log_c_sens,4),
                                    
                                    round(bag.mod_s_sens,4),round(rf.mod_s_sens,4),round(knn.mod_s_sens,4),
                                    round(qda.mod_s_sens,4),round(lda.mod_s_sens,4),
                                    
                                    round(bag.mod_log_s_sens,4),round(rf.mod_log_s_sens,4),round(knn.mod_log_s_sens,4),
                                    round(qda.mod_log_s_sens,4),round(lda.mod_log_s_sens,4)),
                    
                    Specificity = c(round(bag.mod_c_spec,4),round(rf.mod_c_spec,4),round(knn.mod_c_spec,4),
                                    round(qda.mod_c_spec,4),round(lda.mod_c_spec,4),
                                    
                                    round(bag.mod_log_c_spec,4),round(rf.mod_log_c_spec,4),round(knn.mod_log_c_spec,4),
                                    round(qda.mod_log_c_spec,4),round(lda.mod_log_c_spec,4),
                                    
                                    round(bag.mod_s_spec,4),round(rf.mod_s_spec,4),round(knn.mod_s_spec,4),
                                    round(qda.mod_s_spec,4),round(lda.mod_s_spec,4),
                                    
                                    round(bag.mod_log_s_spec,4),round(rf.mod_log_s_spec,4),round(knn.mod_log_s_spec,4),
                                    round(qda.mod_log_s_spec,4),round(lda.mod_log_s_spec,4)))


df_frame # prepared table
