library(tree)
library(DAAG)
library(rpart)
library(ipred)
library(rpart.plot)
library(datasets)
colnames(spam7)
################
### Impuret Ìe ###
################
#entropie
phi1=function(p) -p*log(p)-(1-p)*log(1-p) #gini
phi2=function(p) 2*p-2*p^2
#mauvaise classif
phi3=function(p) p
phi4=function(p) 1-p
curve(phi1,from = 0,to=1,col="red",ylab="",xlab=expression(p))
curve(phi2,from = 0,to=1,col="blue",add=TRUE)
curve(phi3,from = 0,to=0.5,col="green",ylim=c(0,0.7),add=TRUE)
curve(phi4,from = 0.5,to=1,col="green",ylim=c(0,0.7),add=TRUE)
legend("topright",text.col  = c("red","blue","green"),\\
       legend = c("Entropie","Gini","erreur de mauvaise classification"))

##########################################
### Construction de lâ€™arbre avec rpart ###
##########################################
N=dim(spam7)[1]
ntrain=floor(2/3*N)
ntest=N-ntrain
Ind=sample(1:4601,ntrain,replace=FALSE)
spam.train=spam7[Ind,]
spam.test=spam7[-Ind,]
spam.rpart <- rpart(formula = yesno~.,method="class",control=rpart.control(cp=0.001), data=spam.train)
printcp(spam.rpart) #resum Ìe des erreurs
plotcp(spam.rpart)
prp(spam.rpart,extra=1)
###################################
#### Meilleur sous arbre e Ìlagu Ìe ### ##################################
pruned.tree = prune(spam.rpart,cp=0.0029) 

prp(pruned.tree,extra=1)
##################
### Test de prÃ©diction ###
## Avec l'arbre complet
pred=predict(object = spam.rpart,spam.test,type="class")
confusion1=table(spam.test$yesno,pred)
confusion1
erreurpred=1-sum(diag(confusion1))/ntest
erreurpred
##################
## Avec l'arbre Ã©laguÃ© optimal
pred2=predict(object = pruned.tree,spam.test,type="class")
confusion2=table(spam.test$yesno,pred2)
confusion2
erreurpred=1-sum(diag(confusion2))/ntest
erreurpred

## Bagging
Bagspam.train = bagging(yesno ~ .,coob = TRUE,data = spam.train,nbagg = 500)
##########################
### PrÃ©diction avec le bagging ###
#############################
Bagprev.test = predict(Bagspam.train,newdata = spam.test)
bagconfusion=table(spam.test$yesno,Bagprev.test)
bagconfusion
erreurbag=1-sum(diag(bagconfusion))/ntest
erreurbag
## Random Forests
library(randomForest)
RF.spam = randomForest(yesno ~ ., data=spam.train,ntree=500,importance=TRUE)
summary(RF.spam)
################################
## PrÃ©diction avec Random Forest
################################
RFprev.test = predict(RF.spam,newdata = spam.test)
RFconfusion=table(spam.test$yesno,RFprev.test)
RFconfusion
erreurRF=1-sum(diag(RFconfusion))/ntest
erreurRF

help("varImpPlot")
varImpPlot(RF.spam)
###############################
## Boosting
################################
library(adabag)
boost.spam = boosting(yesno~., data=spam.train,boos=TRUE, mfinal=50)
summary(boost.spam)
################################
## Prédiction avec Boosting
################################
boostprev.test = predict(boost.spam,newdata = spam.test)
boostconfusion=table(spam.test$yesno,boostprev.test$class)
boostconfusion

erreurboost=1-sum(diag(boostconfusion))/ntest
erreurboost

## voir aussi XGBoost
library(xgboost)
## mais surtout tester avec des bases contenant plus
## de variables explicatives 
