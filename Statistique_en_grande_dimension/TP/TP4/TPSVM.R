## TP Support Vector Machines
library(e1071)
n=500
b=runif(n,min=0,max=2)
bprim=runif(n,min=-2,max=0)
x=runif(n,min=-3,max=3)
xprim=runif(n,min=-3,max=3)
x1=c(x,xprim)
x2=c(b+x,bprim+xprim)
y=c(rep(1,n),rep(-1,n))
Z=data.frame(X1=x1,X2=x2,classe=y)
Z$classe=as.factor(Z$classe)
svmlinear=svm(classe~.,data=Z,kernel="linear",cost=20)
png(file = "svmlinear.png", width = 800, height = 700)
plot(svmlinear,data=Z)
dev.off()
## vecteurs supports/indices/intercept
xi=svmlinear$SV
alphaygreci=svmlinear$coefs
beta0=svmlinear$rho
svmlinear$index
## Eq. de l'hyperplan séparateur en fonction de x 
## sum(alphaygreci*t(xi)%*%x+beta0=0)

## Prendre cost proche de l'infini pour retrouve le SVM non flexible.
## Remarque : le SVM non flexible est basé sur un faible nombre de vecteurs supports
## prédiction sur l'échantillon d'entraînement
pred=predict(svmlinear,Z)
tab=table(Predits=pred,Obs=Z$classe)
tab
## Erreur d'entraînement : 1-sum(diag(tab))/(taille de l'échantillon d'apprentissage)

## Erreur test : on prend un échantillon test Ztilde et on predit :
#pred=predict(svmlinear,Ztilde)
#tabtilde=table(Predits=pred,Obs=Ztilde$classe)
#tabtilde
#Erreur test : 1-sum(diag(tabtilde))/(taille de l'échantillon test)

## Si l'on enlève l'option sur le noyau, on utilise les SVM à noyau gaussien.
svmgauss=svm(classe~.,data=Z)

## Exercice 1 : Exemple 2
p=3
n=100
X=matrix(rnorm(n*p),nr=n)
y=2*(apply(X^2,1,sum)<sqrt(p))-1
Z=data.frame(cbind(X,y))
colnames(Z)[p+1]="classe"
colnames(Z)[1:p]=paste("X", 1:p, sep="")
Z$classe=as.factor(Z$classe)
svmex2=svm(classe~.,data=Z,kernel="polynomial")
svmex2bis=svm(classe~.,data=Z,kernel="polynomial",degree=5)
#png(file = "svmex2.png", width = 800, height = 700)
plot(svmex2,data=Z,X2~X1,slice=list(X3=mean(Z$X3)))
## Ne fonctionne pas !! Utiliser un noyau de degré pair pour ce problème
#dev.off()
#png(file = "svmex2bis.png", width = 800, height = 700)
plot(svmex2bis,data=Z,X2~X1,slice=list(X3=mean(Z$X3)))
#dev.off()
## Noyau gaussien
svmex2tri=svm(classe~.,data=Z)

## calibrage des paramètres
tune.svm(classe~., data = Z,gamma = 2^(-7:0), cost = 2^(-2:3))
## Classifieur optimal obtenu pour gamma=0.5 et cost=1)
svmopt=svm(classe~.,data=Z,gamma=0.5,cost=1)

## Erreur test pour le choix optimal
p=3
ntest=100
Xtest=matrix(rnorm(ntest*p),nr=ntest)
ytest=2*(apply(Xtest^2,1,sum)<sqrt(p))-1
Ztest=data.frame(cbind(Xtest,ytest))
colnames(Ztest)[p+1]="classe"
colnames(Ztest)[1:p]=paste("X", 1:p, sep="")
Ztest$classe=as.factor(Ztest$classe)

predtest=predict(svmopt,Ztest)
tabtest=table(Predits=predtest,Obs=Ztest$classe)
tabtest
erreurclassiftest=1-sum(diag(tabtest))/ntest
erreurclassiftest

## polynomial
## calibrage des paramètres
tune.svm(classe~., data = Z,gamma = 2^(-7:0), kernel="polynomial", degree=c(2,4,6), cost = 2^(-2:3))
## Classifieur optimal obtenu pour degree=2, gamma=0.5 et cost=1)
svmoptpoly=svm(classe~.,data=Z,kernel="polynomial",gamma=0.5,degree=2,cost=1)
predtestpoly=predict(svmoptpoly,Ztest)
tabtest=table(Predits=predtest,Obs=Ztest$classe)
tabtest
erreurclassiftestpoly=1-sum(diag(tabtest))/ntest
erreurclassiftestpoly

## Utilisation de slice (tranche) : on choisit deux variables
## et on prend une tranche en fixant les valeurs des variables restantes
## Le plus naturel est de prendre la moyenne.

## Si on
liste=liste(X3=mean(Z$X3))
for (i in 4:p){
  liste=cbind(liste,X4=mean$X4)
}
## plot(svmex2Q10,data=Z,X2~X1,slice=liste))

## Exercice 2 : IRIS

data(iris)
str(iris)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,color= Species)

# IRIS et SVM

svmiris=svm(Species~.,data=iris,kernel="linear")
## Le choix par défaut est "radial", i.e. noyau gaussien
summary(svmiris)
plot(svmiris,data=iris,Petal.Width~Petal.Length,,slice=list(Sepal.Width=3,Sepal.Length=5))
print(svmiris$coefs)
## Matrice de confusion
pred=predict(svmiris,iris)
tab=table(Predits=pred,Obs=iris$Species)
tab

#Support Vector Machine
library(e1071)
svmiris=svm(Species~.,data=iris,kernel="linear")
## Le choix par défaut est "radial", i.e. noyau gaussien
summary(svmiris)
plot(svmiris,data=iris,Petal.Width~Petal.Length,,slice=list(Sepal.Width=3,Sepal.Length=5))
print(svmiris.coef_)
## Matrice de confusion
pred=predict(svmiris,iris)
tab=table(Predits=pred,Obs=iris$Species)
tab

## On peut à nouveau envisager de calibrer les paramètres de manière à minimiser l'erreur.
## On sélectionne au hasard 100 individus.
IndTrain=sample(1:150,size=100,replace=FALSE)
iristrain=iris[IndTrain,]
iristest=iris[-IndTrain,]
tune.svm(Species~.,data=iristrain,gamma=seq(0.1,2,by=0.2),cost=seq(0.1,10,by=0.4))
## choix optimal, gam=0.1,cost=4.1 (sur mon exemple)
svmoptiris=svm(Species~.,data=iristrain,gamma=0.1,cost=4.1)
predtest=predict(svmoptiris,iristest)
tab=table(Predits=predtest,Obs=iristest$Species)
tab

## Exercice 3 (proche projet)
