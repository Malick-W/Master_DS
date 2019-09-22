mnist_train=read.csv("mnist_train.csv",header=TRUE)
View(mnist_train)
is(mnist_train)
dim(mnist_train)
## 785=28*28+1
## La 1ere colonne est la réponse (chiffre entre 0 et 9)
Ytrain=as.factor(mnist_train[,1])
## as.factor car problème de classification
Xtrain=mnist_train[,-1]
## Chargement des données test
mnist_test=read.csv("mnist_train.csv",header=TRUE)
View(mnist_test)
is(mnist_test)
dim(mnist_test)
## 10000 données test
## 785=28*28+1
## La 1ere colonne est la réponse (chiffre entre 0 et 9)
Ytest=as.factor(mnist_test[,1])
## as.factor car problème de classification
Xtest=mnist_test[,-1]
## Testons les k-ppv sur ce pb.
## Données déjà nettoyées
## Combien d'individus par classe ?
:
  eff_classe=rep(0,10)
  for (i in(0:9)){eff_classe[i+1]=sum(Ytrain==i)}
eff_classe
## Remarque : plutôt bien répartis
## Visualisation de l'image i
i=20
A=Xtrain[i,]
Ytrain[i] # Réponse est 0
B=which(A>0)
C=B%/%28 # pour récupérer la ligne
D=B%%28 # pour récupérer la colonne
plot(C,D,col=3,pch=3)



## Complexité des k-ppv de l'ordre de nkd...
## On choisit de réduire la taille de l'échantillon 
## pour que le calcul se fasse 
SousEch=sample(1:nrow(Xtrain),12000)
EchXtrain=Xtrain[SousEch,]
EchYtrain=Ytrain[SousEch]

## Calcul de la réponse sur l'échantillon test
library(class)
t1=Sys.time() 
prev.knn=knn(EchXtrain,Xtest,EchYtrain,k=10)
t2=Sys.time()
difftime(t2,t1)

## Matrice de confusion
conf=table(prev.knn,Ytest)
conf
## Calcul de l'erreur de classification
(sum(conf)-sum(diag(conf)))/sum(conf)
## On trouve 5,3% d'erreurs (avec une base d'apprentissage de 12000)


  
  


