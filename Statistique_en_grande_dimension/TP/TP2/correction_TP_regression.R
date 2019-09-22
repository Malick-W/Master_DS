# CORRECTION TP REGRESSION PENALISEE

install.packages("glmnet")
library(glmnet)


## EXERCICE 1
p=200
n=100
sigma=0.5
s=10
theta=c(rep(1,s/2),rep(-1,s/2),rep(0,p-s))
x=matrix(rnorm(n*p),n,p)
simuy=function(n,p,theta,sigma,x){
epsilon=(sigma*rnorm(n))
  y=x%*%theta+sigma*epsilon
  return(y)
   }
y=simuy(n,p,theta,sigma,x)

## Q2
LASSO_ex1=glmnet(x,y)

## Q3
LASSO_ex1
## Affiche en fonction de lambda le nombre
## de variables détectées et le pourcentage de variance expliqué

## Q4
## Génération du "chemin de régularisation" (figure directement en .png)
png(file = "fig1_regularisation_LASSO_ex1.png", width = 800, height = 700)
plot(LASSO_ex1)
dev.off()

##Q5
v=coef(LASSO_ex1,s=0.12080)
v[1:30]

##Q6 : fonction predict
Ytest=x%*%theta+rnorm(n)
Ypred=predict(LASSO_ex1,x, s=0.3)
ErreurPred=sqrt(sum((Ypred-Ytest)^2))
ErreurPred/sqrt(n)
ValSetX=matrix(rnorm(n*p),nr=n)
Ytest=ValSetX%*%theta+sigma*rnorm(n)
Ypred=predict(LASSO_ex1, ValSetX, s=0.3)
ErreurPredCar=(sum((Ypred-Ytest)^2))
ErreurPredCar/n

##Q7 : Validation Croisée
cvLASSO_ex1=cv.glmnet(x,y)
png(file = "fig1_valid_croisee_LASSO_ex1.png", width = 800, height = 700)
plot(cvLASSO_ex1)
dev.off()
bestLambda = cvLASSO_ex1$lambda.min

## Q8c : Test de la fonction predict avec cette valeur de lambda
Ypred=predict(LASSO_ex1, ValSetX, s=bestLambda)
MSE=(sum((Ypred-Ytest)^2))
MSE/n
Rdeux=1-(sum(Ytest-Ypred)^2)/sum(Ytest^2)
##Q8cbis : on reprédit avec le modèle classique où l'on a conservé que
## les variables sélectionnées par le LASSO
thetaselect=coef(cvLASSO_ex1,s=bestLambda)
support=which(thetaselect!=0)
selectlm=lm(y~x[,support])
thetachap=coef(selectlm)
Ypredlm=thetachap[1]+ValSetX[,support]%*%thetachap[-1]
MSElm=(sum(Ytest-Ypredlm)^2)
Rdeuxlm=1-(sum(Ytest-Ypredlm)^2)/sum(Ytest^2)
## Q10 : avec le Ridge
Ridge_ex1=glmnet(x,y,alpha=0)
png(file = "fig1_regularisation_Ridge_ex1.png", width = 800, height = 700)
plot(Ridge_ex1)
dev.off()
cvRidge_ex1=cv.glmnet(x,y)
png(file = "fig1_valid_croisee_Ridge_ex1.png", width = 800, height = 700)
plot(cvRidge_ex1)
dev.off()
bestLambda = cvRidge_ex1$lambda.min

## ....

## Exercice 2

## Chargement de la base
leukemia_small= read.csv("leukemia_small.csv")

#  gènes = matrice X et Y réponse
X = as.matrix(t(leukemia_small))
Y = c(rep(1,20),rep(0,14),rep(1,27),rep(0,11))

#  n = 72 patients et p = 3571 genes
colnames(X)=1:ncol(X)
rownames(X) = 1:nrow(X)
n = nrow(X)
p = ncol(X)

# Frequencies de 0/1 dans les observations
mean(Y == 0)
mean(Y == 1)

# Densité empirique des variables explicatives
hist(as.vector(X), breaks=sqrt(n*p), probability = TRUE, main = "Histogram of the genes log-expression", xlab="Genes log-expression")

# Moyenne sur chaque gene
MGenes = apply(X, 2, mean)
## Données recentrées


# Q-Q plot des covariables conditionnelles (seulement sur les premiers genes)
# Conditionnellement Gaussiennes ? Pas clair
qqnorm(X[Y == 0, 2])
qqline(X[Y == 0, 2])
qqnorm(X[Y == 1, 1])
qqline(X[Y == 1, 1])

# PCA sur X 
PCALeuk = prcomp(X, center = TRUE, scale = TRUE)
summary(PCALeuk)
# Axe 1 = 13.3 % de la variance expliquée
# Axe 2 = 7.0 %
# Axe 3 = 5.5 %

# On retient les Axes 1-2-3 et on représente graphiquements les plans correspondants
par(mfrow=c(1,3))

# Axes 1-2
plot(PCALeuk$x[, 1], PCALeuk$x[, 2], pch = 19, xlab = "Pr Comp 1", ylab = "Pr Comp 2", col = 2+Y)
text(PCALeuk$x[, 1], PCALeuk$x[, 2], labels=rownames(PCALeuk$x), cex=0.7, pos = 3, col = 2+Y)

# Axes 1-3
plot(PCALeuk$x[, 1], PCALeuk$x[, 3], pch = 19, xlab = "Pr Comp 1", ylab = "Pr Comp 3", col = 2+Y)
text(PCALeuk$x[, 1], PCALeuk$x[, 3], labels=rownames(PCALeuk$x), cex=0.7, pos = 3, col = 2+Y)

# Axes 2-3
plot(PCALeuk$x[, 2], PCALeuk$x[, 3], pch = 19, xlab = "Pr Comp 2", ylab = "Pr Comp 3", col = 2+Y)
text(PCALeuk$x[, 2], PCALeuk$x[, 3], labels=rownames(PCALeuk$x), cex=0.7, pos = 3, col = 2+Y)

# Clairement, les axes 1-2 laissent présager une bonne classification
# Probleme: requiert les 3571 genes (contenues dans les vecteurs propres)
# Solutions : Tronquer/Chercher une solution parcimonieuse
# Ici : on teste la régression logistique (mais on pourrait aussi faire de l'ACP sparse par exemple)


# approche standard non consistante car p>> n 
classic_glm=glm(Y ~ X[,1:73]-1, family = binomial(link = "logit")) # X-1 means 'X with no intercept'
classic_glm

## A partir de maintenant, Régression pénalisée
## On coupe l'échantillon en deux :2/3 (Apprentissage) 1/3 (Validation)
IndTrain = sample(1:n)[1:(2*n/3)] ## Découpage aléatoire
TrainSetX = X[IndTrain,]
TrainSetY = Y[IndTrain]
IndVal = setdiff(1:n, IndTrain)
ValSetX = X[IndVal,]
ValSetY = Y[IndVal]

# Apprentissage via regression logistique en mode Lasso, Ridge, Elastic-Net
TrainLasso <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=1)
TrainRidge <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=0)
TrainEN1 <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=0.1)
TrainEN2 <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=0.25)
TrainEN3 <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=0.5)
TrainEN4 <- glmnet(TrainSetX, TrainSetY, family="binomial", alpha=0.75)


# Affichage chemins de régularisation
dev.off()
png(file = "chemins_regula_leukemia.png", width = 800, height = 700)
par(mfrow=c(3,2))
plot(TrainLasso)
plot(TrainRidge) 
plot(TrainEN1)
plot(TrainEN2)
plot(TrainEN3)
plot(TrainEN4)
dev.off()

## Extraction d'un coefficient
v=coef(TrainLasso,s=0.043300)
which(v!=0)
v[which(v!=0)]
## Validation croisée pour le choix optimal du Lambda
LambdaLasso = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=1)
LambdaRidge = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=0)
LambdaEN1 = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=0.1)
LambdaEN2 = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=0.25)
LambdaEN3 = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=0.5)
LambdaEN4 = cv.glmnet(TrainSetX, TrainSetY, family="binomial", type.measure="class", alpha=0.75)

# best lambda
bestLambdaLasso = LambdaLasso$lambda.min
bestLambdaRidge = LambdaRidge$lambda.min
bestLambdaEN1 = LambdaEN1$lambda.min
bestLambdaEN2 = LambdaEN2$lambda.min
bestLambdaEN3 = LambdaEN3$lambda.min
bestLambdaEN4 = LambdaEN4$lambda.min

# Naturellement, lambda(Ridge) >> lambda(Lasso) et lambda(Elastic-Net)
# Evolution de l'erreur de classification avec log(lambda)
png(file = "validation_croisee_leukemia.png", width = 800, height = 700)
par(mfrow=c(3,2))
plot(LambdaLasso)
plot(LambdaRidge)
plot(LambdaEN1)
plot(LambdaEN2)
plot(LambdaEN3)
plot(LambdaEN4)
dev.off()

# Prédiction sur la partie de l'échantillon laissée de côté
ValSetYPredLasso = as.numeric(predict(LambdaLasso$glmnet.fit, ValSetX, s=bestLambdaLasso, type="class"))
ValSetYPredRidge = as.numeric(predict(LambdaRidge$glmnet.fit, ValSetX, s=bestLambdaRidge, type="class"))
ValSetYPredEN1 = as.numeric(predict(LambdaEN1$glmnet.fit, ValSetX, s=bestLambdaEN1, type="class"))
ValSetYPredEN2 = as.numeric(predict(LambdaEN2$glmnet.fit, ValSetX, s=bestLambdaEN2, type="class"))
ValSetYPredEN3 = as.numeric(predict(LambdaEN3$glmnet.fit, ValSetX, s=bestLambdaEN3, type="class"))
ValSetYPredEN4 = as.numeric(predict(LambdaEN4$glmnet.fit, ValSetX, s=bestLambdaEN4, type="class"))

# Erreur de classification
sum(abs(ValSetY - ValSetYPredLasso))/24
sum(abs(ValSetY - ValSetYPredRidge))/24
sum(abs(ValSetY - ValSetYPredEN1))/24
sum(abs(ValSetY - ValSetYPredEN2))/24
sum(abs(ValSetY - ValSetYPredEN4))/24



# Genes sélectionnés
SelLasso = predict(LambdaLasso$glmnet.fit, TrainSetX, s=bestLambdaLasso, type="nonzero")
SelRidge = predict(LambdaRidge$glmnet.fit, TrainSetX, s=bestLambdaRidge, type="nonzero")
SelEN1 = predict(LambdaEN1$glmnet.fit, TrainSetX, s=bestLambdaEN1, type="nonzero")
SelEN2 = predict(LambdaEN2$glmnet.fit, TrainSetX, s=bestLambdaEN2, type="nonzero")
SelEN3 = predict(LambdaEN3$glmnet.fit, TrainSetX, s=bestLambdaEN3, type="nonzero")
SelEN4 = predict(LambdaEN4$glmnet.fit, TrainSetX, s=bestLambdaEN4, type="nonzero")

# Naturellement, on trouve tous les gènes dans le Ridge 


## Pour tirer des conclusions, il faut répéter l'expérience

## Pour faire mieux, chercher d'autres techniques
## Sparse PCA/Groupe Lasso/Sparse Clustering....
