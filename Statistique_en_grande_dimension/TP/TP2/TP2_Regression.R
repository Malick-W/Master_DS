### Statistique en Grande Dimension et Apprentissage - TP Chapitre 3 ###

set.seed(314)

# on importe le package 'glmnet' de Friedman, Hastie, Simon et Tibshirani
library(glmnet)
library(ggplot2) # pour des grpahes plus jolies

### Exercice 1 

## question 1

Simul = function(n,p,theta,sigma,x){
  epsilon = (sigma*rnorm(n)) # rnorm(n): génération de n nombre aléatoires sivant la loi normale centrée réduite
  y = x%*%theta + epsilon
  return(y)
}


## question 2

p=200
n=100
sigma=0.5
s=10
theta=c(rep(1,s/2),rep(-1,s/2),rep(0,p-s))
x=matrix(rnorm(n*p),n,p)
y=Simul(n,p,theta,sigma,x)

# On fait du Lasso avec alpha = 1
# Une grille de lambda est automatiquement determinée, en partant du plus petit lambda, annulant tous les
# coefficients( sauf la constante), jusqu'à une centiéme valeur de lambda (avec l'option par defaut nlambda=100)
# lambdas = 10^seq(3, -2, by = -.1)

LASSO_ex1=glmnet(x,y, alpha = 1)


## question 3

LASSO_ex1
# Affiche en fonction de lambda le nombre de variables selectionné et le pourcentage de variance expliqué
# On peut donner manuellement une grille de valeur pour lambda

## question 4
# Génération du "chemin de régularisation"
plot(LASSO_ex1) #xvar = c("norm", "lambda", "dev")

which(coef(LASSO_ex1, s=1) !=0) # les coef de teta non nulls


## question 5
v=coef(LASSO_ex1, s=0.3) 
v[1:100]


## question 6
Ypred = predict(LASSO_ex1,x, s=0.3) # refaire prédiction avec s="lambda.min" de la question 7

ErreurPred = sum((Ypred-y)^2)
ErreurPred/n


## question 7
cvLASSO_ex1=cv.glmnet(x,y, alpha=1, type.measure="mse") # , type.measure="mse", "mae", "auc
plot(cvLASSO_ex1)

#L'erreur MSE  minimum de la cross-validated est:
min(cvLASSO_ex1$cvm)
# avec un lambda:
meilleur_Lambda = cvLASSO_ex1$lambda.min
meilleur_Lambda

# lambda.1se : la plus grande valeur de lambda de sorte que l'erreur se situe à moins d'une erreur-type du minimum.
cvLASSO_ex1$lambda.1se
cvLASSO_ex1$cvm[which(cvLASSO_ex1$lambda == cvLASSO_ex1$lambda.1se)] 


## question 8
# a)
ValSetX = matrix(rnorm(n*p),n,p)
Ytest = Simul(n,p,theta,sigma,ValSetX)

# b)
Ypred=predict(LASSO_ex1, ValSetX, s=meilleur_Lambda)

# c) Erreur quadratique moyenne 
MSE=(sum((Ypred-Ytest)^2))/n
MSE
R2=1-(sum(Ytest-Ypred)^2)/sum((Ytest-mean(Ytest))^2)
R2

# d) prédiction avec le modèle lm classique avec les variables sélectionnées par le LASSO
teta = coef(cvLASSO_ex1,s=meilleur_Lambda)
Var = which(teta!=0)
LM_ex1 = lm (y~x[,Var])
Ypred_LM = predict(LM_ex1, data = ValSetX) # Ypred_LM = teta[1]+ValSetX[,Var]%*%teta[-1] 

MSE_LM = (sum(Ytest-Ypred_LM)^2)/n
MSE_LM
R2_LM = 1-(sum(Ytest-Ypred_LM)^2)/sum((Ytest-mean(Ytest))^2)
R2_LM

sprintf("R2_Lasso: %s", round(R2,3))
sprintf("R2_LM: %s", round(R2_LM,3))


## question 8  Ridge
Ridge_ex1=glmnet(x,y,alpha=0)
plot(Ridge_ex1, xvar="lambda", label=TRUE) #xvar = c("norm", "lambda", "dev")

cvRidge_ex1=cv.glmnet(x,y)
plot(cvRidge_ex1)

meilleur_Lambda = cvRidge_ex1$lambda.min
meilleur_Lambda



###################################################################################################
###################################################################################################



### Exercice 2 (Classification et Leucémie)

## question 1
path_home = "/home/malick/Bureau/Data/Statistique_en_grande_dimension/leukemia_small.csv"
leukemia_small= read.csv(path_home, header = F)


## question 2&3
#  n = 72 patients et p = 3571 genes
df = as.matrix(t(leukemia_small)) 
dim(df)
Y = rep(0,72)
Y[which(df[,1]=="ALL")] = 1

X = df[,-c(1)]
class(X) = "numeric"  # on change la class de X qui était de type string du fait de header = F
dim(X)


## question 4
## a)
n = nrow(X)
p = ncol(X)

# Densité empirique des variables explicatives
hist(as.vector(X), breaks=sqrt(n*p), probability = TRUE, main = "Histogramme des génes log-expression", xlab="Génes log-expression")
# On remarque que les variables sont distribuées selon une loi normale centrée

## b)
hist(as.vector(X[,15]),breaks = seq(min(X),max(X),l=100), probability = TRUE, main = "Histogramme", xlab="Géne")# Frequencies de 0/1 dans les observations

## c)
MoyenneGenes = apply(X, 2, mean) # moyenne sur les colonnes "paramétre 2 
max(abs(MoyenneGenes)) # les données sont bien centrées

mean(Y == 0)
mean(Y == 1)

## c)
j = 54
par(mfrow=c(1,2))
hist(X[, j], col = grey(0.8), main = "Histogramme", xlab="Géne 10")
qqnorm(X[, j])
qqline(X[, j], col = "red")
# On peut effectuer le test de Shapiro-Wilk
shapiro.test(X[, j])

par(mfrow=c(2,2))
hist(X[Y == 0, 10], col = grey(0.8), main = "Histogramme", xlab="Géne 10")
qqnorm(X[Y == 0, 10])
qqline(X[Y == 0, 10], col = "red")

hist(X[Y == 1, 15], col = grey(0.8), main = "Histogramme", xlab="Géne 10")
qqnorm(X[Y == 1, 15], main = "Histogramme", xlab="Géne 15")
qqline(X[Y == 1, 15], col = "red")

# On peut effectuer le test de Shapiro-Wilk
shapiro.test(X[Y == 1, 1005])


### question 5 ACP (. . . pour voir !) :
#library(factoextra) # pour une ACP compléte et détailleé
#http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/79-acp-dans-r-prcomp-vs-princomp/

## a) 
PCALeuk = prcomp(X, center = TRUE, scale = TRUE)
summary(PCALeuk)
# Axe 1 = 13.3 % de la variance expliquée
# Axe 2 = 7.0 % de la variance expliquée
# Axe 3 = 5.5 % de la variance expliquée

# On retient les Axes 1-2-3 et on représente graphiquements les plans correspondants
## b)
par(mfrow=c(1,3))
# Axes 1-2
plot(PCALeuk$x[, 1], PCALeuk$x[, 2], pch = 19, xlab = "Dim 1", ylab = "Dim 2", col = 2+Y)
text(PCALeuk$x[, 1], PCALeuk$x[, 2], labels=df[,1], cex=0.7, pos = 3, col = 2+Y)
# Axes 1-3
plot(PCALeuk$x[, 1], PCALeuk$x[, 3], pch = 19, xlab = "Dim 1", ylab = "Dim 3", col = 2+Y)
text(PCALeuk$x[, 1], PCALeuk$x[, 3], labels=df[,1], cex=0.7, pos = 3, col = 2+Y)
# Axes 2-3
plot(PCALeuk$x[, 2], PCALeuk$x[, 3], pch = 19, xlab = "Dim 2", ylab = "Dim 3", col = 2+Y)
text(PCALeuk$x[, 2], PCALeuk$x[, 3], labels=df[,1], cex=0.7, pos = 3, col = 2+Y)

## c) les axes 1-2 présente une bonne séparation donc une bonne classification


### Question 6: la Régression Logistique pénalisée.

## a) 
classic_glm=glm(Y ~ X[,1:73]-1, family = binomial(link = "logit")) 
# X-1 means 'X with no intercept':::::: les données sont centrées => pas d'intercepte
summary(classic_glm)

## b)
# On découpe aléatoirement l'echantillon: 2/3 pour l'Apprentissage et 1/3 pour la Validation
n = nrow(X)
IndTrain = sample(1:n)[1:(2*n/3)] 
TrainX = X[IndTrain,]
TrainY = Y[IndTrain]
IndTest = setdiff(1:n, IndTrain)
TestX = X[IndTest,]
TestY = Y[IndTest]


## c)&d) Lasso, Ridge, Elastic-Net
TrainLasso = glmnet(TrainX, TrainY, alpha=1, family="binomial") # type="auc"
TrainRidge = glmnet(TrainX, TrainY, alpha=0, family="binomial")
TrainEN1 = glmnet(TrainX, TrainY, alpha=0.25, family="binomial")
TrainEN2 = glmnet(TrainX, TrainY, alpha=0.5, family="binomial")
TrainEN3 = glmnet(TrainX, TrainY, alpha=0.75, family="binomial")


## e) 
summary(TrainLasso)


## f) chemins de régularisation
# Error in plot.new() : figure margins too large ==> on sauvegarde les plot
dev.off()
png(file = "chemins_Exercice2_f).png", width = 800, height = 700)
par(mfrow=c(3,2))
plot(TrainLasso)
plot(TrainRidge) 
plot(TrainEN1)
plot(TrainEN2)
plot(TrainEN3)
dev.off()


## g)  Validation croisée pour le choix optimal du Lambda
LambdaLasso = cv.glmnet(TrainX, TrainY, family="binomial", type.measure="class", alpha=1)
LambdaRidge = cv.glmnet(TrainX, TrainY, family="binomial", type.measure="class", alpha=0)
LambdaEN1 = cv.glmnet(TrainX, TrainY, family="binomial", type.measure="class", alpha=0.25)
LambdaEN2 = cv.glmnet(TrainX, TrainY, family="binomial", type.measure="class", alpha=0.5)
LambdaEN3 = cv.glmnet(TrainX, TrainY, family="binomial", type.measure="class", alpha=0.75)

# Evolution de l'erreur de classification
png(file = "CV_Exercice2_g).png", width = 800, height = 700)
par(mfrow=c(3,2))
plot(LambdaLasso)
plot(LambdaRidge)
plot(LambdaEN1)
plot(LambdaEN2)
plot(LambdaEN3)
dev.off()


## h)
# meilleur lambda pour chaque modéle
meilleurLambdaLasso = LambdaLasso$lambda.min
meilleurLambdaRidge = LambdaRidge$lambda.min
meilleurLambdaEN1 = LambdaEN1$lambda.min
meilleurLambdaEN2 = LambdaEN2$lambda.min
meilleurLambdaEN3 = LambdaEN3$lambda.min

#teta pour la meilleure valeure de lambda
v = coef(TrainLasso, s=meilleurLambdaLasso) 
v[which(v!=0)]


## i)&j)
# Prédiction sur l'échantillon test avec le meilleur lambda
PredLasso = as.numeric(predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaLasso, type="class"))
PredRidge = as.numeric(predict(LambdaRidge$glmnet.fit, TestX, s=meilleurLambdaRidge, type="class"))
PredEN1 = as.numeric(predict(LambdaEN1$glmnet.fit, TestX, s=meilleurLambdaEN1, type="class"))
PredEN2 = as.numeric(predict(LambdaEN2$glmnet.fit, TestX, s=meilleurLambdaEN2, type="class"))
PredEN3 = as.numeric(predict(LambdaEN3$glmnet.fit, TestX, s=meilleurLambdaEN3, type="class"))


# Erreur de classification
nTest = length(IndTest)
E1 = sum(abs(TestY - PredLasso))/nTest
E2 = sum(abs(TestY - PredRidge))/nTest
E3 = sum(abs(TestY - PredEN1))/nTest
E4 = sum(abs(TestY - PredEN2))/nTest
E5 = sum(abs(TestY - PredEN3))/nTest

df_Erreur = data.frame(erreur=round(c(E1,E2,E3,E4,E5),3),
                    row.names=c("Lasso","Ridge", "ElasticNet_0.25","ElasticNet_0.5","ElasticNet_0.75"))
df_Erreur

#Genes sélectionnés dans chaque modéle
GenesLasso = predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaLasso, type="nonzero")
GenesRidge = predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaRidge, type="nonzero")
GenesEN1 = predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaEN1, type="nonzero")
GenesEN2 = predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaEN2, type="nonzero")
GenesEN3 = predict(LambdaLasso$glmnet.fit, TestX, s=meilleurLambdaEN3, type="nonzero")


## k) 