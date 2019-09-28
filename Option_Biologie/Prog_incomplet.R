
# commencer par creer un jeu de donnees virtuel de petite taille
n = 4
ncol = 4*4+3
MatIndiv = matrix(0, nrow=n, ncol=ncol)

MatIndiv[1, ] = c(10,10,0,0,50,60,0,0,100,100,0,0,150,160,0,0,2,1,1)
MatIndiv[2, ] = c(20,30,0,0,50,80,0,0,110,110,0,0,180,180,0,0,2,1,2)
MatIndiv[3, ] = c(10,20,0,0,50,50,0,0,100,110,0,0,160,180,0,0,2,2,3)
#MatIndiv[4, ] = c(10,20,0,0,50,50,0,0,100,110,0,0,160,180,0,0,2,2,4)


# compte le nombre d'enfants virtuels de meme genotype que l'enfant de reference
# in : enfants virtuels (matrice avec un genotype sur chaque ligne), genotype de l'enfant (vecteur)
# out : le nombre d'enfants virtuels correspondant
comparerEnf = function(EnfVirt, Enf){
  
  # attention : on pensera a trier les vecteurs avant de les comparer
  EnfTri = sort(Enf)
  nb = 0
  
  for (i in 1:nrow(EnfVirt)) {
    EnfVirt_Tri = sort(EnfVirt[i,])
    
    if (sum(abs(EnfTri - EnfVirt_Tri)) == 0) {
      nb = nb + 1 
    } 
  }
  return(nb)
}


# genere la liste des enfants virtuels a partir du genotype des parents pour le schema 2x-2x
# in : genotype des parents (vecteurs de taille 2)
# out : liste des enfants virtuels (matrice de 4 lignes et 2 colonnes)
genererEnfVirt22 = function(GenP1s, GenP2s){

  # creer les 4 enfants virtuels pour le schema 2x-2x
  # x <- c(10,20)
  # y <- c(30,40)
  # d1 <- expand.grid(x = x, y = y)
  
  EnfVirt = expand.grid(x = GenP1s, y = GenP2s)
  return(as.matrix(EnfVirt))
}


# calcul de la proba d'un lien entre deux parents et un enfant
# in : infos genetiques sur les deux parents et l'enfant (vecteurs de taille 17 : genotypes 4x4 + ploidie)
# out : proba du lien
calculerProbaLien = function(GenP1, GenP2, GenE){
  
  # traiter seulement 2x-2x dans un premier temps (recuperer les ploidies et rentrer seulement si toutes sont egales a 2)
  plot1 = GenP1[17]
  plot2 = GenP2[17]
  plotE = GenPE[17]
  
  if (plo1==2 & plo2==2 & ploE==2) {
    p = 1
    # boucler sur les 4 signaux en multipliant p par la nouvelle proba
    # a chaque boucle :
    # creer des vecteurs de taille 2 avec les alleles des parents et de l'enfant sur le signal
    # generer les enfants virtuels du croisement, comparer avec l'enfant et deduire la proba
    
    for (s in 1:4) {
      GenP1S = GenP1[(4*(s-1)+1):(4*s)]
      GenP2S = GenP2[(4*(s-1)+1):(4*s)]
      GenPES = GenPE[(4*(s-1)+1):(4*s)]
      G1 = GenP1S[1:2]
      G2 = GenP2S[1:2]
      GE = GenPES[1:2]
      
      EnfVirt = genererEnfVirt22(G1,G2)
      nb = comparerEnf(EnfVirt, GE)
      p = p*nb/4
    }
  }

  return(p)
}


# construire pour un enfant la matrice des probas associee a tous les couples de parents potentiels
# in : numero de l'enfant
# out : matrice des probas (n lignes et n colonnes)
calculerProbasParents = function(e){
  MatProbasParents = matrix(0, nrow=n, ncol=n)
  
  # recuperer en amont la generation de l'enfant
  # double boucle sur les parents
    # ne rentrer que si le couple est envisageable (parents distincts de l'enfant, anterieurs, ...)
    # calculer la proba du lien a l'aide des infos genetiques des parents et de l'enfant
  # penser a renormaliser
  
  return(MatProbasParents)
}


# pour un enfant, recherche du couple de parents le plus probable
# in : numero de l'enfant
# out : couple de parents le plus probable (vecteur de taille 4 : enfant/parent 1/parent 2/log-proba du lien)
recupParentsMax = function(e){
  
  # calculer la matrice des probas pour tous les couples associes a l'enfant e
  
  pmax = max(MatProbasParents)
  IndMax = which(MatProbasParents == pmax, arr.ind = TRUE)
  
  # par convention : mettre 0-0 pour les parents si toutes les probas sont nulles, et 0 dans la case log-proba
  
  return(ParentsMax)
}


# construire la genealogie de plus grande proba associee a la matrice des individus
# in : rien
# out : genealogie la plus probable (matrice de n lignes et 4 colonnes) munie de la log-vraisemblance
construireGen = function(){
  Gen = matrix(0, nrow=n, ncol=4)
  
  # boucler sur les individus
    # chercher le couple le plus probable pour l'individu courant
    # empiler le resultat
  
  return(list(Gen = Gen, lLik = sum(Gen[, 4])))
}


# representation graphique de la genealogie a l'aide de igraph (noeuds = individus, fleches = liens)
# in : genealogie (matrice de n lignes et 4 colonnes)
# out : rien
representerGen = function(GenMax){
  nrel = nrow(GenMax$Gen)
  
  # jouer sur les couleurs, les formes, ...
  Grp = graph.empty(directed=TRUE) + vertices(GenMax$Gen[, 1], color=..., shape=..., size=10, label.cex=0.7, label.color=...)
  
  # ajouter les fleches munies d'une approximation de la proba du lien a 3 ou 4 chiffres
  
  plot(Grp)
}


library("igraph")
GenMax = construireGen()
representerGen(GenMax)





