
# commencer par creer un jeu de donnees virtuel de petite taille
n = 4
ncol = 4*4+3
MatIndiv = matrix(0, nrow=n, ncol=ncol)

MatIndiv[1, ] = c(10,10,0,0,40,50,0,0,80,80,0,0,110,120,0,0,2,1,1)
MatIndiv[2, ] = c(10,20,0,0,60,60,0,0,80,80,0,0,120,130,0,0,2,1,2)
MatIndiv[3, ] = c(20,20,0,0,50,60,0,0,80,80,0,0,110,120,0,0,2,1,3)
MatIndiv[4, ] = c(10,20,0,0,50,60,0,0,80,80,0,0,110,120,0,0,2,2,4)


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
  plo1 = GenP1[17]
  plo2 = GenP2[17]
  ploE = GenE[17]
  
  if (plo1==2 & plo2==2 & ploE==2) {
    p = 1
    # boucler sur les 4 signaux en multipliant p par la nouvelle proba
    # a chaque boucle :
    # creer des vecteurs de taille 2 avec les alleles des parents et de l'enfant sur le signal
    # generer les enfants virtuels du croisement, comparer avec l'enfant et deduire la proba
    
    for (s in 1:4) {
      GenP1S = GenP1[(4*(s-1)+1):(4*s)]
      GenP2S = GenP2[(4*(s-1)+1):(4*s)]
      GenES = GenE[(4*(s-1)+1):(4*s)]
      G1 = GenP1S[1:2]
      G2 = GenP2S[1:2]
      GE = GenES[1:2]
      
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
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i<j & i!=e[19] &  j!=e[19] & e[18]>MatIndiv[i, 18] & e[18]>MatIndiv[j, 18]) {
        MatProbasParents[i,j] = calculerProbaLien(MatIndiv[i, ],MatIndiv[j, ],e)
      } 
    }
  }
  s = sum(MatProbasParents)
  if (s != 0){
    MatProbasParents = MatProbasParents/s
  }
  return(MatProbasParents)
}


# pour un enfant, recherche du couple de parents le plus probable
# in : numero de l'enfant
# out : couple de parents le plus probable (vecteur de taille 4 : enfant/parent 1/parent 2/log-proba du lien)
recupParentsMax = function(e){
  
  ParentsMax = c(e[19],0,0,0)
  
  # calculer la matrice des probas pour tous les couples associes a l'enfant e
  MatProbasParents = calculerProbasParents(e)
  pmax = max(MatProbasParents)
  
  if (pmax != 0){
    IndMax = which(MatProbasParents == pmax, arr.ind = TRUE)
    
    ParentsMax[4] = log(pmax)
    ParentsMax[2] = round(IndMax[1])
    ParentsMax[3] = round(IndMax[2])
  }
    
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
  
  for (i in 1:n) {
    Gen[i,] = recupParentsMax(MatIndiv[i, ])
      } 
  return(list(Gen = Gen, lLik = sum(Gen[, 4])))
}


# representation graphique de la genealogie a l'aide de igraph (noeuds = individus, fleches = liens)
# in : genealogie (matrice de n lignes et 4 colonnes)
# out : rien

# https://f.hypotheses.org/wp-content/blogs.dir/2996/files/2017/02/visualiseR.pdf

representerGen = function(GenMax){
  nrel = nrow(GenMax$Gen)
  
  # jouer sur les couleurs, les formes, ...
  Grp = graph.empty(directed=TRUE) + vertices(GenMax$Gen[, 1], color="red", shape="circle", size=10, label.cex=0.7, label.color="blue")
  
  # ajouter les fleches munies d'une approximation de la proba du lien a 3 ou 4 chiffres
  
  plot(Grp, vertex.size = 20, vertex.color="orange",
       edge.arrow.size=.4, edge.curved=.4)
}


library("igraph")
GenMax = construireGen()
representerGen(GenMax)





