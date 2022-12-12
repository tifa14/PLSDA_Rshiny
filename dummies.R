

###########################################################################
####         Fonction qui transforme un vecteur X de p facteurs        ####    
####                en une matrice de p indicatrices                   ####
###########################################################################

# X = Variable à transformer
dummies<-function(X){
  
  indic <- levels(X)
  # Matrice d'indicatrice
  dum<-t(sapply(X,function(x){ifelse(indic==x,1,0)}))
  # Renommage des colonnes
  colnames(dum)<-indic
  
  return(as.matrix(dum))
}

#test
#Y<-iris[,5]
#Y_dum = dummies(Y)
#Y_dum
