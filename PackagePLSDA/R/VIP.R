#'Variable Important for Projection in PLS-DA
#'
#' This function calculates the VIP that determines which variables are important for projection.
#' @usage
#' plsda.vip(objectPLS,threshold=0.8)
#' @param
#' ObjectPLSDA is an object from \code{plsda.fit} function.
#' @param
#' threshold is the threshold below which the variable is rejected. By default the threshold is fixed to 0.8.
#' @details
#' The VIP is a criterion based on the coefficient of determination between Y and the components of X calculated
#' from NIPALS algorithm. Usually, if the VIP < 0.8 for a given variable, that variable is not important for the prediction
#' of Y.
#' @return
#' An object of class VIP is a list conatining at least the following components :
#' \cr
#' \code{newX} the data frame containing the Variable Important for Predictions.
#' \cr
#' \code{name} a vector of names of the VIP.
#' \cr
#' \code{vip} a vector containing the VIP criterion calculated for each variables.
#' \cr
#' \code{r2} the coefficient of determination from the PLS Object.

#' @examples
#'vip_var<-plsda.vip(model.pls0 , 0.7)



plsda.vip<-function(objectPLSDA,threshold=0.8){
  
  #from the object PLSDA
  r2 <- as.matrix(objectPLSDA$coef)
  x <- as.matrix(objectPLSDA$X)
  W<-as.matrix(objectPLSDA$x.loadings)
  
  #colSum of correlations
  Sqr2 <- colSums(r2)
  #Sum of all correlations
  Sr2 <- sum(Sqr2)
  
  #VIP
  Sr2b<- sapply(1:ncol(x),function(x){sum(Sqr2*((W^2)[x,]))})
  vip<- sqrt((ncol(x)/Sr2)*Sr2b)
  
  #Colnames
  names(vip)<-colnames(x)
  
  #VIP if vip>treshold
  varnames<-names(vip)[vip>threshold]
  #if only 1 vip with treshold, select minimum 2
  if(length(varnames)<2){
    varnames <- sort(vip, decreasing = T)
    varnames <- names(vip)[1:2]
  }
  
  #df with only vip
  df_var <- as.data.frame(objectPLSDA$X)
  df_var <- df_var[,colnames(x) %in% varnames]
  
  
  object <- list("newX"=df_var,
                 "name"= varnames,
                 "vip"=vip,
                 "r2"=r2
  )
  
  class(object) <- "VIP"
  return(object)
}
