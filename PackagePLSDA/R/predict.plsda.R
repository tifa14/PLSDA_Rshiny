#' PLSDA predictions
#'
#' @description
#' Applies PLSDA model to a newdata set
#'
#' @param ObjectPLSDA
#' a PLSDA model (ObjectPLSDA of class \code{plsda})
#' @param newdata
#' a matrix with newdata values (predictors)
#' @param type
#' indicate the type of prediction 'class' or 'posterior'
#' @return
#' PLS-DA results (an object of class \code{plsdares})
#'
#' @details
#' See examples in help for \code{\link{plsda}} function.
#'#' @examples
#' predict.plsda(model, d$test, type ="class")
#' predict.plsda(model, d$test, type ="posterior")




predict.plsda<-function(ObjectPLSDA,newdata,type="class"){
  
  # New data contrôle de cohérence
  if (class(ObjectPLSDA)=="PLSDA") {
    
    if (missing(newdata) || is.null(newdata)){
      newdata<-ObjectPLSDA$X
    }
    
    # X means
    means <- colMeans(ObjectPLSDA$X)
    
    # Center reduce dataset
    X <- (t(newdata) - means)/apply(ObjectPLSDA$X,2,sd)
    
    y.pred <- t(X)%*%ObjectPLSDA$coef + ObjectPLSDA$intercept
    
    #softmax
    y.pred <- t(apply(y.pred,1,function(x){exp(x)/sum(exp(x))}))
    
    
    if (type == "class") {
      # predicted classes
      l.max <- apply(y.pred,1,which.max)
      predict <- colnames(ObjectPLSDA$y_dummies)[l.max]
      return(predict)
    }else if (type == "posterior") {
      return(y.pred)
    } 
    }else{
    stop("The class of the object is not PLSDA")
    }
}

  
  