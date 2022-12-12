### nipalspls.fit.R: Nipals PLS fit algorithm for tall data.
###
### Implements an adapted version of the `’algorithme PLS1 NIPALS' described in
###   Diplôme PostGrade en Statistique - La régression PLS (2004) from 
###   Séverine Vancolen supervised by Yadolah Dodge
### 
###   Ressources included in Methods and formulas for x- 
###   and y-statistics in Partial Least Squares Regression from support.minitab.com
###   https://support.minitab.com/en-us/minitab/20/help-and-how-to/statistical-modeling/regression/how-to/partial-least-squares/methods-and-formulas/component-information/



#' @title Nipals PLS (M2 SISE R Project)
#'
#' @description Fits a dataset with the Nipals PLS algorithm.
#'
#'
#' @param formula a formula for the PLS regression.  \code{NA}s and \code{Inf}s are
#' not allowed.
#' @param data a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' allowed.
#' @param ncomp the number of components to be used in the modelling.
#' @param center logical, If \code{TRUE}, center descriptive variables. 
#' Default is to perform mean centering.
#' @param reduce logical.  If \code{TRUE} descriptive variables are reduce.  
#' Default is to reduce by SD of each variable in the data matrix.
#' #'@return \item{X}{X dataset}
#'@return \item{y}{y dataset}
#'@return \item{x.scores}{X scores}
#'@return \item{y.scores}{Y scores}
#'@return \item{x.loadings}{X loadings}
#'@return \item{y.loadings}{Y loadings}
#'@return \item{x.weights}{X weigths}
#'@return \item{x.residuals}{X residuals}
#'@return \item{ynames}{levels of y}
#'@return \item{Xnames}{Colnames of X dataset}
#'@return \item{Ncomp}{Number of components of our PLS}
#'@return \item{coef}{Coefficients for prediction}
#' 
#' 
#' @example 
#' 
#' res.pls = plsda.fit(Species ~ ., data = iris, ncomp = 3)
#' print.pls(res.pls)
#' summary.pls(res.pls)

#' @keywords regression multivariate
#' @export

plsda.fit <- function(formula, data, ncomp=2, center=T, reduce=T){
  
  # Check input values 
  
  # formula
  if(inherits(formula, "formula")==FALSE){stop("Error : input formula isn't a formula object !")}
  # data
  if(is.data.frame(data)==FALSE){stop("Error : input data isn't a dataframe object !")}
  
  # Number of columns selected in the formula
  NbXcol = length(attributes(terms(formula, data = data))$term.labels)
  
  # ncomp
  if(is.numeric(ncomp)==FALSE){stop("Error : input ncomp isn't a numeric object !")}
  if(as.integer(ncomp)!=ncomp){stop("Error : input ncomp isn't an integer !")}
  if(ncomp<1){stop("Error : input ncomp cannot be negative !")}
  if(ncomp>NbXcol){stop("Error : input ncomp must be less than the number of descriptive variables included in the formula !")}
  
  # center
  if(!is.logical(center)){"Error : input center isn't a logical value !"}
  if(!is.logical(reduce)){"Error : input reduce isn't a logical value !"}
  
  
  # Get colnames for X and y
  Xcolnames = attributes(terms(formula, data = data))$term.labels
  ycolname = toString(formula[[2]])
  
  # Compare colnames from formula with the data's column names
  
  if(!all(Xcolnames%in%colnames(data))){stop("Error : Descriptives colnames in formula doesn't match with colnames in the data !")}
  if(!ycolname%in%colnames(data)){stop("Error : Target column in formula doesn't match with colnames in the data !")}
  
  X = data[,Xcolnames]
  y = as.factor(data[,ycolname])
  
  y.dm = dummies(y)
  
  y.sd = sapply(data.frame(y.dm),sd)
  y.mean = sapply(data.frame(y.dm),mean)
  
  
  if(center & reduce){ 
    X <- apply(X,2,function(x) return((x-mean(x))/sd(x))) 
    y.dm <- apply(y.dm,2,function(x) return((x-mean(x))/sd(x))) 
  } else if (center & !reduce){ 
    X <- apply(X,2,function(x) return(x-mean(x)))
    y.dm <- apply(y.dm,2,function(x) return(x-mean(x)))
  } else if (!center & reduce){ 
    X <- apply(X,2,function(x) return(x/sd(x))) 
    y.dm <- apply(y.dm,2,function(x) return(x/sd(x))) 
  }
  
  # n, number of rows in the dataset
  n = nrow(data)
  # p, number of x columns selected in the formula
  p = NbXcol
  # r, number of response on the class
  r = nlevels(y)
  ymod = colnames(y.dm)
  
  # Initialization 
  x.loadings = matrix(0, p, ncomp)
  x.scores = matrix(0, n, ncomp)
  y.loadings = matrix(0, r, ncomp)
  y.scores = matrix(0, n, ncomp)
  x.weights = matrix(0, p, ncomp)
 
  
  # Nipals implementation 
  
  for(i in 1:ncomp){
    # First modality
    yA <- matrix(y.dm[,1])
    
    # Weights
    W <- t(X)%*%yA/(t(yA)%*%yA)[[1]]
    W <- W/sqrt((t(W)%*%W)[[1]])
    
    # Convergence 
    repeat {
      Wb <- W
      xs <- X%*%Wb
      
      # Y loadings
      yl <- t(y.dm)%*%xs/(t(xs)%*%xs)[[1]]
      yl <- yl/sqrt((t(yl)%*%yl)[[1]])
      
      # Y scores
      ys <- y.dm%*%yl
      
      # Weights and normalisation
      W <- t(X)%*%ys/(t(ys)%*%ys)[[1]]
      W <- W/sqrt((t(W)%*%W)[[1]])
      
      # convergence min
      gap <- abs(mean(W)-mean(Wb))
      
      if(gap<1e-6){break}
    }
    # X loadings
    xl <- t(X)%*%xs/(t(xs)%*%xs)[[1]]
    a <- t(xs)%*%yA/(t(xs)%*%xs)[[1]]
    
    # Y loadings
    yl <- t(t(xs)%*%y.dm/(t(xs)%*%xs)[1,1])
    
    
    X <- X-xs%*%t(xl)
    y.dm <- y.dm-a[[1]]*xs%*%t(yl)
    
    # Iteration results
    x.loadings[,i] = xl
    x.scores[,i] = xs
    y.loadings[,i] = yl
    y.scores[,i] = ys
    x.weights[,i] = W
    
  }
  # Coefficients 
  Xrotated <- x.weights %*% solve(t(x.loadings)%*%x.weights)
  
  coefs <- Xrotated %*% t(y.loadings)
  coefs <- coefs*y.sd
  intercept <- y.mean
  
  rownames(coefs)=Xcolnames
  colnames(coefs)=ymod
  
  
  classement = rbind(coefs, intercept)
  classnames = append(Xcolnames, "constant")
  rownames(classement)=classnames
  # Return 
  
  #class S3
  res.PLS = list("X" = X,
                 "y" = y, 
                 "x.scores" = x.scores,
                 "y.scores" = y.scores,
                 "x.loadings" = x.loadings,
                 "y.loadings" = y.loadings,
                 "x.weights" = x.weights,
                 "ynames" = levels(y),
                 "Xnames" = Xcolnames,
                 "N_comp" = ncomp,
                 "coef" = coefs,
                 "intercept" = intercept,
                 "classement"= classement,
                 "y_dummies"= y.dm
  )
  class(res.PLS) <- "PLSDA"
  return(res.PLS)
}




# Print PLS

#' print.pls from pls.fit
#'
#' @description
#' Print Coefficients and y.loadings from PLSDA Object
#'
#' @param PLSDA a PLSDA object to print
#'
#' @export
#'
print.pls = function(PLSDA){
  res = rbind(PLSDA$intercept, PLSDA$coef)
  Y_Loadings = PLSDA$y.loadings
  row.names(Y_Loadings) = PLSDA$ynames
  
  
  
  cat("Coefficients : \n")
  print(res)
  cat("\n")
  cat("Y Loadings : \n")
  print(Y_Loadings)
}

# Summary PLS

#' summary.pls from pls.fit
#'
#' @description
#' Summary of a PLSDA Object
#'
#' @param PLSDA a PLSDA object to be summarized
#'
#' @export
#'

summary.pls = function(PLSDA){
  X = PLSDA$X
  
  # X summary
  
  cat("\n")
  cat("Summary for descriptives variables : \n")
  cat("\n")
  print(summary(X))
  
  # Correlation
  cat("\n")
  cat("Correlation between descriptives variables : \n")
  cat("\n")
  print(cor(X))
  
  # Y summary
  Y = PLSDA$y
  cat("\n")
  cat("Summary for target variable : \n")
  cat("\n")
  print(summary(Y))
  
  # Ncomp
  ncomp = PLSDA$N_comp
  
  cat("\n")
  cat("Number of components : \n")
  cat("\n")
  print(ncomp)
  
  # Coef
  Coef = PLSDA$coef
  
  cat("\n")
  cat("Coefficients : \n")
  cat("\n")
  print(Coef)
}
