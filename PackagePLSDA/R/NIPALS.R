#' This function performs NIPALS algorithm, i.e. the singular-value
#' decomposition (SVD) of a data table.
#' 
#' The NIPALS algorithm (Non-linear Iterative Partial Least Squares) has been
#' developed by H. Wold at first for PCA and later-on for PLS. It is the most
#' commonly used method for calculating the principal components of a data set.
#' It gives more numerically accurate results when compared with the SVD of the
#' covariance matrix, but is slower to calculate.



#'@title NIPALS: Non-linear Iterative Partial Least Squares
#'
#'@description
#' NIPALS algorithm
#'
#'@details
#'The function \code{nipals} performs Principal Components Analysis of a data
#'matrix with no missing values.
#'
#'@param Data A numeric matrix or data frame 
#'@param comps Number of components to be calculated (by default 2)


#'@return An object of class \code{"nipals"}, basically a list with the
#'following elements:
#'
#'When the analyzed data contain missing values, the help interpretation tools
#'(e.g. \code{cor.xt, disto, contrib, cos, dmod}) may not be meaningful, that
#'is to say, some of the results may not be coherent.
#'@return \item{values}{The pseudo eigenvalues}
#'@return \item{scores}{The extracted scores (i.e. components)}
#'@return \item{loadings}{The loadings}
#'@return \item{cor.xt}{Correlations between the variables and the scores}
#'@return \item{disto}{Squared distance of the observations to the origin}
#'@return \item{contrib}{Contributions of the observations (rows)}
#'@return \item{dmod}{Distance to the Model}
#'@export



nipals <- function (X, ncomp = 2)
{
  X = as.matrix(X)
  #-# Checklist to see if the function can run #-#
  #~ Matrix is numeric
  if (!is.numeric(X))
    stop ("Matrix is not numeric")
  #~ Col not void
  if (any(colSums(!is.na(X)) == 0) | any(rowSums(!is.na(X)) == 0 ))
    stop("Some rows or columns are void", 
         "Function can't run properly", call)

  
  #-# Preparation objects #-#
  n = nrow(X)
  p = ncol(X)
  nc = ncomp
  X.old = X
  Tm = matrix(0, n, nc) # scores
  Ph = matrix(0, p, nc) # loadings
  eigvals = rep(0, nc) # eigenvalues
  
  
  #-# Iterative process #-#
  
  #### Only for dataset with no missing values (for now)###
  
  for (h in 1:nc)
  {
    T.new = X.old[,1]
    ph.old = rep(1, p)
    ph.new = rep(1, p)
    iter = 1
    repeat
    {
      ph.new = t(X.old) %*% T.new / sum(T.new^2)
      ph.new = ph.new / sqrt(sum(ph.new^2))
      T.new = X.old %*% ph.new
      
      ph.aux = sum((ph.new - ph.old)^2)
      ph.old = ph.new
      # check convergence
      if (ph.aux < 1e-06 || iter==100) break
      iter = iter + 1
    }
    Tm[,h] = T.new
    Ph[,h] = ph.new
    X.new = X.old - T.new %*% t(ph.new)
    X.old = X.new
    eigvals[h] = sum(T.new^2) / (n-1)
  }
  dimnames(Tm) = list(rownames(X), paste(rep("t",nc), 1:nc, sep=""))
  dimnames(Ph) = list(colnames(X), paste(rep("p",nc), 1:nc, sep=""))
  
  # =======================================================#
  # =======================================================#
  
  #-# Eigenvalues #-#
  eig.perc = 100 * eigvals / p
  eigs = data.frame(values=eigvals, percentage=eig.perc, cumulative=cumsum(eig.perc))
  rownames(eigs) = paste(rep("v",nc), 1:nc, sep="")
  
  #-# correlation between components and variables #-#
  cor.sco = cor(X, Tm)
  
  #-# Individuals contribution #-#
  ConInd = (100/n) * Tm^2 %*% diag(1/eigvals)
  dimnames(ConInd) = list(rownames(X), paste(rep("ctr",nc),1:nc,sep=".") )
  
  #-# Results #- 
  res = list(values = eigs, 
             scores = Tm, 
             loadings = Ph, 
             cor.var = cor.sco, 
             contrib = ConInd 
             )
  
  class(res) = "nipals"
  return(res)
}



##################################################
