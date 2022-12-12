
library(ggplot2)
library(tidyverse)
library(ggforce)
library(plotly)



#' PLOT Individuals
#'
#' @description 
#' Show individuals on factorial plan
#'
#' @param PLSDA 
#' an object PLSDA
#'
#' @param Axe1 
#' the number of the component on x axis
#'  
#' @param Axe2 
#' the number of the component on y axis
#' @returns 
#'  Individuals plots for PLSDA
#' @export
#'

individuals_plot <- function(PLSDA, Axe1 = 1, Axe2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = PLSDA$N_comp
  
  if(Axe1 > n_comp | Axe2 > n_comp){
    
    print("La valeur de l'axe est supérieure au nombre de composants")
  }else 
    {
      x= PLSDA$x.scores[,Axe1]
      y= PLSDA$x.scores[,Axe2]
   
    ind <- plot_ly(x = x, y =y , color = PLSDA$y, type = "scatter", mode = "markers")
    
    ind <- ind %>% layout(
      title = "Individuals",
      legend=list(title=list(text='Color')),
      xaxis = list(title=paste0("Comp ",Axe1)),
      yaxis = list(title=paste0("Comp ",Axe2))
    )
    
    return(ind)
  }
}

#' scree plot
#'
#' @description 
#' Show scree plot
#'
#' @param PLSDA 
#' an object PLSDA
#' @returns 
#'  scree plot for PLSDA
#' @export
#'

plsda_scree_plot=function(PLSDA){
  
  
  library(plotly)
  
  # Correlations 
  X=PLSDA$X
  corX=cor(X)
  ncomp=ncol(PLSDA$X)
  
  
  # The eigenvalues
  eigenvalues=eigen(corX)$values
  test=eigenvalues > 1
  cols <- ifelse(test, "rgba(139, 0, 0, 0.3)", "rgba(0, 0, 255, 0.3)")
  lab=1:ncomp
  #  plot
  plot_ly(x=lab, y = eigenvalues,type = "bar",color = I(cols))%>%
    layout(
      xaxis=list(title="Components"),
      yaxis=list(title="Eigenvalues"),
      title="Selection of components"
    )
}

