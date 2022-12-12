### .

#' @title Split Train Test (M2 SISE R Project)
#'
#' @description Split a data set in a train set and a test set.
#'
#'
#' @param data a dataset you want to be split.
#' @param p the threshold to split the data.

#' @example 
#' 
#' new_df = split_train_test(iris)

#' @export

split_train_test<-function(data,p=0.67){
  
  #check if data is a data frame
  if(!is.data.frame(data)){
    stop("The data source must be a data frame")
  }
  
  #check if p in ]0;1[
  if(p>1 || p<=0){
    stop("p must be a number between 0.0 and 1.0")
  }
  
  #select the indices for individuals in the training sample
  i_sample<-sample(1:nrow(data),trunc(nrow(data)*p))
  
  #Output list
  res.split = list("df_train" = data[i_sample,],
                   "df_test" = data[setdiff(1:nrow(data),i_sample),],
                   "Split" = p)

  return(res.split)
}
