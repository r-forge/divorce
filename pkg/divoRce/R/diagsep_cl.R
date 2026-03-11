#' Detailed separation diagnostic for cumulative link ordinal response models.
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we corece to ordered factor and interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithemtic be used?
#' 
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offrows offending rows in X, the ones which are not linearities  (note that individual observations can be duplicated in the cone as they may lie on the boundary) 
#' \item nr.offcols number of columns of the design matrix that have separation
#' \item offcols columns of the design matrix that have separation. It is given as category::effect.  
#' }
#' 
#' @export
#'
#' 
#' @examples
#' data(qcsepdato)
#' y<-qcsepdato$y
#' X<-qcsepdato[,2:ncol(qcsepdato)]
#' diagsep_cl(y,X)
#' 
diagsep_cl<-function(y,X,rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  #if(ratcols) X<- rcdd::q2d(X)
  if(!is.ordered(y)) y <- as.ordered(y)
  #if(!is.ordered(y)) stop("This function needs ordered outcomes.")
  Xstar <- cl_Xstar(y,X,label=TRUE,rational=rational) #ordinal
  lout <- linearities_cl(y,X,rational=rational)$index
#  if (length(lout)==0){
#      offrows <-  X
#      attr(offrows,"assign") <- NULL
#  } else {
#      lis <- unique(rownames(Xstar)[-lout])
#      offrows <- X[lis,,drop=FALSE]
#      attr(offrows,"assign") <- NULL
                                        #    }
  offrows <- seprows_cl(y,X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_cl(y,X,rational=rational)$reccdim
  offcols <- detect_sepcols_cl(y,X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols)
  class(out) <- out$class <- "sepmod"
  out
}
