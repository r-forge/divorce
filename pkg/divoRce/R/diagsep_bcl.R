#' Detailed separation diagnostic for baseline-category link models. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the observations for which we have separation.   
#'
#' 
#' @param y the nominal outcome variable. Works best if it is a factor but can also be numeric, boolean or character. In the case of the latter we coerce to factor and the lowest alphanumeric entry is used as reference (just as 'as.ordered' is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offobs offending observations 
#' \item nr.offcols number of columns of the design matrix that have separation
#' \item offcols columns of the design matrix that have separation. It is given as category::effect.  
#' }
#' 
#' @export
#'
#' @examples
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-qcsepdatm[,2:ncol(qcsepdatm)]
#' diagsep_bcl(y,X)
#' 
diagsep_bcl<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(!is.factor(y)) y <- factor(y)
  if(is.ordered(y)) stop("This function needs non-ordered outcomes.")
  Xstar <- bcl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
  lout <- linearities_bcl(y,X,rational=rational)$index
  #if(ratcols) Xstar <- rcdd::q2d(Xstar)
  #if (length(lout)==0){
  #    offobs <-  Xstar
  #    attr(offobs,"assign") <- NULL
  #  } else {
  #    offobs <- Xstar[-lout,,drop=FALSE]
  #    attr(offobs,"assign") <- NULL
                                        #  }
  offobs <- sepobs_bcl(y,X,rational=rational)$offobs
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
  reccdim <- reccone_bcl(y,X,rational=rational)$reccdim
  offcols <- detect_sepcols_bcl(y,X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offobs=dim(offobs)[1],reccdim=reccdim,offobs=offobs,nr.offcols=length(offcols),offcols=offcols)
  class(out) <- out$class <- "sepmod"
  out
}


