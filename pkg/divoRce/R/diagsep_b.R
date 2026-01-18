#' Detailed separation diagnostic for binary outcomes. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the observations for which we have separation.   
#'
#' 
#' @param y the binary outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character. We coerce to factor internally. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whetehr there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offobs offending observations 
#' \item nr.offcols number of columns of X that have separation
#' \item offcols columns of X that have separation
#' }
#'
#' 
#' @export
#'
#' @examples
#' data(csepdat1)
#' y<-csepdat1$y
#' X<-cbind(1,csepdat1[,2:ncol(csepdat1)])
#' diagsep_b(y,X) #separation
diagsep_b<-function(y, X, rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE  
  y <- as.factor(y)
                                        #TODO: What with NA?
  if(ratcols) X <- rcdd::q2d(X)
  if(length(unique(y))>2) stop("This function needs binary outcomes.")   
  Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational)
  vrep <- cbind(0, 0, Xstar)
  if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
  lout <- rcdd::linearity(vrep, rep = "V") #returns numeric anyway
  #if(rat_cols(Xstar)) Xstar <- rcdd::q2d(Xstar)
  if (length(lout)==0){
      offobs <-  X
      attr(offobs,"assign") <- NULL
    } else {
      offobs <- X[-lout,,drop=FALSE]
      attr(offobs,"assign") <- NULL
  }
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank 
  offcols <- detect_sepcols_b(y,X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offobs=dim(offobs)[1],reccdim=reccdim,offobs=offobs,nr.offcols=length(offcols),offcols=offcols)
  class(out) <- out$class <- "sepmod"
  out
  }

