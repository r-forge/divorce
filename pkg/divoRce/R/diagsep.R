#' Detailed separation diagnostic for all categorical outcomes. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X/S for which we have separation.   
#'
#' The function uses either a response vector y and a design matrix X, or a structure vector matrix S. If S is given, y and X and model are ignored. 
#' 
#' @param y the outcome variable. Can be binary, categorial or ordinal. Works best if it is an ordered or unordered factor but can also be numeric, boolean or character. If y is not a factor, it is treated as a nominal (categorical) outcome. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param S a matrix of structure vectors
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offrows offending rows in X 
#' \item nr.offcols number of columns of the design matrix that have separation
#' \item offcols columns of the design matrix that have separation. It is given as category::effect.  
#' }
#' 
#' @export
#'
#' @examples
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagsep(y,X,model="bcl")
#' 
diagsep<-function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","osm"))
{
  if(missing(S))
  {
  if(length(unique(y))<2) stop("There is only one value in y.")     
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(missing(model)) model <- NULL
    if(is.null(model))
    {
        warning("Default model class used.","\n")
        if(is.ordered(y) & length(unique(y))>2)
        {
            return(diagsep_cl(y=y,X=X,rational=rational))
        } else {
            return(diagsep_bcl(y=y,X=X,rational=rational))
        }
    }
    model <- match.arg(model,several.ok=FALSE)
  switch(model,
           b = diagsep_b(y=y,X=X,rational=rational),
           bcl= diagsep_bcl(y=y,X=X,rational=rational),
           cl= diagsep_cl(y=y,X=X,rational=rational),
           acl= diagsep_acl(y=y,X=X,rational=rational),       
           sl=diagsep_sl(y=y,X=X,rational=rational),
           osm=diagsep_osm(y=y,X=X,rational=rational)
           )
     } else {
        lout <- linearities(S=S,rational=rational)$index
        offrows <- seprows(S=S,rational=rational)$offrows
        typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(S)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
        reccdim <- reccone(S=S,rational=rational)$reccdim
        offcols <- sepcols(S=S,rational=rational)$offcols 
        out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = model)
        class(out) <- out$class <- "sepmod"
        return(out)
     }
  }



## diagsepOLD<-function(y,X,rational=FALSE)
## {
##   if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
##   ratcols <- rat_cols(X)
##   if(ratcols) rational <- TRUE
##   if(is.ordered(y) & length(unique(y))>2) { #TODO: Do we have any other way to check whether y is ordinal?
##         Xstar <- cl_Xstar(y, X, label=TRUE, rational=rational) # for ordinal
##     } else {
##         Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational) #for all nominal and binary
##   }
##   lout <- linearities(y,X,rational=rational)$index
##   if(ratcols(Xstar)) Xstar <- rcdd::q2d(Xstar)
##   if (length(lout)==0){
##       offobs <-  Xstar
##       attr(offobs,"assign") <- NULL
##     } else {
##       offobs <- Xstar[-lout,,drop=FALSE]
##       attr(offobs,"assign") <- NULL
##     }
##   typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
##   reccdim <- reccone(y,X,rational=rational)$reccdim
##   offcols <- detect_sepcols(y,X,rational=rational)$offcols 
##   out <- list(separation=(typ!="Overlap"),septype=typ,nr.offobs=dim(offobs)[1],reccdim=reccdim,offobs=offobs,nr.offcols=length(offcols),offcols=offcols)
##   class(out) <- out$class <- "sepmod"
##   out
## }

#' Detailed separation diagnostic for sequential (continuation-ratio) ordinal response models.
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we internally coerce to ordered factor interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithemtic be used?
#' 
#'
#' @return an object of class 'sepmod_sl'. It is a list with the elements corresponding to each category. The elements are lists with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offrows offending rows in X, the ones which are not linearities  (note that individual rows can be duplicated in the cone as they may lie on the boundary) 
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
#' diagsep_sl(y,X)
#' 
diagsep_sl<-function(y,X,rational=FALSE)
{
  #if(!is.ordered(y)) stop("This function needs ordered outcomes.")  
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y=y,X=X)
  seqout <- lapply(splitdat,function(l) diagsep_b(y=l$y,X=l$X,rational=rational))
  class(seqout) <- "sepmod_sl"
    
  return(seqout)
}

#' Detailed separation diagnostic for ordered stereotype models.
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we coerce to ordered factor interpret the ordering as alphanumerically increasing (just like 'as.ordered' is doing).
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
#' diagsep_osm(y,X)
#' 
diagsep_osm<-function(y,X,rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(!is.ordered(y)) y <- as.ordered(y)
  Xstar <- osm_Xstar(y=y,X=X,label=TRUE,rational=rational) #ordinal
  lout <- linearities_osm(y=y,X=X,rational=rational)$index
  offrows <- seprows_osm(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_osm(y=y,X=X,rational=rational)$reccdim
  offcols <- detect_sepcols_osm(y=y,X=X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "osm")
  class(out) <- out$class <- "sepmod"
  return(out)
}


#' Detailed separation diagnostic for adjacent-category link ordinal response models.
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we coerce to ordered factor and interpret the ordering as alphanumerically increasing (just as 'as.ordered' is doing).
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
#' diagsep_acl(y,X)
#' 
diagsep_acl<-function(y,X,rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(!is.ordered(y)) y <- as.ordered(y)
  Xstar <- acl_Xstar(y=y,X=X,label=TRUE,rational=rational) 
  lout <- linearities_acl(y=y,X=X,rational=rational)$index
  offrows <- seprows_acl(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_acl(y=y,X=X,rational=rational)$reccdim
  offcols <- detect_sepcols_acl(y=y,X=X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "acl")
  class(out) <- out$class <- "sepmod"
  out
}

#' Detailed separation diagnostic for baseline-category link models. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation.   
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
#' \item offrows offending rows in X 
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
  Xstar <- bcl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
  lout <- linearities_bcl(y=y,X=X,rational=rational)$index
  offrows <- seprows_bcl(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
  reccdim <- reccone_bcl(y=y,X=X,rational=rational)$reccdim
  offcols <- detect_sepcols_bcl(y=y,X=X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "bcl")
  class(out) <- out$class <- "sepmod"
  out
}

#' Detailed separation diagnostic for binary outcomes. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation.   
#'
#' 
#' @param y the binary outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character. We coerce to factor internally. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offrows offending rows in X
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
  Xstar <- bcl_Xstar(y=y, X=X, label=TRUE, rational=rational)
  vrep <- cbind(0, 0, Xstar)
  if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
  lout <- rcdd::linearity(vrep, rep = "V") #returns numeric anyway
  #if(rat_cols(Xstar)) Xstar <- rcdd::q2d(Xstar)
  if (length(lout)==0){
      offrows <-  X
      attr(offrows,"assign") <- NULL
    } else {
      offrows <- X[-lout,,drop=FALSE]
      attr(offrows,"assign") <- NULL
  }
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank 
  offcols <- detect_sepcols_b(y=y,X=X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "b")
  class(out) <- out$class <- "sepmod"
  out
  }


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
  Xstar <- cl_Xstar(y=y,X=X,label=TRUE,rational=rational) #ordinal
  lout <- linearities_cl(y=y,X=X,rational=rational)$index
  offrows <- seprows_cl(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_cl(y=y,X=X,rational=rational)$reccdim
  offcols <- detect_sepcols_cl(y=y,X=X,rational=rational)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "cl")
  class(out) <- out$class <- "sepmod"
  out
}
