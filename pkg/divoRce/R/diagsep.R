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
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#'
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
#'
#' # binary data
#' data(csepdat1)
#' y<-csepdat1$y
#' X<-cbind(1,csepdat1[,2:ncol(csepdat1)])
#' diagsep_worker(y, X, model = "b") 
#'
#' # nominal data 
#' # BCL
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagsep_worker(y,X,model="bcl")
#'
#' # ordinal data
#' data(qcsepdato)
#' y<-qcsepdato$y
#' X<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # SL
#' diagsep_worker(y, X, model = "sl")
#'
#' # OSM
#' diagsep_worker(y, X, model = "os")
#' 
#' # ACL
#' diagsep_worker(y, X, model= "acl")
#'
#' # CL
#' diagsep_worker(y, X, model = "cl")
#' 
diagsep_worker<-function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL)
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
            return(diagsep_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
        } else {
            return(diagsep_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
        }
    }
    model <- match.arg(model,several.ok=FALSE)
  switch(model,
           b = diagsep_b(y=y,X=X,rational=rational, backend = backend, solver = solver),
           bcl= diagsep_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver),
           cl= diagsep_cl(y=y,X=X,rational=rational, backend = backend, solver = solver),
           acl= diagsep_acl(y=y,X=X,rational=rational, backend = backend, solver = solver),       
           sl=diagsep_sl(y=y,X=X,rational=rational, backend = backend, solver = solver),
           os=diagsep_os(y=y,X=X,rational=rational, backend = backend, solver = solver)
           )
  } else {
        model <- "strucvec"
        lout <- linearities(S=S,rational=rational)$index
        offrows <- seprows_worker(S=S,rational=rational)$offrows
        typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(S)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
        reccdim <- reccone_worker(S=S,rational=rational)$reccdim
        offcols <- sepcols_worker(S=S,rational=rational, backend = backend, solver = solver)$offcols 
        out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = model)
        class(out) <- out$class <- "sepmod"
        return(out)
     }
  }




#' Detailed separation diagnostic for sequential (continuation-ratio) ordinal response models.
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X for which we have separation. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we internally coerce to ordered factor interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithemtic be used?
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
#' 
diagsep_sl<-function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
{
  #if(!is.ordered(y)) stop("This function needs ordered outcomes.")  
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y=y,X=X)
  seqout <- lapply(splitdat,function(l) diagsep_b(y=l$y,X=l$X,rational=rational, backend = backend, solver = solver))
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
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
#' 
diagsep_os<-function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(!is.ordered(y)) y <- as.ordered(y)
  Xstar <- os_Xstar(y=y,X=X,label=TRUE,rational=rational) #ordinal
  lout <- linearities_os(y=y,X=X,rational=rational)$index
  offrows <- seprows_os(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_os(y=y,X=X,rational=rational)$reccdim
  offcols <- sepcols_os(y=y,X=X,rational=rational, backend = backend, solver = solver)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "os")
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
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
#' 
#' 
diagsep_acl<-function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(!is.ordered(y)) y <- as.ordered(y)
  Xstar <- acl_Xstar(y=y,X=X,label=TRUE,rational=rational) 
  lout <- linearities_acl(y=y,X=X,rational=rational)$index
  offrows <- seprows_acl(y=y,X=X,rational=rational)$offrows
  typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation") 
  reccdim <-  reccone_acl(y=y,X=X,rational=rational)$reccdim
  offcols <- sepcols_acl(y=y,X=X,rational=rational, backend = backend, solver = solver)$offcols 
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
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
#' 
diagsep_bcl<-function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
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
  offcols <- sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver)$offcols 
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
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
diagsep_b<-function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
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
  offcols <- sepcols_b(y=y,X=X,rational=rational, backend = backend, solver = solver)$offcols 
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
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
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
#'
#' 
#' 
diagsep_cl<-function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
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
  offcols <- sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver)$offcols 
  out <- list(separation=(typ!="Overlap"),septype=typ,nr.offrows=dim(offrows)[1],reccdim=reccdim,offrows=offrows,nr.offcols=length(offcols),offcols=offcols, modelclass = "cl")
  class(out) <- out$class <- "sepmod"
  out
}
