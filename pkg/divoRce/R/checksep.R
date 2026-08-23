#' General separation check worker function.  
#'
#' This function checks for (quasi-) complete separation by calling the appropriate low-level functions.
#'
#' The function uses either a response vector y and a design matrix X, or a structure vector matrix S. If S is given, y and X and model are ignored. 
#'
#' 
#'
#' @param y a categorical outcome vector 
#' @param X a design matrix, e.g. generated via a call to \code{\link{model.matrix}}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials). 
#' @param S a matrix of structure vectors
#' @param rational should rational arithmetic be used
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @export
checksep_worker<- function(y, X, S, rational=FALSE, model=c("bcl", "b","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL){
    backend <- .divorce_match_backend(backend)
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
            return(checksep_cl(y=y,X=X,rational=rational, backend=backend, solver=solver))
        } else {
            return(checksep_bcl(y=y,X=X,rational=rational, backend=backend, solver=solver))
            
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b= checksep_b(y=y,X=X,rational=rational, backend=backend, solver=solver),
           bcl= checksep_bcl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           cl= checksep_cl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           acl= checksep_acl(y=y,X=X,rational=rational, backend=backend, solver=solver),       
           sl=checksep_sl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           os=checksep_os(y=y,X=X,rational=rational, backend=backend, solver=solver)
           )
    } else {
        # for S given
        if(!is.matrix(S)) stop("S must be a matrix.")
        ratcols <- rat_cols(S)
        if(ratcols) rational <- TRUE    
        cal <- .divorce_check_sep_lp(
        S,
        rational = rational,
        backend = backend,
        solver = solver
        )
        out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
        return(out)
     }
}


#' Separation check for cumulative link models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' 
checksep_cl<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL){
   backend <- .divorce_match_backend(backend)
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       S <- structure_vectors(y=y, X=X, label=FALSE, rational = rational, model = "cl")
   } else {
       stop("For 2 categories, please use model = 'b'.")
   }
   cal <- .divorce_check_sep_lp(
        S,
        rational = rational,
        backend = backend,
        solver = solver
    )
   out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
   return(out)
}


#' Separation check for baseline category models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#' @import rcdd
checksep_bcl<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL){
   backend <- .divorce_match_backend(backend)
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(is.ordered(y) & length(unique(y))>2) stop("For ordered y, please specify the desired model in the model argument.") 
   S <- structure_vectors(y=y, X=X, label=FALSE, rational = rational, model = "bcl")
   cal <- .divorce_check_sep_lp(
        S,
        rational = rational,
        backend = backend,
        solver = solver
    )
   out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
   return(out)
}

#' Separation check for binary models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
checksep_b<- checksep_bcl


#' Separation check for sequential (continuation-ratio) models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
checksep_sl<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL){
   backend <- .divorce_match_backend(backend)
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   splitdat <- create_bseq(y=y,X=X)
   seqout <- sapply(splitdat,function(l) checksep_b(y=l$y,X=l$X,rational=rational, backend=backend, solver=solver))
   return(any(seqout))
}

#' Separation check for adjacent-category link models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
checksep_acl<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL){
   backend <- .divorce_match_backend(backend) 
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
   S <- structure_vectors(y=y, X=X, label=FALSE, rational = rational, model = "acl")
   } else {
       stop("For 2 categories, please use model = 'b'.")    
   }
   cal <- .divorce_check_sep_lp(
        S,
        rational = rational,
        backend = backend,
        solver = solver
   )
   out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
   return(out)
}

#' Separation check for ordered stereotype models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
checksep_os<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL){
   backend <- .divorce_match_backend(backend)
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
   S <- structure_vectors(y=y, X=X, label=FALSE, rational = rational, model = "os")
   } else {
       stop("For 2 categories, please use model = 'b'.")
   }   
    cal <- .divorce_check_sep_lp(
        S,
        rational = rational,
        backend = backend,
        solver = solver
    )
   out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
   return(out)
}


    
#' General overlap check.
#'
#' This function checks for overlap by calling the appropriate low-level functions. It is not generic.
#'
#' The function uses either a response vector y and a design matrix X or a structure vector matrix S. If S is given, y and X and model are ignored.
#'
#' @param y outcome vector 
#' @param X design matrix
#' @param S structure vector matrix
#' @param rational should rational arithmetic be used.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @return a Boolean; either 'TRUE' if there is overlap or 'FALSE' if not.
#'
#' @export
check_overlap <- function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL){
  if(missing(model)) model <- NULL
  if(missing(S)) {
      !isTRUE(checksep_worker(y=y, X=X, rational=rational, model = model, backend = backend, solver = solver))
  } else {
      !isTRUE(checksep_worker(S=S, rational=rational, backend = backend, solver = solver))
  }
}


checkovl <- check_overlap


