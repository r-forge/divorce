#' Identify separation columns
#' 
#' This function identifies the columns in a design matrix/structure vector matrix that are responsible for separation. It calls lower level functions if given an argument or chooses based on the response type.
#' 
#' @param y a categorical outcome vector.  Can be binary, categorial or ordinal. Works best if it is an ordered or unordered factor appropriate for the model to be fitted but can also be numeric, boolean or character. If \code{y} is not a factor, it is treated as a nominal (categorical) outcome. with the alphanumerically lowest value being the reference.   
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param S a matrix of structure vectors. If given \code{y}, \code{X} and \code{model} are ignored.
#' @param rational Should rational arithmetic be used? 
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by \code{ROI_applicable_solver()} for "ROI".  
#' @export
sepcols_worker<- function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL) {
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
            return(sepcols_cl(y=y,X=X,rational=rational, backend=backend, solver=solver))
        } else {
            return(sepcols_bcl(y=y,X=X,rational=rational, backend=backend, solver=solver))
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b = sepcols_b(y=y,X=X,rational=rational, backend=backend, solver=solver),
           bcl = sepcols_bcl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           cl = sepcols_cl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           acl = sepcols_acl(y=y,X=X,rational=rational, backend=backend, solver=solver),       
           sl =sepcols_sl(y=y,X=X,rational=rational, backend=backend, solver=solver),
           os =sepcols_os(y=y,X=X,rational=rational, backend=backend, solver=solver)
           )
    } else {
        # for S given
        if(!is.matrix(S)) stop("S must be a matrix.")
        ratcols <- rat_cols(S)
        if(ratcols) rational <- TRUE
        if(ratcols) {
            # to turn a rational S into a rational Xstar we need to convert to floating and multiply with -1
            Stmp <- rcdd::q2d(S) 
            Xstar <- -1*Stmp
            Xstar <- rcdd::d2q(Xstar)
            #row.names(Xstar) <- row.names(S)
            #colnames(Xstar) <- colnames(S)
        } else {
            Xstar <- -1*S
        }
     ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the apper
    #A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
    #           - diag(ncol(Xstar)), # lower bounds -beta <= 1
    #             diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    #b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
    #       rep(1,ncol(Xstar)),   #the right hand side is 1
    #       rep(1,ncol(Xstar))    #the right hand side is 1
    #       )
    #if(rational){
    #    A1 <- rcdd::d2q(A1)
    #    b1 <- rcdd::d2q(b1)
    #}
    ## making the H rep 
    #hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   #a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   #a <- as.numeric(a)
   #if(rational) a <- rcdd::d2q(a)
   ### maximization with lpcdd
   #lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
                                        #if(rational) lso <- rcdd::q2d(lso)
    lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
   )
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)    
   }
 }

detect_sepcols <- sepcols_worker


#' Identify separation columns for binary models
#'
#' This function identifies the columns in a binary model design matrix that are responsible for separation. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the dependent variable. Must be binary. If we encounter a factor, we use the levels of the factor and thus also the specified reference. For anything else, we use the lowest alphanumeric category as reference. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_b<- function(y, X, rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL) { 
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    y <- as.factor(y)
    backend <- .divorce_match_backend(backend)
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- b_Xstar(y=y, X=X, label=TRUE, rational=rational)
    ##here we check whether X has full column rank otherwise this check won't work properly.
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")     
    ## constraints
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the paper
    lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
    )
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   out
}

detect_sepcols_b <- sepcols_b 

#' Identify separation columns in sequential models
#'
#' This function identifies the columns in a sequential model design matrix that are responsible for separation. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the dependent variable. Works for ordered factors, factors, characters numeric of boolean. Works best if it is an ordered factor, else we treat it alphanumerically ascending just as `as.ordered`. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_sl <- function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
{
  backend <- .divorce_match_backend(backend)
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y=y,X=X)
  seqout <- lapply(splitdat,function(l) detect_sepcols_b(y=l$y,X=l$X,rational=rational, backend=backend, solver=solver))
  return(seqout)
}

detect_sepcols_sl<- sepcols_sl

#' Identify separation columns in baseline-category models
#'
#' This function identifies the columns in a design matrix for a baseline-category link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, numeric, character or boolean. Works best if it is a factor because then we keep specified reference caetgory, else we use the alphanumerically lowest value. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_bcl<- function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL) {
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    y <- as.factor(y)
    backend <- .divorce_match_backend(backend)
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- bcl_Xstar(y=y,X=X,label=TRUE,rational=rational)
    ##here we check whether X has full column rank otherwise this check won't work properly.
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")      
    lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
   )
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
   }

detect_sepcols_bcl <- sepcols_bcl 

#' Identify separation columns in cumulative link models
#'
#' This function identifies the columns in a design matrix for a cumulative link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is an ordered factor, else we treat it alphanumerically ascending just as `as.ordered`. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_cl<- function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL) {
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    y <- as.ordered(y)
    backend <- .divorce_match_backend(backend)
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- cl_Xstar(y=y,X=X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
    lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
   )
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
}


detect_sepcols_cl <- sepcols_cl



#' Identify separation columns in adjacent-category models
#'
#' This function identifies the columns in a design matrix for an adjacent-category link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is an ordered factor, else we treat it alphanumerically ascending just as `as.ordered`. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_acl <- function(y,X,rational=FALSE,backend = c("rcdd", "ROI"), solver = NULL)
    {
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    y <- as.ordered(y)
    backend <- .divorce_match_backend(backend)
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- acl_Xstar(y=y,X=X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
     lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
   )
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
 }

detect_sepcols_acl<- sepcols_acl

#' Identify separation columns in ordered stereotype models
#'
#' This function identifies the columns in a design matrix for an ordered stereotype model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is an ordered factor, else we treat it alphanumerically ascending just as `as.ordered`. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".  
sepcols_os <- function(y,X,rational=FALSE, backend = c("rcdd", "ROI"), solver = NULL)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    y <- as.ordered(y)
    backend <- .divorce_match_backend(backend)
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- os_Xstar(y=y,X=X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
    lso <- .divorce_detect_sepcols_lp(
       Xstar,
       rational = rational,
       backend = backend,
       solver = solver
   )
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
   }


detect_sepcols_os <- sepcols_os
