#' This function identifies the columns in a design matrix that are responsible for separation. It calls lower level functions if given an argument or chooses based on the response type. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression.
#'
#' This function assumes that either a baseline-category link model for categorical outcomes (incl. binary )is wanted, or a cumulative link model for ordinal outcomes. For adjacent-category link, sequential link or ordered stereotypes models use the subfunctions detect_sepcols_acl, detect_sepcols_sl and detect_sepcols_os respectively.  
#' 
#' @param y the dependent variable.  
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param rational Should rational arithmetic be used? 
#' @param model the model type. If not given, we default to cumulativie link for ordered factors and baseline-category for everything else. 
#' 
#' @export
#'
#'
#'
detect_sepcols<- function(y,X, rational=FALSE, model=c("bcl","cl","acl","sl","osm")){
    if(length(unique(y))<2) stop("There is only one value in y.")
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(missing(model)) model <- NULL
    if(is.null(model))
    {
       # cat("I'm not sure which model you want to fit, so I default to the most common ones.","\n")
        if(is.ordered(y) & length(unique(y))>2)
        {
            detect_sepcols_cl(y,X,rational=rational)
        } else {
            detect_sepcols_bcl(y,X,rational=rational)
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           bcl= detect_sepcols_bcl(y,X,rational=rational),
           cl= detect_sepcols_cl(y,X,rational=rational),
           acl= detect_sepcols_acl(y,X,rational=rational),       
           sl=detect_sepcols_sl(y,X,rational=rational),
           osm=detect_sepcols_osm(y,X,rational=rational)
           )
}

#' @rdname detect_sepcols
#' @export
sepcols <- detect_sepcols


## detect_sepcolsOLD<- function(y,X, rational=FALSE) {
##     if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
##     #this may be the only one we need; check this out again.
##     ratcols <- rat_cols(X)
##     if(ratcols) rational <- TRUE 
##     ## setting up X*
##      if(is.ordered(y) & length(unique(y))>2) { #TODO: Do we have any other way to check whether y is ordinal?
##         Xstar <- cl_Xstar(y, X, label=TRUE, rational=rational) # for ordinal
##     } else {
##         Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational) #for all nominal and binary
##     }
##     ##here we check whether X has full column rank otherwise this check won't work properly.
##     if(ratcols) X <- rcdd::q2d(X)
##     if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")     
##     ## constraints
##     ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
##     ## left hand side just inequalities to folow the linear program in the paper
##     if(rational) Xstar <- rcdd::q2d(Xstar) # we need this to do the calculations; quicker than setting up struc_vec
##     A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
##                - diag(ncol(Xstar)), # lower bounds -beta <= 1
##                  diag(ncol(Xstar))) # upper bounds beta <= 1
##     ## the right hand side are scalars 
##     b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
##            rep(1,ncol(Xstar)),   #the right hand side is 1
##            rep(1,ncol(Xstar))    #the right hand side is 1
##            )
##     if(rational){
##         A1 <- rcdd::d2q(A1)
##         b1 <- rcdd::d2q(b1)
##     }
##     ## making the H rep 
##    hrep<-rcdd::makeH(a1=A1,b1=b1)
##     ## objective function
##    a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
##    a <- as.numeric(a)
##    if(rational) a <- rcdd::d2q(a)
##    ## maximization with lpcdd
##    lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
##    if(rational) lso <- rcdd::q2d(lso)
##    ## if not 0, it is separation column
##    offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
##    offcols <- colnames(Xstar)[offflag]
##    out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
##    out
## }
#' This function identifies the columns in a binary model design matrix that are responsible for separation. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the dependent variable. Must be binary. If we encounter a factor, we use the levels of the factor and thus also the specified reference. For anything else, we use the lowest alphanumeric category as reference. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param rational should rational arithmetic be used.
#' 
#' @export
detect_sepcols_b<- function(y,X,rational=FALSE) { 
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational)
    ##here we check whether X has full column rank otherwise this check won't work properly.
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")     
    ## constraints
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the paper
    if(rational) Xstar <- rcdd::q2d(Xstar)
    A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
               - diag(ncol(Xstar)), # lower bounds -beta <= 1
                 diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
           rep(1,ncol(Xstar)),   #the right hand side is 1
           rep(1,ncol(Xstar))    #the right hand side is 1
           )
    if(rational){
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    ## making the H rep 
   hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   a <- as.numeric(a)
   if(rational) a <- rcdd::d2q(a)
   ## maximization with lpcdd
   lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
   if(rational) lso <- rcdd::q2d(lso)
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   out
}

#' @rdname detect_sepcols_b
#' @export
sepcols_b <- detect_sepcols_b

#' This function identifies the columns in a sequential model design matrix that are responsible for separation. 
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the dependent variable. Works for ordered factors, factors, characters numeric of boolean. Works best if it is an ordered factor, else we treat it alphanuemrically ascneidng just as as.ordered. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' @param rational should rational arithmetic be used.
#' 
#' @export
detect_sepcols_sl <- function(y,X,rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y,X)
  seqout <- lapply(splitdat,function(l) detect_sepcols_b(l$y,l$X))
  seqout
}

#' @rdname detect_sepcols_sl
#' @export
sepcols_sl<- detect_sepcols_sl

## detect_sepcols_bOLD<- function(y,X) { 
##     ## We use y thus:
##     ## If we encounter a factor, we use the levels of the factor and thus also the specified reference 
##     ## For anything else, we use the lowest alphanumeric category as reference
##     if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
##     y <- as.factor(y)
##     refcat <- levels(y)[1]
##        # {
         
##      #    othcat <- levels(y)[2]
##       #  } else { 
##      #    refcat <- sort(unique(y))[1]
##      #    othcat <- sort(unique(y))[2]
##      #    }   
##     ## setting up Z*
##     Xstar[y==refcat,] <- -Xstar[y==refcat,]
##     ## constraints
##     ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
##     ## left hand side just inequalities to folow the linear program in the paper
##     A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
##                - diag(ncol(Xstar)), # lower bounds -beta <= 1
##                  diag(ncol(Xstar))) # upper bounds beta <= 1
##     ## the right hand side are scalars 
##     b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
##            rep(1,ncol(Xstar)),   #the right hand side is 1
##            rep(1,ncol(Xstar))    #the right hand side is 1
##            )
##     ## making the H rep 
##    hrep<-rcdd::makeH(a1=A1,b1=b1)
##     ##turn into rational arithmetic
##     ## objective function
##    a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
##    a <- as.numeric(a)
##    ## maxmization with lpcdd
##    lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
##    ## if 1 or -1 it is separation column, else not
##    offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
##    #intind <- grep("(Intercept)",colnames(Xstar))
##    #offflag[intind] <- FALSE
##    offcols <- colnames(Xstar)[offflag]
##    out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
##    out
## }


#' This function identifies the columns in a design matrix for a baseline-category link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, numeric, character or boolean. Works best if it is a factor. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#' 
#' @export
detect_sepcols_bcl<- function(y,X,rational=FALSE) {
     if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- bcl_Xstar(y,X,label=TRUE,rational=rational)
    ##here we check whether X has full column rank otherwise this check won't work properly.
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")      
    if(rational) Xstar <- rcdd::q2d(Xstar) 
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the apper
    A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
               - diag(ncol(Xstar)), # lower bounds -beta <= 1
                 diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
           rep(1,ncol(Xstar)),   #the right hand side is 1
           rep(1,ncol(Xstar))    #the right hand side is 1
           )
    if(rational){
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    ## making the H rep 
    hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   a <- as.numeric(a)
   if(rational) a <- rcdd::d2q(a)
   ## maximization with lpcdd
   lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
   if(rational) lso <- rcdd::q2d(lso) 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
   }
## detect_sepcols_pOLD<- function(y,X) {
##     # if(is.ordered(y) & length(unique(y))>2) { #TODO: Do we have any other way to check whether y is ordinal?
##     #    Xstar <- ordin_Xstar(y,X)
##     #} else {
##     Xstar <- nomin_Xstar(y,X) #for all ordinal, nominal and binary
##                                         #}
##     #intind <- grep("(Intercept)",colnames(Xstar))
##     ##TODO: can this supplant the ordinal and binary check as well? I think so. 
##     ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
##     ## left hand side just inequalities to folow the linear program in the apper
##     A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
##                - diag(ncol(Xstar)), # lower bounds -beta <= 1
##                  diag(ncol(Xstar))) # upper bounds beta <= 1
##     ## the right hand side are scalars 
##     b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
##            rep(1,ncol(Xstar)),   #the right hand side is 1
##            rep(1,ncol(Xstar))    #the right hand side is 1
##            )
##     ## making the H rep 
##     hrep<-rcdd::makeH(a1=A1,b1=b1)
##     ##turn into rational arithmetic
##     ##hrepr <- d2q(hrep) ## no longer used
##     ## objective function
##    a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
##    a <- as.numeric(a)
##    #rational arithmetic
##    ##ar <- d2q(a) ##skipped
##    ## maximization with lpcdd
##    lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
##    ## if 1 or -1 it is separation column, else not 
##    #offflag <- sapply(ls,function(x) isTRUE(all.equal(x,1)) || isTRUE(all.equal(x,-1))) # we need to test with tolerance
##    offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0)))
##    ##offflag[intind] <- FALSE
##    offcols <- colnames(Xstar)[offflag]
##    out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
##    return(out)
## }

#' @rdname detect_sepcols_bcl
#' @export
sepcols_bcl<- detect_sepcols_bcl


#' This function identifies the columns in a design matrix for a cumulative link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is an ordered factor. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#' @export
detect_sepcols_cl<- function(y,X,rational=FALSE) {
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- cl_Xstar(y,X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
    #ncat <- length(unique(y)) # we need this as in the ordin_Xstar all thresholds are always inf (because redundancy)
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the paper
    if(rational) Xstar <- rcdd::q2d(Xstar) # we need this to do the calculations; quicker than setting up struc_vec
    A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
               - diag(ncol(Xstar)), # lower bounds -beta <= 1
                 diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
           rep(1,ncol(Xstar)),   #the right hand side is 1
           rep(1,ncol(Xstar))    #the right hand side is 1
           )
    if(rational){
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    ## making the H rep 
    hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   a <- as.numeric(a)
   if(rational) a <- rcdd::d2q(a)
   ## maximization with lpcdd
   lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
   if(rational) lso <- rcdd::q2d(lso)
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
}

#' @rdname detect_sepcols_cl
#' @export
sepcols_cl<- detect_sepcols_cl

## detect_sepcols_oOLD<- function(y,X) {
##    # if(is.ordered(y) & length(unique(y))>2) { #TODO: Do we have any other way to check whether y is ordinal?
##     Xstar <- ordin_Xstar(y,X,label=TRUE)
##     ncat <- length(unique(y)) # we need this as in the ordin_Xstar all thresholds are always inf (because redundancy)
##     ##TODO: can this supplant the ordinal and binary check as well? I think so. 
##     ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
##     ## left hand side just inequalities to folow the linear program in the apper
##     A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
##                - diag(ncol(Xstar)), # lower bounds -beta <= 1
##                  diag(ncol(Xstar))) # upper bounds beta <= 1
##     ## the right hand side are scalars 
##     b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
##            rep(1,ncol(Xstar)),   #the right hand side is 1
##            rep(1,ncol(Xstar))    #the right hand side is 1
##            )
##     ## making the H rep 
##     hrep<-rcdd::makeH(a1=A1,b1=b1)
##     ##turn into rational arithmetic
##     ##hrepr <- d2q(hrep) ## no longer used
##     ## objective function
##    a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
##    a <- as.numeric(a)
##    #rational arithmetic
##    ##ar <- d2q(a) ##skipped
##    ## maximization with lpcdd
##    lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
##    ## If not equal to 0 it is separation column, else not 
##    offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
##    #offflag[seq(1:ncat)] <- FALSE #We drop the threshold columns
##    #offflag <- sapply(ls,function(x) isTRUE(all.equal(abs(x),1)))
##    offcols <- colnames(Xstar)[offflag]
##    out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
##    return(out)
## }


#' This function identifies the columns in a design matrix for an adjacent-category link model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @export
detect_sepcols_acl <- function(y,X,rational=FALSE)
    {
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- acl_Xstar(y,X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
    #ncat <- length(unique(y)) # we need this as in the ordin_Xstar all thresholds are always inf (because redundancy)
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the paper
    if(rational) Xstar <- rcdd::q2d(Xstar) # we need this to do the calculations; quicker than setting up struc_vec
    A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
               - diag(ncol(Xstar)), # lower bounds -beta <= 1
                 diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
           rep(1,ncol(Xstar)),   #the right hand side is 1
           rep(1,ncol(Xstar))    #the right hand side is 1
           )
    if(rational){
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    ## making the H rep 
    hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   a <- as.numeric(a)
   if(rational) a <- rcdd::d2q(a)
   ## maximization with lpcdd
   lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
   if(rational) lso <- rcdd::q2d(lso)
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
 }

#' @rdname detect_sepcols_acl
#' @export
sepcols_acl<- detect_sepcols_acl

#' This function identifies the columns in a design matrix for an ordered stereotype model that have an infinite MLE, due to separation. Note this is due to separation which includes the case of a design matrix that doesn't have full rank.  
#'
#' @details We solve a linear program in this function that operates only on y and X, so without a specific model. This program corresponds to detecting which columns in the design matrix leads to infinite MLE for some link functions, but it is more general as there are links that can still give finite estimates even though there is separation. Hence this function detects separation even in the case of (seemingly) finite estimates or if there is no warning. The prime example is using the log link in logistic regression. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @export
detect_sepcols_osm <- function(y,X,rational=FALSE)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- osm_Xstar(y,X,label=TRUE,rational=rational)
    if(ratcols) X <- rcdd::q2d(X)
    if(qr(X)$rank<dim(X)[2]) warning("X doesn't have full column rank. Results of this check are unreliable.")    
    #ncat <- length(unique(y)) # we need this as in the ordin_Xstar all thresholds are always inf (because redundancy)
    ## matrix of constraints for \code{lpcdd} must be of the form A1 * \beta \leq b1. We combine the constraints into one big A1 for the left hand side and a vector b1 of the right hand side scalars.
    ## left hand side just inequalities to folow the linear program in the paper
    if(rational) Xstar <- rcdd::q2d(Xstar) # we need this to do the calculations; quicker than setting up struc_vec
    A1<- rbind(-Xstar,              # the first ineqs -Xstar*beta <=0           
               - diag(ncol(Xstar)), # lower bounds -beta <= 1
                 diag(ncol(Xstar))) # upper bounds beta <= 1
    ## the right hand side are scalars 
    b1<- c(rep(0,dim(Xstar)[1]), #the right hand side is 0
           rep(1,ncol(Xstar)),   #the right hand side is 1
           rep(1,ncol(Xstar))    #the right hand side is 1
           )
    if(rational){
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    ## making the H rep 
    hrep<-rcdd::makeH(a1=A1,b1=b1)
    ## objective function
   a<-t(rep(1,dim(Xstar)[1]))%*%Xstar
   a <- as.numeric(a)
   if(rational) a <- rcdd::d2q(a)
   ## maximization with lpcdd
   lso <- rcdd::lpcdd(hrep,a,minimize=FALSE)$primal.solution
   if(rational) lso <- rcdd::q2d(lso)
   ## If not equal to 0 it is separation column, else not 
   offflag <- sapply(lso,function(x) !isTRUE(all.equal(x,0))) # we need to test with tolerance
   offcols <- colnames(Xstar)[offflag]
   out <- list(ls=lso,offcols=offcols,colnrs=which(offflag),separated=offflag)
   return(out)
   }

#' @rdname detect_sepcols_osm
#' @export
sepcols_osm <- detect_sepcols_osm
