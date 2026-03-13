#' This function calculates the linearities in the negative structure vector matrix X*, so the row vectors for which there is no separation. 
#' If this is an empty set or of length 0, then we have overlap. 
#'
#' The function uses either a response vector y and a design matrix X, or a structure vector matrix S. If S is given, y and X and model are ignored. 
#' 
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param S a matrix of structure vectors
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#' @return a list with elements $lins which lists the row vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.
#'
#' @importFrom rcdd linearity q2d d2q 
#' @export
linearities <- function(y, X, S, rational=FALSE, model=c("b","bcl","acl","cl","sl","osm")){
    if(missing(S))
    {
    if(length(unique(y))<2) stop("There is only one value in y.")
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE
    if(missing(model)) model <- NULL
    if(is.null(model))
    {
        warning("I'm not sure which model you want to fit, so I default to the most common ones.","\n")
        if(is.ordered(y) & length(unique(y))>2)
        {
            linearities_cl(y=y,X=X,rational=rational)
        } else {
            linearities_bcl(y=y,X=X,rational=rational)
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b = linearities_b(y=y,X=X,rational=rational),
           bcl = linearities_bcl(y=y,X=X,rational=rational),
           cl = linearities_cl(y=y,X=X,rational=rational),
           acl = linearities_acl(y=y,X=X,rational=rational),       
           sl = linearities_sl(y=y,X=X,rational=rational),
           osm = linearities_osm(y=y,X=X,rational=rational)
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
       vrep <- cbind(0, 0, Xstar)
       if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
       lout <- rcdd::linearity(vrep, rep = "V")
       if(rat_cols(Xstar))  {
          Xstar <- rcdd::q2d(Xstar)
       }
       out <- list(lins=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in lins for overlap? will be categories*dim(X)[1] of length 
      out
    }
}

#' This function calculates the linearities in the negative structure vector matrix X* for a baseline-category link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#'
#' @return a list with elements $lins which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.
#'
#' @export
linearities_bcl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- bcl_Xstar(y=y, X=X, label=TRUE, rational=rational) #for all nominal and binary
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(lins=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in lins for overlap? will be categories*dim(X)[1] of length 
   return(out)
}
                       
#' This function calculates the linearities in the negative structure vector matrix X* for a cumulative link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#'
#' @return a list with elements $lins which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_cl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- cl_Xstar(y=y, X=X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(lins=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in lins for overlap? will be categories*dim(X)[1] of length 
   return(out)
}

#' This function calculates the linearities in the negative structure vector matrix X* for an ordered stereotype model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#'
#' @return a list with elements $lins which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_osm<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- osm_Xstar(y=y, X=X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(lins=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in lins for overlap? will be categories*dim(X)[1] of length 
   return(out)
}


#' @rdname linearities_bcl 
#' @export
linearities_b <- linearities_bcl

#' This function calculates the linearities in the negative structure vector matrix X* for an adjacent-category link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#'
#' @return a list with elements $lins which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_acl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- acl_Xstar(y=y, X=X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(lins=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in lins for overlap? will be categories*dim(X)[1] of length 
   return(out)
}

#' This function calculates the linearities in the negative structure vector matrix X* for an sequential link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#' @param reduced If TRUE (default) the per category results are merged into one objct and duplicates removed. If FALSE, the result is given for each category separately.   
#'
#' @return a list with elements $lins which lists the rows vectors that are linearities for any category (reduced=TRUE) or by category (reduced=FALSE), and $index which gives the row index of the linearities for any or by category. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_sl<- function(y, X, rational=FALSE,reduced=TRUE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   if(!is.ordered(y)) stop("This function needs ordered outcomes.")
   splitdat <- create_bseq(y=y,X=X)
   linsplit <- lapply(splitdat,function(l) linearities_b(y=l$y,X=l$X,rational=rational))
   lout <- lapply(linsplit,function(x) x$index)
   lins <- lapply(linsplit,function(x) x$lins)
   if(reduced)
       {
         lout <- unique(Reduce(c,lout))
         lins <- unique(Reduce(rbind,lins))
   }
   out <- list(lins=lins, index=lout)
   return(out)
}
