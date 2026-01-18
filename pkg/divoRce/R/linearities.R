#' This function calculates the linearities in the data, so the row vectors for which there is no separation. 
#' If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.
#'
#' @importFrom rcdd linearity q2d d2q 
#' @export
linearities <- function(y, X, rational=FALSE)
{
        stop("I'm not done yet.")
    #TOTO make switch version
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
     if(is.ordered(y) & length(unique(y))>2) { 
        Xstar <- cl_Xstar(y, X, label=TRUE, rational=rational)
    } else {
        Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational) #for all nominal and binary
    }
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(linX=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in linX for overlap? will be categories*dim(X)[1] of length 
   return(out)
}

#' This function calculates the linearities in the data for a baseline-category link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.
#'
#' @export
linearities_bcl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational) #for all nominal and binary
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(linX=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in linX for overlap? will be categories*dim(X)[1] of length 
   return(out)
}
                       
#' This function calculates the linearities in the data for a cumulative link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_cl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- cl_Xstar(y, X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(linX=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in linX for overlap? will be categories*dim(X)[1] of length 
   return(out)
}

#' This function calculates the linearities in the data for an ordered stereotype model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_osm<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- osm_Xstar(y, X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(linX=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in linX for overlap? will be categories*dim(X)[1] of length 
   return(out)
}


#' @rdname linearities_bcl 
#' @export
linearities_b <- linearities_bcl

#' This function calculates the linearities in the data for an adjacent-category link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category and $index which gives the row index of the linearities. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_acl<- function(y, X, rational=FALSE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   Xstar <- acl_Xstar(y, X, label=TRUE, rational=rational)
   vrep <- cbind(0, 0, Xstar)
   if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep) 
   lout <- rcdd::linearity(vrep, rep = "V")
   if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
   out <- list(linX=Xstar[lout,,drop=FALSE], index=lout)
   #TODO: Maybe return nothing in linX for overlap? will be categories*dim(X)[1] of length 
   return(out)
}

#' This function calculates the linearities in the data for an sequential link model, so the row vectors for which there is no separation. If this is an empty set or of length 0, then we have overlap. 
#'
#' @param y the outcome variable. Can be factor, ordered, numeric, character or boolean. Works best if it is a factor or ordered factor. If it is not an (ordered) factor, we treat the outcome as nominal.    
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE
#' @param reduced If TRUE (default) the per category results are merged into one objct and duplicates removed. If FALSE, the result is given for each category separately.   
#'
#' @return a list with elements $linX which lists the rows vectors that are linearities for any category (reduced=TRUE) or by category (reduced=FALSE), and $index which gives the row index of the linearities for any or by category. If there is complete separation they are both empty set. If their length/row dimension is number of categories*dim(X)[1], there is overlap. Anything in between is quasi-complete separation.  
#' @export
linearities_sl<- function(y, X, rational=FALSE,reduced=TRUE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   if(!is.ordered(y)) stop("This function needs ordered outcomes.")
   splitdat <- create_bseq(y,X)
   linsplit <- lapply(splitdat,function(l) linearities_b(l$y,l$X))
   lout <- lapply(linsplit,function(x) x$index)
   linX <- lapply(linsplit,function(x) x$linX)
   if(reduced)
       {
         lout <- unique(Reduce(c,lout))
         linX <- unique(Reduce(rbind,linX))
   }
   out <- list(linX=linX, index=lout)
   return(out)
}
