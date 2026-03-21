#' Calculates recession cone for categorical data models.
#'
#' This function returns the structure vectors that are not linearities (determining the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (making up the recession cone).
#'
#' The function uses either a response vector y and a design matrix X, or a structure vector matrix S. If S is given, y and X and model are ignored. 
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param S a matrix of structure vectors
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone <- function(y, X, S, rational=FALSE, model=c("b","bcl","acl","cl","sl","osm")){
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
      if(is.ordered(y) & length(unique(y))>2) { 
        return(reccone_cl(y=y,X=X,rational=rational))
      } else {
        return(reccone_bcl(y=y,X=X,rational=rational)) #for all nominal and binary
      }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b = reccone_b(y=y,X=X,rational=rational),
           bcl = reccone_bcl(y=y,X=X,rational=rational),
           cl = reccone_cl(y=y,X=X,rational=rational),
           acl = reccone_acl(y=y,X=X,rational=rational),       
           sl = reccone_sl(y=y,X=X,rational=rational),
           osm = reccone_osm(y=y,X=X,rational=rational)
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
        lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
        if(rat_cols(Xstar))  {
           Xstar <- rcdd::q2d(Xstar)
         }
        reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
        ind <- seq(1,dim(Xstar)[1],by=1)
        if (length(lout)==0){
          cone <- Xstar[,,drop=FALSE]
          ind <- ind
        } else {
          ind <- ind[-lout]
          cone <- Xstar[ind,,drop=FALSE]
        }
     out <- list(cone=-cone, reccdim=reccdim, index=ind)
    #TODO: So we return rows the negative of the rows in Xstar that are not linearities and the row index. If we have complete separation, then lout is integer(0) and we return all X (as all are not linearities) and the full index vector. In overlap we return an empty X and index. Maye we should return nothing for overlap?
     if(reccdim>length(ind)) warning("The dimension of the recession cone is artificially higher due to X not having full rank.")
     return(out)
    }
}

#' Calculates recession cone for baseline-category link models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (making up the recession cone). reccone_b can also be used if one has a binary outcome.
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone_bcl<- function(y,X,rational=FALSE)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- bcl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
    vrep <- cbind(0, 0, Xstar)
    if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
    lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
    if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
    reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
    ind <- seq(1,dim(Xstar)[1],by=1)
    if (length(lout)==0){
        cone <- Xstar[,,drop=FALSE]
        ind <- ind
    } else {
        ind <- ind[-lout]
        cone <- Xstar[ind,,drop=FALSE]
    }
    out <- list(cone=-cone, reccdim=reccdim, index=ind)
    if(reccdim>length(ind)) warning("The dimension of the recession cone is artificially higher due to X not having full rank.")
    return(out)
}

#'@rdname reccone_bcl
#'@export
reccone_b <- reccone_bcl

#' Calculates recession cone for cumulative link models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (mnaking up the recession cone).
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone_cl<- function(y,X,rational=FALSE)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- cl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
    vrep <- cbind(0, 0, Xstar)
    if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
    lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
    if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
    reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
    ind <- seq(1,dim(Xstar)[1],by=1)
    if (length(lout)==0){
        cone <- Xstar[,,drop=FALSE]
        ind <- ind
    } else {
        ind <- ind[-lout]
        cone <- Xstar[ind,,drop=FALSE]
    }
    out <- list(cone=-cone, reccdim=reccdim, index=ind)
    if(reccdim>length(ind)) warning("The dimension of the recession cone is artificially higher due to X not having full rank.")
    return(out)
}

#' Calculates recession cone for adjacent-category link models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (mnaking up the recession cone).
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone_acl<- function(y,X,rational=FALSE)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- acl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
    vrep <- cbind(0, 0, Xstar)
    if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
    lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
    if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
    reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
    ind <- seq(1,dim(Xstar)[1],by=1)
    if (length(lout)==0){
        cone <- Xstar[,,drop=FALSE]
        ind <- ind
    } else {
        ind <- ind[-lout]
        cone <- Xstar[ind,,drop=FALSE]
    }
    out <- list(cone=-cone, reccdim=reccdim, index=ind)
    if(reccdim>length(ind)) warning("The dimension of the recession cone is artificially higher due to X not having full rank.")
    return(out)
}

#' Calculates recession cone for ordered stereotype models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (mnaking up the recession cone).
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone_osm<- function(y,X,rational=FALSE)
{
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    Xstar <- osm_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
    vrep <- cbind(0, 0, Xstar)
    if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
    lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
    if(rat_cols(Xstar))  {
       Xstar <- rcdd::q2d(Xstar)
       }
    reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
    ind <- seq(1,dim(Xstar)[1],by=1)
    if (length(lout)==0){
        cone <- Xstar[,,drop=FALSE]
        ind <- ind
    } else {
        ind <- ind[-lout]
        cone <- Xstar[ind,,drop=FALSE]
    }
    out <- list(cone=-cone, reccdim=reccdim, index=ind)
    if(reccdim>length(ind)) warning("The dimension of the recession cone is artificially higher due to X not having full rank.")
    return(out)
}

#' Calculates recession cone for sequential link models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (mnaking up the recession cone).
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param reduced if TRUE the result is pooled over all the categories and FALSE it is per category. 
#' 
#' @return a list (or list of lists if 'reduced=FALSE') with 'cone' being the recession cone over all categories ('reduced=TRUE') or per category ('reduced=FALSE'), 'reccdim' being the dimension of the recession cone for each category (reduced=FALSE) or the dimension of the largest recession cone of any category ('reduced=TRUE'), and 'index' the row index of the structure vectors that are not linearities for each category ('reduced=FALSE') or over all categories ('reduced=TRUE'). Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone_sl<- function(y, X, rational=FALSE,reduced=TRUE)
{
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   #if(!is.ordered(y)) stop("This function needs ordered outcomes.")
   splitdat <- create_bseq(y=y,X=X)
   reccsplit <- lapply(splitdat,function(l) reccone_b(y=l$y,X=l$X,rational=rational))
   ind <- lapply(reccsplit,function(x) x$index)
   reccdim <- lapply(reccsplit,function(x) x$reccdim)
   cone <- lapply(reccsplit,function(x) -x$cone)
   if(reduced)
       {
         ind <- unique(Reduce(c,ind))
         cone <- unique(Reduce(rbind,cone))
         reccdim <- Reduce(max,reccdim) 
   }
   out <- list(cone=cone, reccdim=reccdim, index=ind)
   return(out)
}
