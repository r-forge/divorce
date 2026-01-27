#' Calculates recession cone for categorical data models.
#'
#' This function returns the structure vectors that are not linearities (comprising the recession cone), the recession cone dimension and the row index of the structure vectors that are not linearities (mnaking up the recession cone).
#'
#' 
#' @param y the outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational boolean flag whether rational arithmetic should be used. Default is FALSE.
#' @param model which model is the check for? One of bcl (baseline-category link), acl (adjacent-category link), b (binary), osm (ordered stereotype model), sl (sequential aka continuation-ratio link).
#' 
#' @return a list with 'cone' being the recession cone, 'reccdim' being the dimensions of the recession cone, and 'index' the row index of the structure vectors that are not linearities. Note that in case of 'X' not having full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent.   
#' 
#' @export
reccone <- function(y,X,rational=FALSE,model=c("b","bcl","acl","sl","osm"))
{
    stop("I'm not done yet.")
    # TODO make switch version
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(is.ordered(y) & length(unique(y))>2) { 
        Xstar <- cl_Xstar(y,X,label=TRUE,rational=rational)
        #offcols <- detect_sepcols_o(y,X)$offcols
    } else {
        Xstar <- bcl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
        #offcols <- detect_sepcols_p(y,X)$offcols
    }
    vrep <- cbind(0, 0, Xstar)
    if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
    lout <- rcdd::linearity(vrep, rep = "V") #always returns numeric
    if(rat_cols(Xstar))  {
       #lout <- rcdd::q2d(lout)
       Xstar <- rcdd::q2d(Xstar)
       }
    reccdim <- dim(Xstar)[2]-qr(Xstar[lout,])$rank
    ind <- seq(1,dim(Xstar)[1],by=1)
    if (length(lout)==0){
        #cone <-  X
        cone <- Xstar[,,drop=FALSE]
        #attr(cone,"assign") <- NULL
        ind <- ind
    } else {
        ind <- ind[-lout]
        cone <- Xstar[ind,,drop=FALSE]
        #cone <- Xstar[-lout,offcols,drop=FALSE]
    }
    out <- list(cone=-cone, reccdim=reccdim, index=ind)
    #TODO: So we return rows the negative of the rows in Xstar that are not linearities and the row index. If we have complete separation, then lout is integer(0) and we return all X (as all are not linearities) and the full index vector. In overlap we return an empty X and index. Maye we should return nothing for overlap?
    if(reccdim>length(ind)) warning("The dimension of the recession cone is higher due to X not having full rank.")
    return(out)
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
    Xstar <- bcl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
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
    if(reccdim>length(ind)) warning("The dimension of the recession cone is higher due to X not having full rank.")
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
    Xstar <- cl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
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
    if(reccdim>length(ind)) warning("The dimension of the recession cone is higher due to X not having full rank.")
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
    Xstar <- acl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
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
    if(reccdim>length(ind)) warning("The dimension of the recession cone is higher due to X not having full rank.")
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
    Xstar <- osm_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
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
    if(reccdim>length(ind)) warning("The dimension of the recession cone is higher due to X not having full rank.")
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
   if(!is.ordered(y)) stop("This function needs ordered outcomes.")
   splitdat <- create_bseq(y,X)
   reccsplit <- lapply(splitdat,function(l) reccone_b(l$y,l$X))
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
