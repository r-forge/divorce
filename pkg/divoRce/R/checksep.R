#' General separation check.  
#'
#' This function checks for (quasi-) complete separation by calling the appropriate low-level functions.
#'
#' The function uses either a response vector y and a design matrix X, or a structure vector matrix S. If S is given, y and X and model are ignored. 
#'
#' 
#'
#' @param y a categorical outcome vector 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals). 
#' @param S a matrix of structure vectors
#' @param rational should rational arithmetic be used
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @export
checksep <- function(y, X, S, rational=FALSE, model=c("bcl", "b","cl","acl","sl","osm")){
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
            return(checksep_cl(y=y,X=X,rational=rational))
        } else {
            return(checksep_bcl(y=y,X=X,rational=rational))
            
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b= checksep_b(y=y,X=X,rational=rational),
           bcl= checksep_bcl(y=y,X=X,rational=rational),
           cl= checksep_cl(y=y,X=X,rational=rational),
           acl= checksep_acl(y=y,X=X,rational=rational),       
           sl=checksep_sl(y=y,X=X,rational=rational),
           osm=checksep_osm(y=y,X=X,rational=rational)
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
        a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
        if(rational) a1 <- rcdd::d2q(a1)
        b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
        if(rational) b1 <- rcdd::d2q(b1)
        a2 <- cbind(t(Xstar),0)
        if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check also if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
        b2 <- rep(0,ncol(Xstar))
        if(rational) b2 <- rcdd::d2q(b2)
        objgrd <- c(rep(0,nrow(Xstar)),1)
        if(rational) objgrd <- rcdd::d2q(objgrd)
        cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
        if(rational) cal <- rcdd::q2d(cal)
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
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' 
#' @export
checksep_cl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- cl_Xstar(y=y, X=X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use model = 'b' or checksep_b.")    
   ## we distiguish the following cases.
   ## Xstar numeric, rational FALSE: all is done with numeric
   ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
   ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
   ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
   a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
   if(rational) a1 <- rcdd::d2q(a1)
   b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
   if(rational) b1 <- rcdd::d2q(b1)
   a2 <- cbind(t(Xstar),0)
   if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
   b2 <- rep(0,ncol(Xstar))
   if(rational) b2 <- rcdd::d2q(b2)
   objgrd <- c(rep(0,nrow(Xstar)),1)
   if(rational) objgrd <- rcdd::d2q(objgrd)
   cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
   if(rational) cal <- rcdd::q2d(cal)
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
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd 
#' @export
checksep_osm<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- osm_Xstar(y=y, X=X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use model= 'b' or checksep_b.")    
   ## we distiguish the following cases.
   ## Xstar numeric, rational FALSE: all is done with numeric
   ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
   ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
   ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
   a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
   if(rational) a1 <- rcdd::d2q(a1)
   b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
   if(rational) b1 <- rcdd::d2q(b1)
   a2 <- cbind(t(Xstar),0)
   if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
   b2 <- rep(0,ncol(Xstar))
   if(rational) b2 <- rcdd::d2q(b2)
   objgrd <- c(rep(0,nrow(Xstar)),1)
   if(rational) objgrd <- rcdd::d2q(objgrd)
   cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
   if(rational) cal <- rcdd::q2d(cal)
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
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#' @import rcdd
#' @export
checksep_bcl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(is.ordered(y) & length(unique(y))>2) stop("For ordered y, please specify the model via the model argument or the appropriate checksep function.") 
        Xstar <- bcl_Xstar(y=y, X=X, label=FALSE, rational = rational) #for all nominal and binary
   ## we distiguish the following cases.
   ## Xstar numeric, rational FALSE: all is done with numeric
   ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
   ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
   ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
   a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
   if(rational) a1 <- rcdd::d2q(a1)
   b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
   if(rational) b1 <- rcdd::d2q(b1)
   a2 <- cbind(t(Xstar),0)
   if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
   b2 <- rep(0,ncol(Xstar))
   if(rational) b2 <- rcdd::d2q(b2)
   objgrd <- c(rep(0,nrow(Xstar)),1)
   if(rational) objgrd <- rcdd::d2q(objgrd)
   cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
   if(rational) cal <- rcdd::q2d(cal)
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
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' @export
checksep_b<- checksep_bcl


#' Separation check for sequential (continuation-ratio) models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' @export
checksep_sl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   splitdat <- create_bseq(y=y,X=X)
   seqout <- sapply(splitdat,function(l) checksep_b(y=l$y,X=l$X,rational=rational))
   any(seqout)
}

#' Separation check for adjacent-category link models.
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' @export
checksep_acl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- acl_Xstar(y=y, X=X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use model= 'b' or checksep_b.")    
   ## we distiguish the following cases.
   ## Xstar numeric, rational FALSE: all is done with numeric
   ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
   ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
   ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
   a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
   if(rational) a1 <- rcdd::d2q(a1)
   b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
   if(rational) b1 <- rcdd::d2q(b1)
   a2 <- cbind(t(Xstar),0)
   if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
   b2 <- rep(0,ncol(Xstar))
   if(rational) b2 <- rcdd::d2q(b2)
   objgrd <- c(rep(0,nrow(Xstar)),1)
   if(rational) objgrd <- rcdd::d2q(objgrd)
   cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
   if(rational) cal <- rcdd::q2d(cal)
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
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @import rcdd
#' @export
checksep_osm<- function(y, X, rational=FALSE){
    ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- osm_Xstar(y=y, X=X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use model= 'b' or checksep_b.")    
   ## we distiguish the following cases.
   ## Xstar numeric, rational FALSE: all is done with numeric
   ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
   ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
   ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
   a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
   if(rational) a1 <- rcdd::d2q(a1)
   b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
   if(rational) b1 <- rcdd::d2q(b1)
   a2 <- cbind(t(Xstar),0)
   if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
   b2 <- rep(0,ncol(Xstar))
   if(rational) b2 <- rcdd::d2q(b2)
   objgrd <- c(rep(0,nrow(Xstar)),1)
   if(rational) objgrd <- rcdd::d2q(objgrd)
   cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
                     b1 = b1,
                     a2 = a2,
                     b2 = b2),
                     objgrd=objgrd,
                     minimize = TRUE)$optimal.value
   if(rational) cal <- rcdd::q2d(cal)
   out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
   return(out)
}



## checksepOld <- function(y, X, rational=FALSE){
## ratcols <- rat_cols(X)
##    if(ratcols) rational <- TRUE 
##    if(is.ordered(y) & length(unique(y))>2) { 
##         Xstar <- cl_Xstar(y, X, label=FALSE, rational = rational)
##     } else {
##         Xstar <- bcl_Xstar(y, X, label=FALSE, rational = rational) #for all nominal and binary
##     }
##    ## we distiguish the following cases.
##    ## Xstar numeric, rational FALSE: all is done with numeric
##    ## Xstar numeric, rational TRUE: Matrices are built numeric and d2q is called for rational before linear program
##    ## Xstar rational, rational TRUE: Matrices are built numeric and d2q is called on all but on a1 which is already rational.
##    ## Xstar rational, rational FALSE: Treated as if rational were TRUE (makes no sense to me to supply rational and then wanting the calculations numeric)
##    a1 <- rbind(cbind(-diag(nrow(Xstar)),-1), c(rep(0,nrow(Xstar)),-1))
##    if(rational) a1 <- rcdd::d2q(a1)
##    b1 <- c(rep(-1 ,each=nrow(Xstar)),0)
##    if(rational) b1 <- rcdd::d2q(b1)
##    a2 <- cbind(t(Xstar),0)
##    if(rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2) #here we need to check alos if Xstar is rational, because t(Xstar) uses Xstar as character if it is.  
##    b2 <- rep(0,ncol(Xstar))
##    if(rational) b2 <- rcdd::d2q(b2)
##    objgrd <- c(rep(0,nrow(Xstar)),1)
##    if(rational) objgrd <- rcdd::d2q(objgrd)
##    cal <- rcdd::lpcdd(rcdd::makeH(a1 = a1,
##                      b1 = b1,
##                      a2 = a2,
##                      b2 = b2),
##                      objgrd=objgrd,
##                      minimize = TRUE)$optimal.value
##    if(rational) cal <- rcdd::q2d(cal)
##    out <- ifelse(isTRUE(all.equal(cal,1)),TRUE,FALSE)
##    return(out)
    
#' General overlap check.
#'
#' This function checks for overlap by calling the appropriate low-level functions.
#'
#' The function uses either a response vector y and a design matrix X or a structure vector matrix S. If S is given, y and X and model are ignored.
#'
#' @param y outcome vector 
#' @param X design matrix
#' @param S structure vector matrix
#' @param rational should rational arithmetic be used.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#' @return a Boolean; either 'TRUE' if there is overlap or 'FALSE' if not.
#'
#' @export
checkovl <- function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","osm")){
  if(missing(model)) model <- NULL
  if(missing(S)) !isTRUE(checksep(y=y, X=X, rational=rational, model = model)) else !isTRUE(checksep(S=S, rational=rational)) 
}



