#' General separation check. This calls to the appropriate low-level function. 
#'
#' This function checks for (quasi-) complete separation. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#'
#' @return a Boolean; either 'TRUE' if we detected separation or 'FALSE' if not.
#'
#' @export
check_sep <- function(y, X, rational=FALSE, model=c("bcl","cl","acl","sl","osm")){
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(length(unique(y))<2) stop("There is only one value in y.") 
    if(missing(model)) model <- NULL
    if(is.null(model))
    {
        warning("I'm not sure which model you want to fit, so I default to the most common ones.","\n")
        if(is.ordered(y) & length(unique(y))>2)
        {
            check_sep_cl(y,X,rational=rational)
        } else {
            check_sep_bcl(y,X,rational=rational)
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           bcl= check_sep_bcl(y,X,rational=rational),
           cl= check_sep_cl(y,X,rational=rational),
           acl= check_sep_acl(y,X,rational=rational),       
           sl=check_sep_sl(y,X,rational=rational),
           osm=check_sep_osm(y,X,rational=rational)
           )
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
check_sep_cl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- cl_Xstar(y, X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use check_sep_b.")    
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
check_sep_osm<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- osm_Xstar(y, X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use check_sep_b.")    
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
check_sep_bcl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(is.ordered(y) & length(unique(y))>2) stop("This function is for unordered y. For ordered y, please use the appropriate check_sep function.") 
        Xstar <- bcl_Xstar(y, X, label=FALSE, rational = rational) #for all nominal and binary
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
check_sep_b<- check_sep_bcl


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
check_sep_sl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   y <- as.ordered(y)
   splitdat <- create_bseq(y,X)
   seqout <- sapply(splitdat,function(l) check_sep_b(l$y,l$X,rational=rational))
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
check_sep_acl<- function(y, X, rational=FALSE){
   ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- acl_Xstar(y, X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use check_sep_b.")    
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
check_sep_osm<- function(y, X, rational=FALSE){
    ratcols <- rat_cols(X)
   if(ratcols) rational <- TRUE 
   if(length(unique(y))>2) { 
       Xstar <- osm_Xstar(y, X, label=FALSE, rational = rational)
   } else stop("For 2 categories, please use check_sep_b.")    
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



## check_sepOld <- function(y, X, rational=FALSE){
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
#' This function checks for overlap. It is a low-level function for a response vector y and a design matrix X.  
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#' @return a Boolean; either 'TRUE' if there is overlap or 'FALSE' if not.
#'
#' @export
check_ovl <- function(y, X, rational=FALSE, model=c("bcl","cl","acl","sl","osm")){
  if(missing(model)) model <- NULL
  !isTRUE(check_sep(y, X, rational=rational, model = model))
}



