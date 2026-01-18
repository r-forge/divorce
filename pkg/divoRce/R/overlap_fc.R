#' A fraction check for overlap.
#'
#' Intended for data where one suspects overlap, this function checks for overlap on an ever growing subset of the data. It is a low-level function for a response vector y and a design matrix X. 
#'
#' The function samples a fraction of observations form the data and checks for overlap. If overlap exists in a subset, then overlap exists in the overal data (Corollary in Sablica et al,. (2025)). If no overlap is found, it takes a larger sample and checks again until all data are tested. If no overlap is found even for all data, it concludes there is separation.
#'
#' Since solving the exact linear program on the full data via \link{check_ovl} can take a long time for large data, this check can be quicker in case of overlap (especially if the overlapping categories are not rare). However, if there is separation this function usually takes longer. 
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param frac the fraction of the data to use for checking (uses n/frac data). Defaults to 10. If frac is below 1 or n, it uses frac=1. Using frac=1 is the same as using check_ovl.  
#' @param verbose should progress be reported. Defaults ot 'FALSE'.
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else. 
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#'
#' @export
overlap_fc <- function(y, X, frac=10L, verbose=FALSE, rational=FALSE, model=c("b","bcl","cl","acl","sl","osm"))
{
 n <- length(y)
 #n.cat <- length(unique(y))
 if(frac>n || frac < 1) frac <- 1
 i <- 1
 repeat{
 nco <- nc <- i*floor(n/frac)
 if(nc > n) nc <-n
 if (verbose>0) cat("Checking ",nc,"observations.","\n")
 ind <- sample(1:n,nc,replace=FALSE)
 ys <- y[ind]
 Xs <- X[ind,]
 olcheck <- FALSE
 if(isTRUE(all.equal(length(unique(ys)),length(unique(y))))) #we skip evaluation if not all categories are in the subsample
 {
     olcheck <- check_ovl(ys,Xs,rational=rational)
     #olcheck <- overlap_qc(ys,Xs,rational=rational)
 }
 if(isTRUE(olcheck)) break()
 if(nco >= n) break() 
 i <- i+1
 }
 return(olcheck)   
}

#' A quick check for overlap.
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#' 
#'
#' @export
overlap_qc <- function(y, X, rational=FALSE,model=c("b","bcl","cl","acl","sl","osm"))
{
if(missing(model)) model <- NULL
return(!any(detect_sepcols(y,X,rational=rational,model=model)$separated))
}


#' A quick check for separation.
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' 
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#'
#' @export
separation_qc <- function(y, X, rational=FALSE,model=c("b","bcl","cl","acl","sl","osm"))
{
if(missing(model)) model <- NULL
return(any(detect_sepcols(y,X,rational=rational,model=model)$separated))
}
