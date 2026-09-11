#' A sequential check for overlap.
#'
#' Intended for data where one suspects overlap, this function checks for overlap on sequential subset of the data. It is a mid-level function for a response vector y and a design matrix X. 
#'
#' The function sequentially takes a subset of observations from the data and checks for overlap in it. If overlap exists in a subset that spans the whole space, then overlap exists in the overal data (Sablica et al,. 2026). If no overlap is found in any full-rank subset, it runs the linear program on the full data set to decide whether there is separation. The rank condition is important because one can have overlap/separation in a non-full-rank marix that does not hold up in the full rank case.  
#'
#' Since solving the exact linear program on the full data via \link{check_overlap} can take a long time for large data, this check can be quicker in case of overlap (especially if the overlapping categories are not rare). However, if there is separation or many subsets have non-full rank this function takes longer as we need to call run the progra on the full data anyway. This can be made less likely by using shuffle = TRUE. 
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param S structure vector matrix 
#' @param nss number of subsets to use for sequential checking. Defaults to 10. If nss is below 1 or above n-1, it uses 'nss = 1'. Using 'nss = 1' is the same as checking the full data..
#' @param verbose should progress be reported. Defaults to 'FALSE'.
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param quick Use the columnwise ('TRUE') or the full linear program ('FALSE'). 
#' @param backend which backend to use for the linear program. Can be 'rcdd' (default and only option for rational=TRUE) or 'ROI'.
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param shuffle should the data be shuffled before sequential checking (defaults to 'TRUE'). This can help if the data are ordered in such a way that subsets do not span the full space.  
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#'
#' @export
check_overlap_sequential <- function(y, X, S, nss = 10L, verbose = FALSE, rational = FALSE, model = c("b","bcl","cl","acl","sl","os"), quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, shuffle = TRUE) {
  backend <- .divorce_match_backend(backend)   
  if(missing(S)) {
    if(missing(model)) model <- NULL
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("Length of y and number of rows of X do not match.")   
    n <- length(y)
    if(isTRUE(shuffle)) {
        neword <- sample(seq(1,n))
        y <- y[neword]
        X <- X[neword,]
    }
    frank <- qr(X)$rank
    if(nss > n || nss < 1) nss <- 1
    breaks <- seq(1,n,length.out=nss+1)
    olcheck <- rep(NA,length(breaks)-1)
    for (i in 2:length(breaks)) {
      strt <- breaks[i-1]
      end <- breaks[i]
      ind <- seq(ceiling(strt), floor(end), by = 1)
      if (verbose>0) cat("Checking subset", i-1,"\n") 
      ys <- y[ind]
      Xs <- X[ind,]
      ## Implementation: We check each subset for full rank. If it doesn't have full rank we skip evaluation and say inconclusive. If it has full rank, we evaluate with the linear program.  
      ## Possible Cases:
      ## 1.) If we have overlap in any full rank subset we have overlap overall and stop.
      ## 2.) If we don't have overlap in any full rank subset it is inconclusive and we must run the lin prog on the full data.
      if(isTRUE(all.equal(length(unique(ys)),length(unique(y))))) { #we skip evaluation if not all categories are in the subsample
          if(isTRUE(all.equal(qr(Xs)$rank,frank))) {
             olcheck[i-1] <- check_overlap_worker(y=ys, X=Xs, rational=rational, model=model, quick=quick, backend=backend, solver = solver) # we only check if subset has full-rank
          } 
      }
      if(isTRUE(olcheck[i-1])) return(olcheck[i-1]) #full rank subset has overlap 
    }
    if(all(olcheck!=TRUE)) olout <- check_overlap_worker(y=y, X=X, rational=rational, model=model, quick=quick, backend=backend, solver = solver) #all inconclusive 
  } else {
     n <- dim(S)[1]
    if(isTRUE(shuffle)) {
        neword <- sample(seq(1,n))
        S <- S[neword,]
    }
    frank <- qr(S)$rank
    if(nss > n || nss < 1) nss <- 1
    breaks <- seq(1,n,length.out=nss+1)
    olcheck <- rep(NA,length(breaks)-1)
    for (i in 2:length(breaks)) {
      strt <- breaks[i-1]
      end <- breaks[i]
      ind <- seq(ceiling(strt), floor(end), by = 1)
      if (verbose>0) cat("Checking subset", i-1,"\n")
      Ss <- S[ind,]
      if(isTRUE(all.equal(qr(Ss)$rank,frank))) {
          olcheck[i-1] <- check_overlap_worker(S=Ss, rational=rational, model=model, quick=quick, backend=backend, solver = solver) 
          } 
      if(isTRUE(olcheck[i-1])) return(olcheck[i-1])
    }
    if(all(olcheck!=TRUE)) olout <- check_overlap_worker(S=S, rational=rational, model=model, quick=quick, backend=backend, solver = solver) 
  }
  olout
}
     
#' @rdname check_overlap_sequential
#' @export
overlap_sequential_check <- overlap_fraction_check <- check_overlap_sequential


## overlap_sequential_checkOLD <- function(y, X, S, nss = 10L, quick = FALSE, verbose=FALSE, rational=FALSE, model=c("b","bcl","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL)
## {
##  backend <- .divorce_match_backend(backend)   
##  if(missing(S)) {
##  if(missing(model)) model <- NULL    
##  n <- length(y)
##  if(nss > n || nss < 1) nss <- 1
##  i <- 1
##  repeat{ 
##  nco <- nc <- i*floor(n/frac)
##  if(nc > n) nc <-n
##  if (verbose>0) cat("Checking ",nc,"rows.","\n")
##  ind <- sample(1:n,nc,replace=FALSE)
##  ys <- y[ind]
##  Xs <- X[ind,]
##  olcheck <- FALSE
##  if(isTRUE(all.equal(length(unique(ys)),length(unique(y))))) #we skip evaluation if not all categories are in the subsample
##  {
##      olcheck <- check_overlap(y=ys, X=Xs, quick = quick, rational=rational, model=model, backend=backend, solver = solver)
##  }
##  if(isTRUE(olcheck)) break()
##  if(nco >= n) break() 
##  i <- i+1
##  }
##  return(olcheck)   
##  } else {
##  n <- dim(S)[1]
##  #n.cat <- length(unique(y))
##  if(frac>n || frac < 1) frac <- 1
##  i <- 1
##  repeat{
##  nco <- nc <- i*floor(n/frac)
##  if(nc > n) nc <-n
##  if (verbose>0) cat("Checking ",nc,"rows.","\n")
##  ind <- sample(1:n,nc,replace=FALSE)
##  Ss <- S[ind,]
##  olcheck <- check_overlap(S=Ss, rational=rational, quick = quick, backend = backend, solver = solver)
##  if(isTRUE(olcheck)) break()
##  if(nco >= n) break() 
##  i <- i+1
##  }
##  return(olcheck)   
##  }
## }
