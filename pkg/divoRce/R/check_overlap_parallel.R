#' A parallel check for overlap.
#'
#' Intended for data where one suspects overlap, this function checks for overlap on subset of the data in parallel via 'mclapply' from package parallel. It is a mid-level function for a response vector y and a design matrix X. 
#'
#'
#' The function splits the data into subsets and checks for overlap in them in parallel. If overlap exists in a subset that spans the whole space, then overlap exists in the overall data (Sablica et al,. 2026). If no overlap is found in any full-rank subset, it runs the linear program on the full data set to decide whether there is separation. The rank condition is important because one can have overlap/separation in a non-full-rank marix that does not hold up in the full rank case.  
#'
#' Since solving the exact linear program on the full data via \link{check_overlap} can take a long time for large data, this check can be quicker in case of overlap (especially if the overlapping categories are not rare). However, if there is separation or many subsets have non-full rank this function takes longer as we need to call run the progra on the full data anyway. This can be made less likely by using shuffle = TRUE. 
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param S structure vector matrix
#' @param nc number of cores to be used. Defaults to 'getOption("mc.cores", 1L)'.
#' @param nss number of parallel subsets. Defaults to 'nc'. If nss is below 1 or above n-1, it uses 'nss'=1. 
#' @param verbose should progress be reported. Defaults to 'FALSE'. Currently no effect.
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param quick Use the columnwise ('TRUE') or the full linear program ('FALSE').
#' @param backend which backend to use for the linear program. Can be 'rcdd' (default and only option for rational=TRUE) or 'ROI'.
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param shuffle should the data be shuffled before parallel checking (defaults to 'TRUE'). This can help if the data are ordered in such a way that subsets do not span the full space.
#' @param ... further arguments passed to 'mclapply'.
#'
#' 
#' @importFrom parallel mclapply
#' 
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#'
#' @export
check_overlap_parallel <- function(y, X, S, nc = getOption("mc.cores", 1L), nss = nc, verbose = FALSE, rational = FALSE, model = c("b","bcl","cl","acl","sl","os"), quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, shuffle = TRUE, ...) {
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
    splitlist <- vector("list",nss)
    breaks <- seq(1,n,length.out=nss+1)
    for (i in 2:length(breaks)) {
      strt <- breaks[i-1]
      end <- breaks[i]
      ind <- seq(ceiling(strt), floor(end), by = 1)
      splitlist[[i-1]] <- ind
    }
    polchecks <- parallel::mclapply(splitlist, function(ind) {
      ys <- y[ind]
      Xs <- X[ind,]
      olcheck <- NA
      if(isTRUE(all.equal(length(unique(ys)),length(unique(y))))) { #we skip evaluation if not all categories are in the subsample and say no overlap
        if(isTRUE(all.equal(qr(Xs)$rank,frank))) {
        olcheck <- check_overlap_worker(y=ys, X=Xs, rational=rational, model=model, quick = quick, backend = backend, solver = solver)
        }
     }
    olcheck
    }, mc.cores = nc, ...
    )
    if(any(isTRUE(polchecks))) return(TRUE)
    if(all(polchecks!=TRUE)) olout <- check_overlap_worker(y=y, X=X, rational=rational, model=model, quick=quick, backend=backend, solver = solver)
  } else {
      n <- dim(S)[1]
      if(isTRUE(shuffle)) {
        neword <- sample(seq(1,n))
        S <- S[neword,]
    }
    frank <- qr(S)$rank
    if(nss > n || nss < 1) nss <- 1
    splitlist <- vector("list",nss)
    breaks <- seq(1,n,length.out=nss+1)
    for (i in 2:length(breaks)) {
      strt <- breaks[i-1]
      end <- breaks[i]
      ind <- seq(ceiling(strt), floor(end), by = 1)
      splitlist[[i-1]] <- ind
    }
    polchecks <- parallel::mclapply(splitlist, function(ind) {
      Ss <- S[ind,]
      olcheck <- NA
      if(isTRUE(all.equal(qr(Ss)$rank,frank))) {
          olcheck <- check_overlap_worker(S=Ss, rational=rational, model=model, quick = quick, backend=backend, solver = solver)
          }
      olcheck
    }, mc.cores = nc, ...
    )
    if(any(isTRUE(polchecks))) return(TRUE)
    if(all(polchecks!=TRUE)) olout <- check_overlap_worker(y=y, X=X, rational=rational, model=model, quick=quick, backend=backend, solver = solver)
  }
  olout
}
     
