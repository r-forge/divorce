#' General worker function of overlap check.
#'
#' This function checks for overlap by calling the appropriate low-level functions. It is not generic.
#'
#' The function uses either a response vector y and a design matrix X or a structure vector matrix S. If S is given, y and X and model are ignored.
#'
#' @param y outcome vector 
#' @param X design matrix
#' @param S structure vector matrix
#' @param rational should rational arithmetic be used.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param quick use columnwise linear program.
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @return a Boolean; either 'TRUE' if there is overlap or 'FALSE' if not.
#' @noRd
check_overlap_worker<- function(y, X, S, rational=FALSE, model=c("bcl","b","cl","acl","sl","os"), quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL){
  if(missing(model)) model <- NULL
  if(missing(S)) {
      !isTRUE(check_separation(y, X=X, rational=rational, model = model, quick = quick, backend = backend, solver = solver))
  } else {
      !isTRUE(check_separation(S, rational=rational, backend = backend, quick = quick, solver = solver))
  }
}

#' for back comp
#' @noRd
checkovl <- check_overlap_worker


