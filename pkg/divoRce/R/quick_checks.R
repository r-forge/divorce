#' A quick check for overlap.
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param S structure vector matrix
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param backend which backend to use for the linear program. Can be 'rcdd' (default and only option for rational=TRUE) or 'ROI'.
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".   
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#' 
#'
#' @export
overlap_quick_check <- function(y, X, S, rational=FALSE, model=c("b","bcl","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL)
{
if(missing(S)) {
if(missing(model)) model <- NULL
return(!any(sepcols_worker(y = y,X = X, rational = rational, model = model, backend = backend, solver = solver)$separated))
        } else {
return(!any(sepcols_worker(S=S, rational = rational, backend = backend, solver = solver)$separated))    
        }
}

#'
overlap_qc <- overlap_quick_check

#' A quick check for separation.
#'
#' @param y outcome vector. 
#' @param X design matrix.
#' @param S structure vector matrix 
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param backend which backend to use for the linear program. Can be 'rcdd' (default and only option for rational=TRUE) or 'ROI'.
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".   
#' @return a Boolean; either 'TRUE' if we detect overlap or 'FALSE' if we do not (so the data show separation).
#'
#' @export
separation_quick_check <- function(y, X, S, rational=FALSE,model=c("b","bcl","cl","acl","sl","os"), backend = c("rcdd", "ROI"), solver = NULL)
{
if(missing(S)) {
if(missing(model)) model <- NULL
return(any(sepcols_worker(y=y,X=X,rational=rational,model=model, backend = backend, solver=solver)$separated))
} else {
  return(any(sepcols_worker(S=S,rational=rational, backend = backend, solver = solver)$separated))  
 }
}

#'
separation_qc <- separation_quick_check

