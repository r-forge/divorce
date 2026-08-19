#'
#'
#' @details \code{check_separation} is an S3 generic function. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{checksep_worker} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{checksep_worker}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{checksep_worker}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}.  
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one. 
#' @rdname checksep_worker
#' @examples
#' 
#'  # pre fit
#'
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' check_separation(y = outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' check_separation(y = y, X = X, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' check_separation(yo, Xo, model = "sl")
#'
#' # Ordered stereotype
#' check_separation(yo, Xo, model = "os")
#' 
#' # Adjacent-category link
#' check_separation(yo, Xo, model= "acl")
#'
#' # Cumulative link
#' check_separation(yo, Xo, model = "cl")
#' 
#'
#' # post fit
#' m1 <- stats::glm(y~x1+x2,data=csepdat1,family=binomial())
#' check_separation(m1)
#' @export
check_separation<- function (..., rational, backend, solver, quick) {
    UseMethod("check_separation")
}


#' @details  \code{diagnose_separation} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{diagsep_worker} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{diagsep_worker}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{diagsep_worker}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{os} or \code{nnet}. 
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @rdname diagsep_worker
#' @examples
#' data(qcsepdatm)
#'
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' c1<- diagnose_separation(y = outc, X = desma, model = "b")
#' print(c1)
#' print(c1, "full")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagnose_separation(y = y, X = X, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' diagnose_separation(yo, Xo, model = "sl")
#'
#' # Ordered stereotype
#' diagnose_separation(yo, Xo, model = "os")
#' 
#' # Adjacent-category link
#' diagnose_separation(yo, Xo, model= "acl")
#'
#' # Cumulative link
#' diagnose_separation(yo, Xo, model = "cl")
#'
#' #post fit
#' if (require('nnet')) {
#' m1 <- nnet::multinom(y ~ x1 + x2, data = qcsepdatm)
#' diagnose_separation(m1)
#' }
#' @export
diagnose_separation <- function (..., rational, backend, solver) {
    UseMethod("diagnose_separation")
}



#' @details  \code{separation_columns} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{sepcols_worker} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{sepcols_worker}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{sepcols_worker}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{os} or \code{nnet}.
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @rdname sepcols_worker
#' @examples
#'
#'
#' #' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' separation_columns(y = outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' separation_columns(y = y, X = X, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' separation_columns(yo, Xo, model = "sl")
#'
#' # Ordered stereotype
#' separation_columns(yo, Xo, model = "os")
#' 
#' # Adjacent-category link
#' separation_columns(yo, Xo, model= "acl")
#'
#' # Cumulative link
#' separation_columns(yo, Xo, model = "cl")
#'
#' 
#' # post fit
#' if (require('clustord')) {
#' m1 <- clustord::osm(y~x1+x2, data = qcsepdato)
#' separation_columns(m1)
#' }
#' @export
separation_columns<- function (..., rational, backend, solver) {
    UseMethod("separation_columns")
}


#' @details  \code{separation_rows} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{seprows_worker} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{seprows_worker}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{seprows}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{os} or \code{nnet}. 
#' @param rational should rational arithmetic be used
#' @rdname seprows_worker
#' @examples
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' separation_rows(y = outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' separation_rows(y = y, X = X, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' separation_rows(yo, Xo, model = "sl")
#'
#' # Ordered stereotype
#' separation_rows(yo, Xo, model = "os")
#' 
#' # Adjacent-category link
#' separation_rows(yo, Xo, model= "acl")
#'
#' # Cumulative link
#' separation_rows(yo, Xo, model = "cl")
#' 
#' # post fit
#' if (require('MASS')) {
#' m1 <- MASS::polr(y~x1+x2, data = qcsepdato)
#' separation_rows(m1)
#' }
#' @export
separation_rows <- function (..., rational) {
    UseMethod("separation_rows")
}

#' @details  \code{recession_cone} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{reccone_worker} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{reccone_worker}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{reccone_worker}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{os} or \code{nnet}.
#' @param rational should rational arithmetic be used
#' @rdname reccone_worker
#' @examples
#' 
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' recession_cone(y = outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' recession_cone(y = y, X = X, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' recession_cone(yo, Xo, model = "sl")
#'
#' # Ordered stereotype
#' recession_cone(yo, Xo, model = "os")
#' 
#' # Adjacent-category link
#' recession_cone(yo, Xo, model= "acl")
#'
#' # Cumulative link
#' recession_cone(yo, Xo, model = "cl")
#'
#' # post fit
#' if (require('ordinal')) {
#' m1 <- ordinal::clm(y~x1+x2, data = qcsepdato)
#' recession_cone(m1)
#' }
#' @export
recession_cone <- function (..., rational) {
    UseMethod("recession_cone")
}


#'  Structure Vector S3 Generic
#'
#' Generic function to compute structure vectors from different types of inputs.
#'
#' @name structure_vectors
#' @param y First arguments for the generic: \code{y} is either a vector of type factor, character, logical, numeric or integer. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. \code{y} can also be a formula object, then it also needs an accompanying \code{data} argument and optional \code{model}.  
#' @param ... Additional arguments. Using a vector \code{y} one needs to supply the argument \code{X}. If \code{y} is a formula, one needs to supply the argument \code{data} as well.
#' @param model Model string specifying the model. One of "bcl", "b", "cl", "acl", "os", "sl". If missing or NULL the default model classes used are "cl" for \code{y} being an ordered factors and "bcl" otherwise.
#' @param label Should the columns and rows be labeled?
#' @param rational Should the matrix be returned in rational form.
#'
#' @details \code{structure_vectors} is an S3 generic function.
#'
#' @return A structure vector matrix with or without labeled rows and columns. For \code{model = "sl"}, a list of structure vector matrices, where each list element corresponds sequentially to the categories of y, starting with the lowest and ending with the (K-1)-th category. At each category k, we consider all observations with category k or higher for the structure vector matrix. 
#' @examples
#' 
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link (default method and no labelling) 
#' structure_vectors(yo, Xo, model = "sl", label = FALSE)
#'
#' # Ordered stereotype (formula method)
#' structure_vectors(y ~ x1 + x2, data = qcsepdato, model = "os")
#' 
#' @export
structure_vectors<- function(y, ..., model, label, rational) {
    UseMethod("structure_vectors")
}
