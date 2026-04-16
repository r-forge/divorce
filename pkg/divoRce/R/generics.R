#'
#'
#' @details \code{check_separation} is an S3 generic function. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{checksep} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{checksep}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{checksep}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}.  
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one. 
#' @rdname checksep
#' @examples
#' 
#' data(csepdat1)
#'
#' # pre fit
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' check_separation(y = outc, X = desma) 
#'
#' # post fit
#' m1 <- stats::glm(y~x1+x2,data=csepdat1,family=binomial())
#' check_separation(m1)
#' @export
check_separation<- function (..., rational, backend, solver, quick) {
    UseMethod("check_separation")
}


#' @details  \code{diagnose_separation} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{diagsep} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{diagsep}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{diagsep}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}. 
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @rdname diagsep
#' @examples
#' data(qcsepdatm)
#'
#' #pre fit
#' y<-as.factor(qcsepdatm$y)
#' X<-cbind("(Intercept)"=1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagnose_separation(y, X=X, model="bcl") 
#'
#' #post fit
#' if (require('nnet')) {
#' m1 <- nnet::multinom(y~x1+x2,data=qcsepdatm)
#' diagnose_separation(m1)
#' }
#' @export
diagnose_separation <- function (..., rational, backend, solver) {
    UseMethod("diagnose_separation")
}



#' @details  \code{separation_columns} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{sepcols} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{sepcols}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{sepcols}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}.
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI". 
#' @rdname detect_sepcols
#' @examples
#' data(qcsepdato)
#'
#' # pre fit
#' y<-factor(qcsepdato$y, ordered=TRUE)
#' X<-cbind("(Intercept)"=1,qcsepdato[,2:ncol(qcsepdato)])
#' separation_columns(y=y, X=X) 
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


#' @details  \code{separation_rows} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{seprows} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{seprows}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{seprows}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}. 
#' @param rational should rational arithmetic be used
#' @rdname seprows
#' @examples
#' data(qcsepdato)
#'
#' # pre fit
#' y<-factor(qcsepdato$y,ordered=TRUE)
#' X<-cbind("(Intercept)"=1,qcsepdato[,2:ncol(qcsepdato)])
#' separation_rows(y=y,X=X) 
#'
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

#' @details  \code{recession_cone} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{reccone} with it.   
#' 
#' @param ... arguments for the generic: For pre-fit \code{y}, \code{X} with \code{y} a vector of type factor, character, logical, numeric or integer. This is the \code{y} argument of \code{reccone}. In this case one also needs to supply the argument \code{X} and optional but recommended a \code{model}. One can also supply a matrix \code{S}, in which case we treat it as the \code{S} argument to \code{reccone}. For post-fit this can currently be an object of class \code{glm}, \code{polr}, \code{clm}, \code{osm} or \code{nnet}.
#' @param rational should rational arithmetic be used
#' @rdname reccone
#' @examples
#' data(csepdat1)
#'
#' # pre fit
#' y <- factor(qcsepdato$y, ordered=TRUE)
#' X <- cbind("(Intercept)"=1,qcsepdato[,2:ncol(qcsepdato)])
#' recession_cone(y=y,X=X) 
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


