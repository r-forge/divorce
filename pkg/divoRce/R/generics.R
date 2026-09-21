#' Check separation
#'
#' General separation check function.
#'
#' This function checks for (quasi-)complete separation by calling the
#' appropriate low-level functions either with linear program 1 (`quick = FALSE`; default`) or linear program 2 (`quick = TRUE`).
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return A logical, `TRUE` is separation is detected. 
#' @name check_separation
#' 
#' @details
#' \code{check_separation} is an S3 generic function.
#'
#' \strong{For developers:} If a method should be provided for the generic,
#' it is best to have that method create a matrix of structure vectors
#' \code{S} and use the generic with the matrix.
#'
#' 
#' @examples
#' 
#'  # pre fit
#'
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' check_separation(outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' check_separation(y, X = X, model = "bcl")
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
check_separation <- function(object, ...) {
  UseMethod("check_separation")
}

#' Detailed separation diagnostic for all categorical outcomes. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the rows in X/S for which we have separation.   
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offrows rows associated with separation
#' \item nr.offcols number of columns of the design matrix that have separation
#' \item offcols columns associated with separation 
#' }
#' For \code{model = "sl"} an object of class 'sepmod_sl'. It is a list or the above lists with the elements corresponding to each category.
#' @name diagnose_separation
#' 
#' @details
#' \code{diagnose_separation} is an S3 generic function.
#'
#' \strong{For developers:} If a method should be provided for the generic,
#' it is best to have that method create a matrix of structure vectors
#' \code{S} and use the generic with the matrix. 
#' @examples
#' data(qcsepdatm)
#'
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' c1<- diagnose_separation(outc, X = desma, model = "b")
#' print(c1)
#' print(c1, "full")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagnose_separation(y, X = X, model = "bcl")
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
diagnose_separation <- function(object, ...) {
  UseMethod("diagnose_separation")
}


#' Identify separation columns
#' 
#' This function identifies the columns in a design matrix/structure vector matrix that are responsible for separation. It calls lower level functions if given an argument or chooses based on the response type.
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return A list with the components:
#' \itemize{
#' \item ls the solution vector of the linear program
#' \item offcols the names of the columns that show separation
#' \item colnrs the index number of the columns that show separation
#' \item separated a logical vector of whetehr the colum shows separation
#' }
#' For \code{model = "sl"} it is a list of these lists for each category.
#' @name separation_columns
#' 
#' @details
#' \code{separation_columns} is an S3 generic function.
#'
#' \strong{For developers:} If a method should be provided for the generic,
#' it is best to have that method create a matrix of structure vectors
#' \code{S} and use the generic with the matrix.
#' @examples
#'
#'
#' #' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' separation_columns(outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' separation_columns(y, X = X, model = "bcl")
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
separation_columns<- function(object, ...) {
  UseMethod("separation_columns")
}

#' Identify separation rows
#' 
#' This function identifies the rows in a design matrix/structure vector matrix that are associated for separation. It calls lower level functions if given an argument or chooses based on the response type.
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return A list with the components:
#' \itemize{
#' \item offrows the submatrix of the matrix (X,y) with the rows responsible for separation
#' \item index the index of the rows responsible for separation
#' }
#' For \code{model = "sl"} it is a list of these lists for each category.
#' @name separation_rows
#' 
#' @details  \code{separation_rows} is S3 generic. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{seprows_worker} with it.   
#' @examples
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' separation_rows(outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' separation_rows(y, X = X, model = "bcl")
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
separation_rows <- function(object, ...) {
  UseMethod("separation_rows")
}

#' Recession cone calculation
#'
#' This function calculates the dimension of the recession cone and returns the recession cone.
#'
#' @name recession_cone 
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return A list with the components:
#' \itemize{
#' \item cone being the recession cone,
#' \item reccdim being the dimensions of the recession cone
#' \item index the row index of the structure vectors that are not linearities.
#' }
#' For \code{model = "sl"} 'cone' is the recession cone over all categories, 'reccdim' the dimension of the largest recession cone of any category and 'index' the row index of the structure vectors that are not linearities over all categories.
#' @details Note that in case of non full column rank, the 'reccdim' value is the dimension of the recession cone due to separation plus the number of columns that are linear dependent. 
#' @examples
#' 
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' recession_cone(outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' recession_cone(y, X = X, model = "bcl")
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
recession_cone <- function(object, ...) {
  UseMethod("recession_cone")
}

#' Check overlap
#'
#'  
#' This function checks for overlap by calling the appropriate low-level functions. It can be run with the linear program (`quick = FALSE`, default), linear program 2 (`quick = TRUE`), on sequential subsets of data (`sequential = TRUE`) or via multicore parallelization (`parallel = TRUE`).
#'
#' @param object an R object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (vector):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (matrix):} a matrix of structure vectors.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'     \item \strong{Post-fit:} a fitted model object of class \code{glm},
#'       \code{polr}, \code{clm}, \code{osm}, \code{brmultinom},
#'       \code{bracl}, \code{brglm} or \code{multinom}.
#'   }
#' @param ... further arguments passed to the methods or lower-level functions.
#' @return A logical, `TRUE` is overlap is detected. 
#'
#' @details \code{check_overlap} is an S3 generic function. For developers: If a method should be provided for the generic, it is best to have that method create a matrix of structure vectors \code{S} and use the low-level function \code{check_overlap_worker} with it.   
#'
#' @name check_overlap
#' @examples
#' 
#'  # pre fit
#'
#' ## Binary data
#' data(csepdat1)
#' outc<-csepdat1$y
#' desma<-cbind("(Intercept)"=1,csepdat1[,2:ncol(csepdat1)])
#' check_overlap(outc, X = desma, model = "b")
#'
#' # Nominal data 
#' # Baseline-category link
#' data(qcsepdatm)
#' check_overlap(y ~ x1 + x2, data = qcsepdatm, model = "bcl")
#'
#' # Ordinal data
#' data(qcsepdato)
#' yo<-qcsepdato$y
#' Xo<-qcsepdato[,2:ncol(qcsepdato)]
#' 
#' # Sequential link
#' check_overlap(yo, Xo, model = "sl", rational = TRUE)
#'
#' # Ordered stereotype
#' check_overlap(yo, Xo, model = "os", sequential = TRUE, nss =2)
#' 
#' # Adjacent-category link
#' check_overlap(yo, Xo, model= "acl", parallel = TRUE, nc = 1)
#'
#' # Cumulative link
#' check_overlap(yo, Xo, model = "cl", quick = TRUE)
#' 
#'
#' # post fit
#' m1 <- stats::glm(y~x1+x2,data=csepdat1,family=binomial())
#' check_overlap(m1)
#' @export
check_overlap <- function(object, ...) {
  UseMethod("check_overlap")
}



#' Structure Vector S3 Generic
#'
#' Generic function to compute structure vectors from different types of inputs.
#'
#' @name structure_vectors
#' @param x input object. Supported inputs:
#'   \itemize{
#'     \item \strong{Pre-fit (default):} a vector of type \code{factor},
#'       \code{character}, \code{logical}, \code{numeric} or \code{integer}.
#'       In this case one also needs to supply the argument \code{X} and,
#'       optionally but recommended, a \code{model}.
#'     \item \strong{Pre-fit (formula):} an object of class \code{"formula"}
#'       (or one that can be coerced to that class): a symbolic description
#'       of the model to be fitted. The details of model specification are
#'       given under \sQuote{Details} in \code{\link[stats]{glm}}. In this
#'       case one needs to supply \code{data}, with an optional \code{model}
#'       argument as well.
#'      }
#' @param ... Additional arguments.
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
structure_vectors<- function(x, ...) {
  UseMethod("structure_vectors")
}


