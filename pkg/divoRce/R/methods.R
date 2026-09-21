########### Pre-fit

##### check_separation
#' @export
#' @rdname check_separation
check_separation.default <- function(object, ... )
{
    stop("No  method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.",
       call. = FALSE)
}

#' @export
#' @rdname check_separation
#'
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used?
#' @param quick logical; if \code{TRUE}, use the quick linear program
#'   variant, otherwise the full linear program.
#' @param backend character string specifying the backend for the linear
#'   program. One of \code{"rcdd"} (default, and the only option when
#'   \code{rational = TRUE}) or \code{"ROI"}.
#' @param solver character string specifying the solver used by the
#'   backend. Defaults to \code{"DualSimplex"} for \code{backend = "rcdd"},
#'   and to the first LP solver returned by
#'   \code{\link[ROI]{ROI_applicable_solvers}} for \code{backend = "ROI"}.
check_separation.factor <- function(object, X, model = NULL, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    y <- object
    if(isTRUE(quick))
    {
        return(separation_quick_check(y = y, X = X, model = model, rational = rational, backend = backend, solver = solver, ...))
    } else {
        return(checksep_worker(y = y, X = X, model = model, rational = rational, backend = backend, solver = solver, ...))
    }
}

#' @export
#' @rdname check_separation
check_separation.logical <- check_separation.factor
#' @export
#' @rdname check_separation
check_separation.numeric <- check_separation.factor
#' @export
#' @rdname check_separation
check_separation.integer <- check_separation.factor
#' @export
#' @rdname check_separation
check_separation.character<- check_separation.factor 

#' @export
#' @rdname check_separation
check_separation.matrix <- function(object, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL,  ... )
{
    S <- object
    if(isTRUE(quick))
    {
       return(separation_quick_check(S = S, rational = rational, backend = backend, solver = solver, ...))  
    } else {
       return(checksep_worker(S = S, rational = rational, backend = backend, solver = solver, ...))
    }
}

#' @rdname check_separation
#'
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#'
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
check_separation.formula <- function(object, data, model = NULL, rational = FALSE, quick = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    formula <- object
    yx <- make_yx(formula, data, contrasts) 
    check_separation(object = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, quick = quick, ...)
}

##### diagnose_separation
#' @export
#' @rdname diagnose_separation
diagnose_separation.default <- function(object, ... )
{
   stop("No  method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.",
       call. = FALSE)
}

#' @export
#' @rdname diagnose_separation
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used?
#' @param backend character string specifying the backend for the linear
#'   program. One of \code{"rcdd"} (default, and the only option when
#'   \code{rational = TRUE}) or \code{"ROI"}.
#' @param solver character string specifying the solver used by the
#'   backend. Defaults to \code{"DualSimplex"} for \code{backend = "rcdd"},
#'   and to the first LP solver returned by
#'   \code{\link[ROI]{ROI_applicable_solvers}} for \code{backend = "ROI"}.
diagnose_separation.factor <- function(object, X, model = NULL, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    y <- object
    return(diagsep_worker(y = y, X = X, model = model, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname diagnose_separation 
diagnose_separation.character <-  diagnose_separation.factor
#' @export
#' @rdname diagnose_separation
 diagnose_separation.logical <- diagnose_separation.factor
#' @export
#' @rdname diagnose_separation
 diagnose_separation.numeric <- diagnose_separation.factor
#' @export
#' @rdname diagnose_separation
diagnose_separation.integer <- diagnose_separation.factor

#' @export
#' @rdname diagnose_separation
diagnose_separation.matrix <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    S <- object
    return(diagsep_worker(S = S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname diagnose_separation
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#'
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
diagnose_separation.formula <- function(object, data, model = NULL, rational = FALSE, contrasts = NULL,  backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    formula <- object 
    yx <- make_yx(formula, data, contrasts) 
    out <- diagsep_worker(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, ...)
    out$modelcall <- formula
    if(model=="sl"){
        out$modelcall <- NULL
        for(i in 1:length(out)) out[[i]]$modelcall <- formula
        }
    return(out)
}

##### separation_columns
#' @export
#' @rdname separation_columns
separation_columns.default <- function(object, ... )
{
   stop("No method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.",
       call. = FALSE)
}


#' @export
#' @rdname separation_columns
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used.
#' @param backend character string specifying the backend for the linear
#'   program. One of \code{"rcdd"} (default, and the only option when
#'   \code{rational = TRUE}) or \code{"ROI"}.
#' @param solver character string specifying the solver used by the
#'   backend. Defaults to \code{"DualSimplex"} for \code{backend = "rcdd"},
#'   and to the first LP solver returned by
#'   \code{\link[ROI]{ROI_applicable_solvers}} for \code{backend = "ROI"}.
separation_columns.factor <- function(object, X, model = NULL, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    y <- object
    return(sepcols_worker(y = y, X=X, model = model, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname separation_columns
separation_columns.character <-  separation_columns.factor
#' @export
#' @rdname separation_columns
separation_columns.logical <- separation_columns.factor
#' @export
#' @rdname separation_columns
separation_columns.numeric <- separation_columns.factor
#' @export
#' @rdname separation_columns
separation_columns.integer <- separation_columns.factor

#' @export
#' @rdname separation_columns
separation_columns.matrix <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    S <- object
    return(sepcols_worker(S = S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname separation_columns
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#' 
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_columns.formula <- function(object, data, model = NULL, rational = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    formula <- object
    yx <- make_yx(formula, data, contrasts) 
    return(sepcols_worker(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, ...))
}

##### separation_rows
#' @export
#' @rdname separation_rows
separation_rows.default <- function(object, ... )
{
      stop("No method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.",
       call. = FALSE)
}


#' @export
#' @rdname separation_rows
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used.
separation_rows.factor <- function(object, X, model = NULL, rational = FALSE, ... )
{
    y <- object
    return(seprows_worker(y = y, X = X, model = model, rational=rational, ...))
}

#' @export
#' @rdname separation_rows 
separation_rows.character <-  separation_rows.factor
#' @export
#' @rdname separation_rows 
separation_rows.logical <- separation_rows.factor
#' @export
#' @rdname separation_rows
separation_rows.numeric <- separation_rows.factor
#' @export
#' @rdname separation_rows 
separation_rows.integer <- separation_rows.factor

#' @export
#' @rdname separation_rows 
separation_rows.matrix <- function(object, rational = FALSE, ... )
{
    S <- object
    return(seprows_worker(S=S, rational=rational, ...))
}

#' @rdname separation_rows
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#' 
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_rows.formula <- function(object, data, model = NULL , rational = FALSE, contrasts = NULL, ... )
{
    formula <- object
    yx <- make_yx(formula, data, contrasts) 
    return(seprows_worker(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}

##### recession_cone
#' @export
#' @rdname recession_cone
recession_cone.default <- function(object, ... )
{
      stop("No method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.",
       call. = FALSE)
}

#' @export
#' @rdname recession_cone
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used.
recession_cone.factor <- function(object, X, model = NULL, rational = FALSE, ... )
{
    y <- object
    return(reccone_worker(y = y, X = X, model = model, rational=rational, ...))
}


#' @export
#' @rdname recession_cone 
recession_cone.character <-  recession_cone.factor
#' @export
#' @rdname recession_cone 
recession_cone.logical <- recession_cone.factor
#' @export
#' @rdname recession_cone 
recession_cone.numeric <- recession_cone.factor
#' @export
#' @rdname recession_cone 
recession_cone.integer <- recession_cone.factor


#' @export
#' @rdname recession_cone 
recession_cone.matrix <- function(object, rational = FALSE,  ... )
{
    S <- object
    return(reccone_worker(S = S, rational=rational, ...))
}

#' @rdname recession_cone
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#' 
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
recession_cone.formula <- function(object, data, model = NULL , rational = FALSE, contrasts = NULL,  ... )
{
    formula <- object
    yx <- make_yx(formula, data, contrasts) 
    return(reccone_worker(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}


############# POST FIT 

### OSM 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_separation
check_separation.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(object = y, X = X, model = "os", rational = rational, backend = backend, solver = solver, quick = quick, ...))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    out <- diagsep_os(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- x$call
    return(out)
}

#' @export
#' @rdname separation_columns
separation_columns.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_os(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname separation_rows
separation_rows.osm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_os(y=y,X=X,rational=rational))
}

#' @export
#' @rdname recession_cone
recession_cone.osm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_os(y=y,X=X,rational=rational))
}

#### CLM

#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_separation
check_separation.clm <- function(object, rational = FALSE,  backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(check_separation(object = y, X = X, model = "cl", rational = rational, quick = quick, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object   
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    out <- diagsep_cl(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- object$call
    return(out)
}

#' @export
#' @rdname separation_columns
separation_columns.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname separation_rows
separation_rows.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname recession_cone 
recession_cone.clm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(reccone_cl(y=y,X=X,rational=rational))
}

###  polr

#' @export
#' @rdname check_separation
check_separation.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(object = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    out <- diagsep_cl(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- x$call
    return(out)
}

#' @export
#' @rdname separation_columns
separation_columns.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname separation_rows
separation_rows.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname recession_cone
recession_cone.polr <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_cl(y=y,X=X,rational=rational))
}


#### multinom 

#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_separation
check_separation.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, ...))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    out <- diagsep_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- x$call
    return(out)
}

#' @export
#' @rdname separation_columns
separation_columns.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname separation_rows
separation_rows.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname recession_cone
recession_cone.multinom <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_bcl(y=y,X=X,rational=rational))
}


#####  GLM binary
## TODO what for the aggregation interface?

#' @export
#' @importFrom stats model.matrix model.frame
#' @rdname check_separation
check_separation.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(check_separation(object = y, X = X, model = "b", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    out <- diagsep_b(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- object$call
    return(out)
}

#' @export
#' @rdname separation_columns
separation_columns.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(sepcols_b(y=y,X=X,rational=rational, backend = backend, solver = solver)) 
}

#' @export
#' @rdname separation_rows
separation_rows.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(seprows_b(y=y,X=X,rational=rational)) 
}

#' @export
#' @rdname recession_cone
recession_cone.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(reccone_b(y=y,X=X,rational=rational)) 
}


########## bracl
#' @export
#' @importFrom stats model.matrix 
#' @rdname check_separation
check_separation.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(check_separation(object = y, X = X, model = "acl", rational = rational, backend = backend, solver = solver, quick = quick))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(check_separation(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
        }
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        out <- diagsep_acl(y=y,X=X,rational=rational, backend = backend, solver = solver)
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        out <- diagsep_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver)
    }
   out$modelcall <- object$call
   return(out)
}

#' @export
#' @rdname separation_rows
separation_rows.bracl <- function(object, rational = FALSE, ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(seprows_acl(y=y,X=X,rational=rational))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(seprows_bcl(y=y,X=X,rational=rational))
        }
}

#' @export
#' @rdname separation_columns
separation_columns.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(sepcols_acl(y=y,X=X,rational=rational, backend = backend, solver = solver ))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver ))
        }
}

#' @export
#' @rdname recession_cone
recession_cone.bracl <- function(object, rational = FALSE,  ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(reccone_acl(y=y,X=X,rational=rational))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(reccone_bcl(y=y,X=X,rational=rational))
        }
}

########## brmultinom
#' @export
#' @importFrom stats model.matrix
#' @rdname check_separation
check_separation.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    y <- model.frame(object)[,1]
    X <- model.matrix(object)
    return(check_separation(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagnose_separation
diagnose_separation.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ...)
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    out <- diagsep_bcl(y=y,X=X,rational=rational,backend = backend, solver = solver)
    out$modelcall <- x$call
    return(out)
}


#' @export
#' @rdname separation_columns
separation_columns.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname separation_rows
separation_rows.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname recession_cone
recession_cone.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_bcl(y=y,X=X,rational=rational))
}


##### structure_vectors
#' @export
#' @rdname structure_vectors
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param label Should the columns and rows be labeled?
#' @param rational Should the matrix be returned in rational form (as per rcdd definition)?
structure_vectors.default <- function(x, X, model=c("b","bcl","acl","os","sl","cl"), label = TRUE, rational = FALSE, ... )
{
    y <- x
    if(length(unique(y))<2) stop("There is only one value in y.")
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    if(missing(model)) model <-  NULL
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(is.null(model))
    {
        warning("Default model class used.","\n")
        if(is.ordered(y) && length(unique(y))>2)
        {
            return(struc_vec_cl(y=y, X=X, label = label, rational=rational))
        } else {
            return(struc_vec_bcl(y=y, X=X, label = label, rational=rational))
            
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b= struc_vec_b(y=y, X=X, label = label, rational=rational),
           bcl= struc_vec_bcl(y=y, X=X, label = label, rational=rational),
           cl= struc_vec_cl(y=y, X=X, label = label, rational=rational),
           acl= struc_vec_acl(y=y, X=X, label = label, rational=rational),       
           sl=struc_vec_sl(y=y, X=X, label = label, rational=rational),
           os=struc_vec_os(y=y, X=X, label = label, rational=rational)
           )
}

#' Structure Vectors Formula Method
#'
#' Method for \code{structure_vectors} when the input is a formula and data frame.
#' 
#' @rdname structure_vectors
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#' 
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
structure_vectors.formula <- function(x, data, contrasts = NULL, model = NULL, label = TRUE,  rational = FALSE, ... )
{
    formula <- x
    yx <- make_yx(formula, data, contrasts) 
    structure_vectors(x = yx$y, X = yx$X, model = model, label = label, rational=rational, ...)
}


########### Pre-fit

##### check_overlap
#' @rdname check_overlap
#' @export
check_overlap.default <- function(object, ... )
{
   stop("No  method for object of class ",
       paste(class(object), collapse = "/"), 
       ". Supported classes: logical, integer, factor, numeric, character, matrix, glm, polr, clm, osm, nnet, multinom, brmultinom, bracl, brglm.", call. = FALSE)
}

#' @export
#' @rdname check_overlap
#' @param X a design matrix, e.g. generated via a call to
#'   \code{\link{model.matrix}}. \code{X} is expected to already contain the
#'   desired contrasts for factors (e.g., dummies) and any other expanded
#'   columns (e.g., for polynomials).
#' @param model character string specifying the model class to be fitted.
#'   One of:
#'   \itemize{
#'     \item \code{"b"} – binary
#'     \item \code{"bcl"} – baseline-category link
#'     \item \code{"cl"} – cumulative link
#'     \item \code{"acl"} – adjacent-category link
#'     \item \code{"sl"} – sequential link
#'     \item \code{"os"} – ordered stereotype model
#'   }
#'   If missing or \code{NULL}, defaults to \code{"cl"} for ordinal \code{y}
#'   and \code{"bcl"} otherwise.
#' @param rational logical; should rational arithmetic be used?
#' @param quick logical; if \code{TRUE}, use the quick linear program
#'   variant, otherwise the full linear program.
#' @param sequential conduct a sequential check on data subsets (default `FALSE`).
#' @param parallel conduct a check in parallel on data subsets via multicores (default `FALSE`).
#' @param backend character string specifying the backend for the linear
#'   program. One of \code{"rcdd"} (default, and the only option when
#'   \code{rational = TRUE}) or \code{"ROI"}.
#' @param solver character string specifying the solver used by the
#'   backend. Defaults to \code{"DualSimplex"} for \code{backend = "rcdd"},
#'   and to the first LP solver returned by
#'   \code{\link[ROI]{ROI_applicable_solvers}} for \code{backend = "ROI"}.
#' @param nc number of cores to be used for parallel execution. Defaults to 1.
#' @param nss number of subsets for parallel or sequneital execution. Defaults to 'nc' for parallel and 10 for sequential. If nss is below 1 or above n-1, it uses 'nss = 1'. 
#' @param ... additional arguments to be passed to other functions (e.g. to mclappy in parallel execution)
#' @importFrom parallel mclapply
check_overlap.factor <- function(object, X, rational = FALSE, quick = FALSE, sequential= FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, nc = NULL, nss = NULL, ... ) {
    y <- object
    
    if (isTRUE(parallel) && isTRUE(sequential)) {
        warning("Both 'parallel' and 'sequential' are TRUE. ",
                "'sequential' is ignored.")
    }


    if(isTRUE(parallel)) {
        return(check_overlap_parallel(y = y, X = X, rational = rational, quick = quick, backend = backend, solver = solver, nss = nss, nc = nc, ...))
    }

    if(isTRUE(sequential))  {
        return(check_overlap_sequential(y = y, X = X, rational = rational, quick = quick, backend = backend, solver = solver, nss=nss, ...))
    }

    return(check_overlap_worker(y = y, X = X, rational = rational, quick = quick, backend = backend, solver = solver, ...))
}

##' @export
##' @rdname check_overlap
check_overlap.logical <- check_overlap.factor
##' @export
##' @rdname check_overlap
check_overlap.numeric <- check_overlap.factor
##' @export
##' @rdname check_overlap
check_overlap.integer <- check_overlap.factor
##' @export
##' @rdname check_overlap
check_overlap.character<- check_overlap.factor 

#' @export
#' @rdname check_overlap
check_overlap.matrix <- function(object, rational = FALSE, quick = FALSE, sequential = FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, nc = NULL, nss = NULL, ... ){
    S <- object

    if (isTRUE(parallel) && isTRUE(sequential)) {
        warning("Both 'parallel' and 'sequential' are TRUE. ",
                "'sequential' gets ignored.")
    }

    if(isTRUE(parallel)) {
        return(check_overlap_parallel(S = S, rational = rational, quick = quick, backend = backend, solver = solver, nss = nss, nc = nc, ...))
    }

    if(isTRUE(sequential)) {
        return(check_overlap_sequential(S = S, rational = rational, quick = quick, backend = backend, solver = solver, nss=nss, ...))
    } 

    return(check_overlap_worker(S = S, rational = rational, quick = quick, backend = backend, solver = solver, ...))
}

##### check_overlap
#' @rdname check_overlap
#'
#' @param data either a standard data frame, list or environment (or object
#'   coercible by \code{\link{as.data.frame}} to a data frame) containing
#'   the variables in the model. If not found in \code{data}, the variables
#'   are taken from \code{environment(formula)}, typically the environment
#'   from which the function is called.
#'
#'   Alternatively, \code{data} can be a data frame or matrix containing
#'   rational numbers as per the definition in \pkg{rcdd} (i.e. columns are
#'   characters, entries are either integer numbers or ratios of integer
#'   numbers, e.g. \code{"1"} or \code{"-234/19008"}). This is checked
#'   internally; see \sQuote{Details} for what happens when this structure
#'   is discovered.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}. Only effective for standard
#'   data frames.
#'
#' @details
#' The \code{formula} method is for standard data frames and formulas that
#' work the same way as when used with \code{\link[stats]{glm}}. It does
#' not support extended formulas, and may not work for functions that do
#' formula processing differently.
#'
#' For a data frame or matrix given as rational numbers in the \pkg{rcdd}
#' definition, this is recognized but the formula does \emph{not} get
#' expanded and is taken literally. Consequently:
#' \itemize{
#'   \item variables in \code{formula} must match exactly with the column
#'     names in \code{data};
#'   \item factors need to be converted to dummies beforehand (which would
#'     not be possible in the rational format in any other way anyway).
#' }
#'
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
check_overlap.formula <- function(object, data, model = NULL, rational = FALSE, contrasts = NULL, quick = FALSE, sequential = FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    formula <- object
    yx <- make_yx(formula, data, contrasts) 
    check_overlap(object = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, quick = quick, sequential = sequential, parallel = parallel, ...)
}



### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
check_overlap.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(object = y, X = X, model = "os", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... ))
}

### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
check_overlap.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(check_overlap(object = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... ))
}


### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
check_overlap.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(object = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
check_overlap.multinom<- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

#####  GLM binary
## TODO what for the aggregation interface?

#' @export
#' @importFrom stats model.matrix model.frame
#' @rdname check_overlap
check_overlap.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(check_overlap(object = y, X = X, model = "b", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

########## bracl
#' @export
#' @importFrom stats model.matrix 
#' @rdname check_overlap
check_overlap.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE,  parallel = FALSE, sequential = FALSE, nss= NULL, nc = NULL , ...  )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(check_overlap(object = y, X = X, model = "acl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(check_overlap(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
        }
}

########## brmultinom
#' @export
#' @importFrom stats model.matrix
#' @rdname check_overlap
check_overlap.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... )
{
    y <- model.frame(object)[,1]
    X <- model.matrix(object)
    return(check_overlap(object = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}
