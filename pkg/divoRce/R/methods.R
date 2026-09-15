########### Pre-fit

##### check_separation
#' @param object an R object.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one.
#' @param ... further arguments to be passed to the low level function. For example the optional model argument. 
#' @export
#' @rdname check_separation
check_separation.default <- function(object, model = NULL, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
   cat("Could not find a method for this class:", class(object),"\n") 
}

#' @export
#' @rdname check_separation
#' @param y a categorical outcome vector 
#' @param X a design matrix, e.g. generated via a call to \code{\link{model.matrix}}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one.
#' @param ... further arguments to be passed to the low level function 
check_separation.factor <- function(y, X, model = NULL, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    if(isTRUE(quick))
    {
        return(separation_quick_check(y = y, X = X, model = model, rational = rational, backend = backend, solver = solver, ...))
    } else {
        return(checksep_worker(y = y, X = X, model = model, rational = rational, backend = backend, solver = solver, ...))
    }
}

##' @export
##' @rdname check_separation
check_separation.logical <- check_separation.factor
##' @export
##' @rdname check_separation
check_separation.numeric <- check_separation.factor
##' @export
##' @rdname check_separation
check_separation.integer <- check_separation.factor
##' @export
##' @rdname check_separation
check_separation.character<- check_separation.factor 

#' @export
#' @param S a matrix of structure vectors
#' @rdname check_separation
check_separation.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    if(isTRUE(quick))
    {
       return(separation_quick_check(S = S, rational = rational, backend = backend, solver = solver, ...))  
    } else {
       return(checksep_worker(S = S, rational = rational, backend = backend, solver = solver, ...))
    }
}

##### check_separation
#' @rdname check_separation
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "os" for ordered stereotype model. If missing or NULL it defaults to cumulative link for ordinal y and baseline-category for everything else.
#' @param rational should rational arithmetic be used
#' @param backend which backend to use for the linear program. Can be "rcdd" (default and only option for rational=TRUE) or "ROI".
#' @param solver the solver to be used in the backend. Defaults to "DualSimplex" for "rcdd" and the first LP solver returned by `ROI_applicable_solver()` for "ROI".
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one (default is FALSE). 
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
check_separation.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl") , rational = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    check_separation(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, quick = quick, ...)
}

##### diagnose_separation
#' @export
#' @rdname diagsep_worker
diagnose_separation.default <- function(object,  rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
   cat("Could not find a method for this class:", class(object),"\n") 
}

#' @export
#' @rdname diagsep_worker
diagnose_separation.factor <- function(y, X, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(diagsep_worker(y = y, X = X, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname diagsep_worker 
diagnose_separation.character <-  diagnose_separation.factor
#' @export
#' @rdname diagsep_worker
 diagnose_separation.logical <- diagnose_separation.factor
#' @export
#' @rdname diagsep_worker
 diagnose_separation.numeric <- diagnose_separation.factor
#' @export
#' @rdname diagsep_worker
diagnose_separation.integer <- diagnose_separation.factor

#' @export
#' @rdname diagsep_worker
diagnose_separation.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(diagsep_worker(S = S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname diagsep_worker
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "os", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @export
diagnose_separation.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl") , rational = FALSE, contrasts = NULL,  backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
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
#' @rdname sepcols_worker
separation_columns.default <- function(object, rational = FALSE, ... )
{
     cat("Could not find a method for this class:", class(object),"\n") 
}


#' @export
#' @rdname sepcols_worker
separation_columns.factor <- function(y, X, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(sepcols_worker(y = y, X=X, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname sepcols_worker
separation_columns.character <-  separation_columns.factor
#' @export
#' @rdname sepcols_worker
separation_columns.logical <- separation_columns.factor
#' @export
#' @rdname sepcols_worker
separation_columns.numeric <- separation_columns.factor
#' @export
#' @rdname sepcols_worker
separation_columns.integer <- separation_columns.factor

#' @export
#' @rdname sepcols_worker
separation_columns.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(sepcols_worker(S=S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname sepcols_worker
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "os", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_columns.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl") , rational = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(sepcols_worker(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, ...))
}

##### separation_rows
#' @export
#' @rdname seprows_worker 
separation_rows.default <- function(object, rational = FALSE, ... )
{
     cat("Could not find a method for this class:", class(object),"\n")
}


#' @export
#' @rdname seprows_worker 
separation_rows.factor <- function(y, X, rational = FALSE, ... )
{
    return(seprows_worker(y = y, X = X, rational=rational, ...))
}

#' @export
#' @rdname seprows_worker 
separation_rows.character <-  separation_rows.factor
#' @export
#' @rdname seprows_worker 
separation_rows.logical <- separation_rows.factor
#' @export
#' @rdname seprows_worker
separation_rows.numeric <- separation_rows.factor
#' @export
#' @rdname seprows_worker 
separation_rows.integer <- separation_rows.factor

#' @export
#' @rdname seprows_worker 
separation_rows.matrix <- function(S, rational = FALSE, ... )
{
    return(seprows_worker(S=S, rational=rational, ...))
}

#' @rdname seprows_worker
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "os", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_rows.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl") , rational = FALSE, contrasts = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(seprows_worker(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}

##### recession_cone
#' @export
#' @rdname reccone_worker
recession_cone.default <- function(object, rational = FALSE, ... )
{
       cat("Could not find a method for this class:", class(object),"\n")
}

#' @export
#' @rdname reccone_worker
recession_cone.factor <- function(y, X, rational = FALSE, ... )
{
    return(reccone_worker(y = y, X = X, rational=rational, ...))
}


#' @export
#' @rdname reccone_worker 
recession_cone.character <-  recession_cone.factor
#' @export
#' @rdname reccone_worker
recession_cone.logical <- recession_cone.factor
#' @export
#' @rdname reccone_worker
recession_cone.numeric <- recession_cone.factor
#' @export
#' @rdname reccone_worker
recession_cone.integer <- recession_cone.factor


#' @export
#' @rdname reccone_worker
recession_cone.matrix <- function(S, rational = FALSE,  ... )
{
    return(reccone_worker(S = S, rational=rational, ...))
}

#' @rdname reccone_worker
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string of the model to be checked. One of "bcl", "b", "cl", "acl", "os", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
recession_cone.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl") , rational = FALSE, contrasts = NULL,  ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(reccone_worker(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}


############# POST FIT 

### OSM 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_separation
#' @param object model object
check_separation.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "os", rational = rational, backend = backend, solver = solver, quick = quick, ...))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_os(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.osm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_os(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.clm <- function(object, rational = FALSE,  backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(check_separation(y = y, X = X, model = "cl", rational = rational, quick = quick, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, ...))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "b", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(sepcols_b(y=y,X=X,rational=rational, backend = backend, solver = solver)) 
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(seprows_b(y=y,X=X,rational=rational)) 
}

#' @export
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(check_separation(y = y, X = X, model = "acl", rational = rational, backend = backend, solver = solver, quick = quick))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(check_separation(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
        }
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname seprows_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
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
#' @rdname reccone_worker
#' @param object model object
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
#' @param object model object
check_separation.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    y <- model.frame(object)[,1]
    X <- model.matrix(object)
    return(check_separation(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep_worker
#' @param object model object
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
#' @rdname sepcols_worker
#' @param object model object
separation_columns.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows_worker
#' @param object model object
separation_rows.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone_worker
#' @param object model object
recession_cone.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_bcl(y=y,X=X,rational=rational))
}



########### Pre-fit

##### structure_vectors
#' @export
#' @rdname structure_vectors
#' @param X a design matrix, e.g. generated via a call to \code{\link{model.matrix}}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynominals).
#' 
structure_vectors.default <- function(y, X, model = c("bcl", "b", "cl", "acl", "os", "sl"), label = TRUE, rational = FALSE, ... )
{
    if(length(unique(y))<2) stop("There is only one value in y.")
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(missing(model)) model <- NULL
    if(is.null(model))
    {
        warning("Default model class used.","\n")
        if(is.ordered(y) & length(unique(y))>2)
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
#' @param y An object of class \code{"formula"} (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data A data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts Contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model Model string. One of "bcl", "b", "cl", "acl", "os", "sl".
#' @param label If TRUE rows and columns are labeled 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
structure_vectors.formula <- function(y, data, contrasts = NULL, model = c("bcl", "b", "cl", "acl", "os", "sl"), label = TRUE,  rational = FALSE, ... )
{
    formula <- y
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    structure_vectors(y = yx$y, X = yx$X, model = model, label = label, rational=rational, ...)
}


########### Pre-fit

##### check_overlap
#' @rdname check_overlap
#' @export
#' @param object an R object
check_overlap.default <- function(object, rational = FALSE, quick = FALSE, sequential= FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
   cat("Could not find a method for this class:", class(object),"\n") 
}

#' @export
#' @rdname check_overlap
#' @param y outcome vector 
#' @param X design matrix
#' @param nc number of cores to be used for parallel execution. Defaults to 'getOption("mc.cores", 1L)'.
#' @param nss number of subsets for parallel or sequneital execution. Defaults to 'nc' for parallel and 10 for sequential. If nss is below 1 or above n-1, it uses 'nss'=1. 
#' @param ... additional arguments to be passed to other functions (e.g. to mclappy in parallel execution)
check_overlap.factor <- function(y, X, rational = FALSE, quick = FALSE, sequential= FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, nc = NULL, nss = NULL, ... ) {
    if (isTRUE(parallel) && isTRUE(sequential)) {
        warning("Both 'parallel' and 'sequential' are TRUE. ",
                "'sequential' gets ignored.")
    }

    if(isTRUE(parallel)) {
        if (is.null(nc)) nc <- getOption("mc.cores", 1L)
        if (is.null(nss)) nss <- nc
        return(check_overlap_parallel(y = y, X = X, rational = rational, quick = quick, backend = backend, solver = solver, nss = nss, nc = nc, ...))
    }

    if(isTRUE(sequential))  {
        if (is.null(nss)) nss <- 10L
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
#' @param S structure vector matrix
check_overlap.matrix <- function(S, rational = FALSE, quick = FALSE, sequential = FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, nc = NULL, nss = NULL, ... ){
    if (isTRUE(parallel) && isTRUE(sequential)) {
        warning("Both 'parallel' and 'sequential' are TRUE. ",
                "'sequential' gets ignored.")
    }

    if(isTRUE(parallel)) {
        if (is.null(nc)) nc <- getOption("mc.cores", 1L)
        if (is.null(nss)) nss <- nc
        return(check_overlap_parallel(S = S, rational = rational, quick = quick, backend = backend, solver = solver, nss = nss, nc = nc, ...))
    }

    if(isTRUE(sequential)) {
        if (is.null(nss)) nss <- 10L
        return(check_overlap_sequential(S = S, rational = rational, quick = quick, backend = backend, solver = solver, nss=nss, ...))
    } 

    return(check_overlap_worker(S = S, rational = rational, quick = quick, backend = backend, solver = solver, ...))
}

##### check_overlap
#' @rdname check_overlap
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "os", "sl".  
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one (default is FALSE).
#' @param sequential If 'TRUE' checks sequentially. Then an 'nss' (number of subsets) should be supplied (defaults to 10).
#' @param parallel If 'TRUE' checks 'nss' subsets in parallel on 'nc' multiple cores. The 'nc' (number of cores) and 'nss' (number of subsets) should be supplied (defaults to 1 otherwise).
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
check_overlap.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "os", "sl"), rational = FALSE, contrasts = NULL, quick = FALSE, sequential = FALSE, parallel = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    check_overlap(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, quick = quick, sequential = sequential, parallel = parallel, ...)
}



############# POST FIT 

### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
#' @param object model object
check_overlap.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(y = y, X = X, model = "os", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... ))
}

### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
#' @param object model object
check_overlap.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(check_overlap(y = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... ))
}


### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
#' @param object model object
check_overlap.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(y = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

### 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname check_overlap
#' @param object model object
check_overlap.multinom<- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_overlap(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

#####  GLM binary
## TODO what for the aggregation interface?

#' @export
#' @importFrom stats model.matrix model.frame
#' @rdname check_overlap
#' @param object model object
check_overlap.glm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = FALSE, sequential = FALSE, nss = NULL, nc = NULL , ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    return(check_overlap(y = y, X = X, model = "b", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}

########## bracl
#' @export
#' @importFrom stats model.matrix 
#' @rdname check_overlap
#' @param object model object
check_overlap.bracl <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE,  parallel = FALSE, sequential = FALSE, nss= NULL, nc = NULL , ...  )
{
    y <- as.ordered(model.frame(object)[,1])
    X <- model.matrix(object)
    if(object$parallel)
        return(check_overlap(y = y, X = X, model = "acl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
    if(!object$parallel) {
        y <- factor(y, ordered = FALSE)
        return(check_overlap(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
        }
}

########## brmultinom
#' @export
#' @importFrom stats model.matrix
#' @rdname check_overlap
#' @param object model object
check_overlap.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ... )
{
    y <- model.frame(object)[,1]
    X <- model.matrix(object)
    return(check_overlap(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick, parallel = parallel, sequential = sequential, nss = nss, nc = nc, ...))
}
