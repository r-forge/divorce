########### Pre-fit

##### check_separation
#' @export
#' @rdname checksep
check_separation.default <- function(object, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
   cat("Could not find a method for this class:", class(object),"\n") 
}

#' @export
#' @rdname checksep
check_separation.factor <- function(y, X, rational = FALSE, quick = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    if(isTRUE(quick))
    {
        return(separation_qc(y = y, X = X, rational = rational, backend = backend, solver = solver, ...))
    } else {
        return(checksep(y = y, X = X, rational = rational, backend = backend, solver = solver, ...))
    }
}

##' @export
##' @rdname checksep
check_separation.logical <- check_separation.factor
##' @export
##' @rdname checksep
check_separation.numeric <- check_separation.factor
##' @export
##' @rdname checksep
check_separation.integer <- check_separation.factor
##' @export
##' @rdname checksep
check_separation.character<- check_separation.factor 

#' @export
#' @rdname checksep
check_separation.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    if(isTRUE(quick))
    {
       return(separation_qc(S = S, rational = rational, backend = backend, solver = solver, ...))  
    } else {
       return(checksep(S = S, rational = rational, backend = backend, solver = solver, ...))
    }
}

##### check_separation
#' @rdname checksep
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "osm", "sl".  
#' @param quick boolean flag whether the quick linear program is to be used or the full fledged one (default is FALSE). 
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
check_separation.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "osm", "sl") , rational = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    check_separation(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, quick = quick, ...)
}

##### diagnose_separation
#' @export
#' @rdname diagsep
diagnose_separation.default <- function(object,  rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
   cat("Could not find a method for this class:", class(object),"\n") 
}

#' @export
#' @rdname diagsep
diagnose_separation.factor <- function(y, X, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(diagsep(y = y, X = X, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname diagsep 
diagnose_separation.character <-  diagnose_separation.factor
#' @export
#' @rdname diagsep
 diagnose_separation.logical <- diagnose_separation.factor
#' @export
#' @rdname diagsep
 diagnose_separation.numeric <- diagnose_separation.factor
#' @export
#' @rdname diagsep
diagnose_separation.integer <- diagnose_separation.factor

#' @export
#' @rdname diagsep
diagnose_separation.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(diagsep(S = S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname diagsep
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "osm", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @export
diagnose_separation.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "osm", "sl") , rational = FALSE, contrasts = NULL,  backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    out <- diagsep(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, ...)
    out$modelcall <- formula
    if(model=="sl"){
        out$modelcall <- NULL
        for(i in 1:length(out)) out[[i]]$modelcall <- formula
        }
    return(out)
}

##### separation_columns
#' @export
#' @rdname detect_sepcols
separation_columns.default <- function(object, rational = FALSE, ... )
{
     cat("Could not find a method for this class:", class(object),"\n") 
}


#' @export
#' @rdname detect_sepcols
separation_columns.factor <- function(y, X, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(sepcols(y = y, X=X, rational=rational, backend = backend, solver = solver, ...))
}

#' @export
#' @rdname detect_sepcols
separation_columns.character <-  separation_columns.factor
#' @export
#' @rdname detect_sepcols
separation_columns.logical <- separation_columns.factor
#' @export
#' @rdname detect_sepcols
separation_columns.numeric <- separation_columns.factor
#' @export
#' @rdname detect_sepcols
separation_columns.integer <- separation_columns.factor

#' @export
#' @rdname detect_sepcols
separation_columns.matrix <- function(S, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    return(sepcols(S=S, rational=rational, backend = backend, solver = solver, ...))
}

#' @rdname detect_sepcols
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "osm", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_columns.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "osm", "sl") , rational = FALSE, contrasts = NULL, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(sepcols(y = yx$y, X = yx$X, model = model, rational=rational, backend = backend, solver = solver, ...))
}

##### separation_rows
#' @export
#' @rdname seprows 
separation_rows.default <- function(object, rational = FALSE, ... )
{
     cat("Could not find a method for this class:", class(object),"\n")
}


#' @export
#' @rdname seprows 
separation_rows.factor <- function(y, X, rational = FALSE, ... )
{
    return(seprows(y = y, X = X, rational=rational, ...))
}

#' @export
#' @rdname seprows 
separation_rows.character <-  separation_rows.factor
#' @export
#' @rdname seprows 
separation_rows.logical <- separation_rows.factor
#' @export
#' @rdname seprows
separation_rows.numeric <- separation_rows.factor
#' @export
#' @rdname seprows 
separation_rows.integer <- separation_rows.factor

#' @export
#' @rdname seprows 
separation_rows.matrix <- function(S, rational = FALSE, ... )
{
    return(seprows(S=S, rational=rational, ...))
}

#' @rdname seprows
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string. One of "bcl", "b", "cl", "acl", "osm", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
separation_rows.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "osm", "sl") , rational = FALSE, contrasts = NULL, ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(seprows(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}

##### recession_cone
#' @export
#' @rdname reccone
recession_cone.default <- function(object, rational = FALSE, ... )
{
       cat("Could not find a method for this class:", class(object),"\n")
}

#' @export
#' @rdname reccone
recession_cone.factor <- function(y, X, rational = FALSE, ... )
{
    return(reccone(y = y, X = X, rational=rational, ...))
}


#' @export
#' @rdname reccone 
recession_cone.character <-  recession_cone.factor
#' @export
#' @rdname reccone
recession_cone.logical <- recession_cone.factor
#' @export
#' @rdname reccone
recession_cone.numeric <- recession_cone.factor
#' @export
#' @rdname reccone
recession_cone.integer <- recession_cone.factor


#' @export
#' @rdname reccone
recession_cone.matrix <- function(S, rational = FALSE,  ... )
{
    return(reccone(S = S, rational=rational, ...))
}

#' @rdname reccone
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which the function is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered.
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#' @param model model string of the model to be checked. One of "bcl", "b", "cl", "acl", "osm", "sl".  
#' 
#' @details The `formula` method is for standard data frames and formulas that work the same way as when used with \code{\link[stats]{glm}}. It does not support extended formulas, and may not work for functions that do formula processing differently. For a data frame/matrix given as rational numbers in the \code{rcdd} definition this is recognized but the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).
#' @importFrom stats model.response is.empty.model model.matrix
#' @export
recession_cone.formula <- function(formula, data, model = c("bcl", "b", "cl", "acl", "osm", "sl") , rational = FALSE, contrasts = NULL,  ... )
{
    yx <- make_yx(formula, data, contrasts) 
    if(missing(model)) model <-  NULL
    return(reccone(y = yx$y, X = yx$X, model = model, rational=rational, ...))
}


############# POST FIT 

### OSM 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname checksep
#' @param object model object
check_separation.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "osm", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep
#' @param object model object
diagnose_separation.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    out <- diagsep_osm(y=y,X=X,rational=rational, backend = backend, solver = solver)
    out$modelcall <- x$call
    return(out)
}

#' @export
#' @rdname detect_sepcols
#' @param object model object
separation_columns.osm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_osm(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows
#' @param object model object
separation_rows.osm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_osm(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone
#' @param object model object
recession_cone.osm <- function(object, rational = FALSE,  ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_osm(y=y,X=X,rational=rational))
}

#### CLM

#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname checksep
#' @param object model object
check_separation.clm <- function(object, rational = FALSE,  backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(check_separation(y = y, X = X, model = "cl", rational = rational, quick = quick, backend = backend, solver = solver))
}

#' @export
#' @rdname diagsep
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
#' @rdname detect_sepcols
#' @param object model object
separation_columns.clm <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows
#' @param object model object
separation_rows.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone
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
#' @rdname checksep
#' @param object model object
check_separation.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "cl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep
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
#' @rdname detect_sepcols
#' @param object model object
separation_columns.polr <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_cl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows
#' @param object model object
separation_rows.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_cl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone
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
#' @rdname checksep
#' @param object model object
check_separation.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(check_separation(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep
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
#' @rdname detect_sepcols
#' @param object model object
separation_columns.multinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows
#' @param object model object
separation_rows.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone
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
#' @rdname checksep
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
#' @rdname diagsep
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
#' @rdname detect_sepcols
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
#' @rdname seprows
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
#' @rdname reccone
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
#' @rdname checksep
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
#' @rdname diagsep
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
#' @rdname seprows
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
#' @rdname detect_sepcols
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
#' @rdname reccone
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
#' @rdname checksep
#' @param object model object
check_separation.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, quick = FALSE, ... )
{
    y <- model.frame(object)[,1]
    X <- model.matrix(object)
    return(check_separation(y = y, X = X, model = "bcl", rational = rational, backend = backend, solver = solver, quick = quick))
}

#' @export
#' @rdname diagsep
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
#' @rdname detect_sepcols
#' @param object model object
separation_columns.brmultinom <- function(object, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(sepcols_bcl(y=y,X=X,rational=rational, backend = backend, solver = solver))
}

#' @export
#' @rdname seprows
#' @param object model object
separation_rows.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(seprows_bcl(y=y,X=X,rational=rational))
}

#' @export
#' @rdname reccone
#' @param object model object
recession_cone.brmultinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    return(reccone_bcl(y=y,X=X,rational=rational))
}
