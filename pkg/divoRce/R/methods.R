########### Pre-fit

##### check_separation
#' @export
#' @rdname checksep
check_separation.default <- function(y, X, rational = FALSE, ... )
{
    checksep(y = y, X = X, rational=rational, ...)
}

#' @export
check_separation.factor <-  check_separation.default
#' @export
check_separation.logical <- check_separation.factor
#' @export
check_separation.numeric <- check_separation.factor
#' @export
check_separation.integer <- check_separation.factor
#' @export
check_separation.numeric <- check_separation.factor 

#' @export
#' @rdname checksep
check_separation.matrix <- function(S, rational = FALSE, ... )
{
    checksep(S = S, rational=rational, ...)
}

##### diagnose_separation
#' @export
#' @rdname diagsep
diagnose_separation.default <- function(y, X, rational = FALSE, ... )
{
    diagsep(y = y, X=X, rational=rational, ...)
}

#' @export
#' @rdname diagsep 
diagnose_separation.factor <-  diagnose_separation.default
## #' @export
## diagnose_separation.logical <- diagnose_separation.factor
## #' @export
## diagnose_separation.numeric <- diagnose_separation.factor
## #' @export
## diagnose_separation.integer <- diagnose_separation.factor
## #' @export
## diagnose_separation.numeric <- diagnose_separation.factor 

#' @export
#' @rdname diagsep
diagnose_separation.matrix <- function(S, rational = FALSE, ... )
{
    diagsep(S=S, rational=rational, ...)
}

##### separation_columns
#' @export
#' @rdname detect_sepcols
separation_columns.factor <- function(y, X, rational = FALSE, ... )
{
    sepcols(y = y, X=X, rational=rational, ...)
}

#' @export
#' @rdname detect_sepcols
separation_columns.default <-  separation_columns.factor
## #' @export
## separation_columns.logical <- separation_columns.factor
## #' @export
## separation_columns.numeric <- separation_columns.factor
## #' @export
## separation_columns.integer <- separation_columns.factor
## #' @export
## separation_columns.numeric <- separation_columns.factor 

#' @export
#' @rdname detect_sepcols
separation_columns.matrix <- function(S, rational = FALSE, ... )
{
    sepcols(S=S, rational=rational, ...)
}

##### separation_rows
#' @export
#' @rdname seprows 
separation_rows.factor <- function(y, X, rational = FALSE, ... )
{
    seprows(y = y, X = X, rational=rational, ...)
}

#' @export
#' @rdname seprows 
separation_rows.default <-  separation_rows.factor
#' @export
## separation_rows.logical <- separation_rows.factor
## #' @export
## separation_rows.numeric <- separation_rows.factor
## #' @export
## separation_rows.integer <- separation_rows.factor
## #' @export
## separation_rows.numeric <- separation_rows.factor 

#' @export
#' @rdname seprows 
separation_rows.matrix <- function(S, rational = FALSE, ... )
{
    seprows(S=S, rational=rational, ...)
}

##### recession_cone
#' @export
#' @rdname reccone
recession_cone.default <- function(y, X, rational = FALSE, ... )
{
    reccone(y = y, X = X, rational=rational, ...)
}

#' @export
#' @rdname reccone 
recession_cone.factor <-  recession_cone.default
## #' @export
## recession_cone.logical <- recession_cone.factor
## #' @export
## recession_cone.numeric <- recession_cone.factor
## #' @export
## recession_cone.integer <- recession_cone.factor
## #' @export
## recession_cone.numeric <- recession_cone.factor 

#' @export
#' @rdname reccone
recession_cone.matrix <- function(S, rational = FALSE, ... )
{
    reccone(S = S, rational=rational, ...)
}



############# POST FIT 

### OSM 
#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname checksep
#' @param object model obj́ect
check_separation.osm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    checksep_osm(y=y,X=X,rational=rational)
}

#' @export
#' @rdname diagsep
#' @param object model obj́ect
diagnose_separation.osm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    diagsep_osm(y=y,X=X,rational=rational)
}

#' @export
#' @rdname detect_sepcols
#' @param object model obj́ect
separation_columns.osm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    sepcols_osm(y=y,X=X,rational=rational)
}

#' @export
#' @rdname seprows
#' @param object model obj́ect
separation_rows.osm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    seprows_osm(y=y,X=X,rational=rational)
}

#' @export
#' @rdname reccone
#' @param object model obj́ect
recession_cone.osm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    reccone_osm(y=y,X=X,rational=rational)
}

#### CLM

#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname checksep
#' @param object model obj́ect
check_separation.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    checksep_cl(y=y, X=X, rational=rational)
}

#' @export
#' @rdname diagsep
#' @param object model obj́ect
diagnose_separation.clm <- function(object, rational = FALSE, ... )
{
    x <- object   
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    diagsep_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname detect_sepcols
#' @param object model obj́ect
separation_columns.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    sepcols_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname seprows
#' @param object model obj́ect
separation_rows.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    seprows_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname reccone
#' @param object model obj́ect
recession_cone.clm <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)$X
    reccone_cl(y=y,X=X,rational=rational)
}

###  polr

#' @export
#' @rdname checksep
#' @param object model obj́ect
check_separation.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    checksep_cl(y=y, X=X, rational=rational)
}

#' @export
#' @rdname diagsep
#' @param object model obj́ect
diagnose_separation.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    diagsep_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname detect_sepcols
#' @param object model obj́ect
separation_columns.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    sepcols_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname seprows
#' @param object model obj́ect
separation_rows.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    seprows_cl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname reccone
#' @param object model obj́ect
recession_cone.polr <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    reccone_cl(y=y,X=X,rational=rational)
}

#### multinom 

#' @export
#' @importFrom stats model.frame model.matrix
#' @rdname checksep
#' @param object model obj́ect
check_separation.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    checksep_bcl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname diagsep
#' @param object model obj́ect
diagnose_separation.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    diagsep_bcl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname detect_sepcols
#' @param object model obj́ect
separation_columns.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    sepcols_bcl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname seprows
#' @param object model obj́ect
separation_rows.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    seprows_bcl(y=y,X=X,rational=rational)
}

#' @export
#' @rdname reccone
#' @param object model obj́ect
recession_cone.multinom <- function(object, rational = FALSE, ... )
{
    x <- object
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    reccone_bcl(y=y,X=X,rational=rational)
}


#####  GLM binary
## TODO what for the aggregation interface?

#' @export
#' @importFrom stats model.matrix
#' @rdname checksep
#' @param object model obj́ect
check_separation.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    checksep_b(y=y,X=X,rational=rational)
}

#' @export
#' @rdname diagsep
#' @param object model obj́ect
diagnose_separation.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    diagsep_b(y=y,X=X,rational=rational)
}

#' @export
#' @rdname detect_sepcols
#' @param object model obj́ect
separation_columns.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    sepcols_b(y=y,X=X,rational=rational) 
}

#' @export
#' @rdname seprows
#' @param object model obj́ect
separation_rows.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    seprows_b(y=y,X=X,rational=rational) 
}

#' @export
#' @rdname reccone
#' @param object model obj́ect
recession_cone.glm <- function(object, rational = FALSE, ... )
{
    x <- object
    if(!(x$family$family %in% "binomial")) stop("This is only implemented for the binomial family.")
    y <- x$y
    X <- model.matrix(x)
    reccone_b(y=y,X=X,rational=rational) 
}
