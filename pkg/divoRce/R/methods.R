########### Pre-fit

##### check_separation
#' @export
check_separation.factor <- function(x, rational = FALSE, ... )
{
    checksep(y = x, rational=rational, ...)
}

#' @export
check_separation.character <-  check_separation.factor
#' @export
check_separation.logical <- check_separation.factor
#' @export
check_separation.numeric <- check_separation.factor
#' @export
check_separation.integer <- check_separation.factor
#' @export
check_separation.numeric <- check_separation.factor 

#' @export
check_separation.matrix <- function(x, rational = FALSE, ... )
{
    checksep(S=x, rational=rational, ...)
}

##### diagnose_separation
#' @export
diagnose_separation.factor <- function(x, rational = FALSE, ... )
{
    diagsep(y = x, rational=rational, ...)
}

#' @export
diagnose_separation.character <-  diagnose_separation.factor
#' @export
diagnose_separation.logical <- diagnose_separation.factor
#' @export
diagnose_separation.numeric <- diagnose_separation.factor
#' @export
diagnose_separation.integer <- diagnose_separation.factor
#' @export
diagnose_separation.numeric <- diagnose_separation.factor 

#' @export
diagnose_separation.matrix <- function(x, rational = FALSE, ... )
{
    diagsep(S=x, rational=rational, ...)
}

##### separation_columns
#' @export
separation_columns.factor <- function(x, rational = FALSE, ... )
{
    sepcols(y = x, rational=rational, ...)
}

#' @export
separation_columns.character <-  separation_columns.factor
#' @export
separation_columns.logical <- separation_columns.factor
#' @export
separation_columns.numeric <- separation_columns.factor
#' @export
separation_columns.integer <- separation_columns.factor
#' @export
separation_columns.numeric <- separation_columns.factor 

#' @export
separation_columns.matrix <- function(x, rational = FALSE, ... )
{
    sepcols(S=x, rational=rational, ...)
}

##### separation_rows
#' @export
separation_rows.factor <- function(x, rational = FALSE, ... )
{
    seprows(y = x, rational=rational, ...)
}

#' @export
separation_rows.character <-  separation_rows.factor
#' @export
separation_rows.logical <- separation_rows.factor
#' @export
separation_rows.numeric <- separation_rows.factor
#' @export
separation_rows.integer <- separation_rows.factor
#' @export
separation_rows.numeric <- separation_rows.factor 

#' @export
separation_rows.matrix <- function(x, rational = FALSE, ... )
{
    seprows(S=x, rational=rational, ...)
}

##### recession_cone
#' @export
recession_cone.factor <- function(x, rational = FALSE, ... )
{
    reccone(y = x, rational=rational, ...)
}

#' @export
recession_cone.character <-  recession_cone.factor
#' @export
recession_cone.logical <- recession_cone.factor
#' @export
recession_cone.numeric <- recession_cone.factor
#' @export
recession_cone.integer <- recession_cone.factor
#' @export
recession_cone.numeric <- recession_cone.factor 

#' @export
recession_cone.matrix <- function(x, rational = FALSE, ... )
{
    reccone(S=x, rational=rational, ...)
}



############# POST FIT 

#' @export
#' @importFrom stats model.frame model.matrix
check_separation.osm <- function(x, rational = FALSE, ... )
{
    y <- model.frame(x)[,1]
    X <- model.matrix(x)
    checksep_osm(y=y,X=X,rational=rational)
}
