
#' Generic for checking for separation
#' 
#' @param x an object. For pre-fit this can be a vector of type factor, character, logical, numeric or integer. This is the y argument of checksep. In this case we also need an X. It can alos be a matrix, in which case we treat it as the S argument to checksep. For post-fit this can currently be an object of class glm, polr, clm, osm or nnet. 
#' @param rational should rational arithemtic be used
#' @param ... other arguments
#' @export
#' @rdname checksep
check_separation<- function (x, rational, ... ) {
    UseMethod("check_separation")
}


#' Generic for detailed separation diagnostics
#' 
#'
#' @param x an object based on which we dispatch
#' @param rational should rational arithemtic be used
#' @param ... other arguments
#' @export
#' @rdname diagsep
diagnose_separation <- function (x, rational, ...) {
    UseMethod("diagnose_separation")
}


#' Generic for detecting separation columns
#' 
#' @param x an object based on which we dispatch
#' @param rational should rational arithemtic be used
#' @param ... other arguments
#' @rdname sepcols
#' @export
separation_columns<- function (x, rational, ...) {
    UseMethod("separation_columns")
}

#' Generic for detecting separation rows 
#' 
#' @param x an object based on which we dispatch
#' @param rational should rational arithemtic be used
#' @param ... other arguments
#' @rdname seprows
#' @export
separation_rows <- function (x, rational, ...) {
    UseMethod("separation_rows")
}



#' Generic for calculating the recession cone
#' 
#'
#' @param x an object based on which we dispatch
#' @param rational should rational arithemtic be used
#' @param ... other arguments
#' @rdname reccone
#' @export
recession_cone <- function (x, rational, ...) {
    UseMethod("recession_cone")
}


