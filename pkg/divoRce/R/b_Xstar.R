#' Function to calculate the negative structure vector matrix X* for binary outcomes.
#'
#' @param y a binary outcome variable. Works best if it is a factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#'
#' @return a matrix of negative structure vectors with or without labels. 
#' @export
b_Xstar <- function(y, X, label=TRUE, rational=FALSE){
   X <- as.matrix(X)
   y <- droplevels(as.factor(y))
   if(nlevels(y)!=2) stop("y must be binary.")
   if(is.null(row.names(X))) row.names(X) <- seq(1,dim(X)[1],by=1)
   israt <- rat_cols(X)
   if(israt) {
       rational <- TRUE
       X <- rcdd::q2d(X)
       }

   refcat <- levels(y)[1]
   Xstar <- X
   Xstar[y==refcat,] <- -1*Xstar[y==refcat, ]
   out <- -Xstar
   if(rational) out <- rcdd::d2q(out)
   if(!isTRUE(label)) colnames(out) <- row.names(out) <- NULL 
   return(out)
}

