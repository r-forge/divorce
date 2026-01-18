#' Function to calculate the structure vector matrix X* for categorical outcomes.
#'
#' @param y an outcome variable. Should be a factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that \code{X} already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the matrices be returned in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), the structure vector matrix is returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. This returned matrix is like \code{-nomin_Xstar(y,X)}.
#' 
#' @return a matrix of structure vectors with or without labels
#' @export
struc_vec <- function(y, X, label=TRUE, rational=FALSE){
   X <- as.matrix(X)
   israt <- rat_cols(X)
   if(israt) {
       rational <- TRUE
       X <- rcdd::q2d(X)
       }
   y <- as.factor(y)
   refcat <- levels(y)[1]
   a <- as.numeric(y)
   n <- length(a) 
   n.cat <- length(unique(a))
   r <- function(i){
      M <- -diag(n.cat-1)
      if(a[i]!=1)  M[,a[i]-1] <- 1
      sout <- kronecker(M, t(X[i,]))
      ## We label the matrix of structure vectors this way:
      ## Each column gets the category and a :: and the name of the effect column
      ## So say the categories are a, b, c and a is reference we return b::Intercept and c::Intercept 
      ## reference category is not returned clearly.
      catnames <- levels(y)[-1]
      colnames(sout) <- paste(rep(catnames,each=length(colnames(X))),rep(colnames(X),n.cat-1),sep="::")
      rownames(sout) <- paste(rep(i,n.cat-1),seq(1,n.cat-1),sep=".")
      sout
   }
   tmpp <- lapply(seq_len(n),r) 
   out <- do.call("rbind", tmpp)
   if(rational) out <- rcdd::d2q(out) 
   if(!isTRUE(label)) colnames(out) <- rownames(out) <- NULL 
   return(out)
}


## struc_vecOLD <- function(y, X, label=TRUE){
##    y <- as.factor(y)
##    refcat <- levels(y)[1]
##    a <- as.numeric(y)
##    n <- length(a) 
##    n.cat <- length(unique(a))
##    r <- function(i,label=TRUE){
##       M <- -diag(n.cat-1)
##       if(a[i]!=n.cat)  M[,a[i]] <- 1
##       kronecker(M, t(X[i,]))
##    }
##    out <- do.call("rbind", lapply(seq_len(n),r))
##    ## We label the matrix of structure vectors this way:
##     ## Each column gets the category and a :: and the name of the effect column
##     ## So say the categories are a, b, c and a is reference we return b::Intercept and c::Intercept 
##   ## reference category is not returned clearly.
##   ##TODO: Labeling is wrong!                
##    if(isTRUE(label))
##    {
##    catnames <- levels(y)[-1] 
##    colnames(out) <- paste(rep(catnames,each=length(colnames(X))),rep(colnames(X),n.cat-1),sep="::")
##    rownames(out) <- rep(rownames(X),n.cat-1)
##    }
##    return(out)
## }

