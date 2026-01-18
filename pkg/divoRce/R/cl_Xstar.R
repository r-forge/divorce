#' Function to calculate the structure vector matrix X* for a cumulative link model.
#'
#' @param y an ordinal outcome variable. Should be an ordered factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials). For this function we also assume the X to either have an intercept column labeled with a string as *ntercept*, or not having an intercept column.  
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#'
#'@details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix) or as a data frame, they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#' 
#' @return a matrix of structure vectors with or without labels
#'
#' @export
cl_Xstar <- function(y, X, label=TRUE, rational=FALSE){
   X <- as.matrix(X)
   if(is.null(rownames(X))) rownames(X) <- seq(1,dim(X)[1],by=1)
    israt <- rat_cols(X)
    if(israt) {
       rational <- TRUE
       X <- rcdd::q2d(X)
       }
   intind<-grep("*ntercept*",colnames(X))
   if(isTRUE(any(intind>0)) & isTRUE(all(X[,intind]==1))) X <- X[,-intind,drop=FALSE]
   y <- droplevels(as.factor(y))
   a <- as.numeric(y)
   n <- length(a)
   n.cat <- length(unique(a))
   Z <- cbind(matrix(0, ncol = n.cat-1, nrow = n), X)
   r <- function(i){
      Z[,i-1] <- 1
      rbind(Z[a==i,], -Z[a==(i-1),])
   }
   out <- do.call("rbind", lapply(seq(2,n.cat),r))
   catnames <- sort(unique(y)) 
   colnames(out)[seq(1,length(catnames)-1)] <- paste(catnames[-1],"Threshold",sep="::")
   if(rational) out <- rcdd::d2q(out) 
   if(!isTRUE(label)) attr(out,"dimnames") <- NULL     
   return(out)
}

