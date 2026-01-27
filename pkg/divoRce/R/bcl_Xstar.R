#' Function to calculate the negative structure vector matrix X* for baseline-category outcomes.
#'
#' @param y a nominal or binary outcome variable. Works best if it is a factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. This returned matrix is like \code{-struc_vec(y,X)}.
#'
#' @return a matrix of negative structure vectors with or without labels. 
#' @export
bcl_Xstar <- function(y, X, label=TRUE, rational=FALSE){
   X <- as.matrix(X)
   if(is.null(row.names(X))) row.names(X) <- seq(1,dim(X)[1],by=1)
   israt <- rat_cols(X)
   if(israt) {
       rational <- TRUE
       X <- rcdd::q2d(X)
       }
   y <- droplevels(as.factor(y))
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
                                        #rownames(sout) <- paste(rep(row.names(X)[i],n.cat-1),seq(1,n.cat-1),sep=".")
      rownames(sout) <- paste(rep(row.names(X)[i],n.cat-1),catnames,sep=".")
      sout
   }
   tmpp <- lapply(seq_len(n),r) 
   out <- do.call("rbind", tmpp)
   out <- -out
   if(rational) out <- rcdd::d2q(out)
   if(!isTRUE(label)) colnames(out) <- row.names(out) <- NULL 
   return(out)
}

