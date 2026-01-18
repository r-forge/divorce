#' Function to calculate the structure vector matrix X* for an adjacent-category link model.
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
acl_Xstar <- function(y,X,label=TRUE,rational=FALSE)
{
  X <- as.matrix(X)
  if(is.null(row.names(X))) row.names(X) <- seq(1,dim(X)[1],by=1)
  israt <- rat_cols(X)
  if(israt) {
      rational <- TRUE
      X <- rcdd::q2d(X)
  }
  intind <- grep("*ntercept*",colnames(X))
  if(isTRUE(any(intind>0)) & isTRUE(all(X[,intind]==1))) X <- X[,-intind,drop=FALSE]
  y <- droplevels(as.factor(y))
  n <- length(y)
  a <- as.numeric(y)
  m <- length(unique(a))
  make_z <- function(j, xi, m) {
      head <- if (j < m) {
        v <- numeric(m - 1L); v[j:(m - 1L)] <- 1; v
      } else {
       numeric(m - 1L)
      }
      tail <- if (j < m) -(m - j) * xi else rep(0, length(xi))
     c(head, tail)
  }
  labs1 <- paste(seq_len(m-1),"Threshold",sep="::")
  labs <- c(labs1,colnames(X))
  row_idx <- 1
  out <- matrix(NA,nrow=n*(m-1),ncol=(m-1)+dim(X)[2])
  row.names(out) <- 1:dim(out)[1]
  for (i in seq_len(n)) {
     xi <- X[i, ]
     j  <- a[i]
     zj <- make_z(j, xi, m)
     for (k in seq_len(m)) {
       if (k == j) next
       zk <- make_z(k, xi, m)
       out[row_idx, ] <- zj - zk
       row.names(out)[row_idx] <- paste0(row.names(X)[i],".",k)
       row_idx <- row_idx + 1L
    }
  }
  colnames(out) <- labs
  if(rational) out <- rcdd::d2q(out) 
  if(!isTRUE(label)) attr(out,"dimnames") <- NULL     
  return(out)
}
