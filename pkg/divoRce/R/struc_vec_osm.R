#' Function to calculate the negative structure vector matrix X* for an ordered stereotype model.
#'
#' @param y an ordinal outcome variable. Should be an ordered factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels? 
#' @param rational should the structure vectors been given in rational format?
#'
#'
#'@details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix) or as a data frame, they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. OSM checks do not need to consider the reference category. 
#' 
#' @return a matrix of negative structure vectors with or without labels
os_Xstar <- function(y, X, label=TRUE, rational=FALSE){
  X <- as.matrix(X)
  y <- droplevels(as.factor(y))

  # Remove intercept
  intind<-grep("*ntercept*",colnames(X))
  if(isTRUE(any(intind>0)) & isTRUE(all(X[,intind]==1))) X <- X[,-intind,drop=FALSE]
  labs <- colnames(X)

  if(is.null(row.names(X))) row.names(X) <- seq(1,dim(X)[1],by=1)

  israt <- rat_cols(X)
  if(israt) {
    rational <- TRUE
    X <- rcdd::q2d(X)
  }
  ## for the x_i
  a <- as.numeric(y)
  n.cat <- length(unique(a))
  n.obs <- length(y)
  yi <- rep(a, each=n.cat)
  ks <- rep(1:n.cat, n.obs)
  t1 <- sign(yi - ks)
  t2 <- kronecker(X, rep(1, n.cat))
  out <- t2 * t1
  colnames(out) <- labs
  ## for the intercepts
  A <- matrix(0, nrow=nrow(out), ncol=n.cat-1)
  colnames(A) <- paste(levels(y)[1:(n.cat-1)],"Intercept",sep="::")
  mask_y <- yi < n.cat
  if(any(mask_y)) {
    idx_y <- cbind(which(mask_y), yi[mask_y])
    A[idx_y] <- 1
  }
  mask_k <- ks < n.cat
  if(any(mask_k)) {
    idx_k <- cbind(which(mask_k), ks[mask_k])
    A[idx_k] <- A[idx_k] - 1
  }

  out <- cbind(A, out)
  row.names(out) <- paste0(rep(row.names(X), each=n.cat), ".", ks)
  out <- out[rowSums(abs(out)) > 0, , drop=FALSE]
  if(rational) out <- rcdd::d2q(out)
  if(!isTRUE(label)) attr(out, "dimnames") <- NULL
  return(out)
}

#' Function to calculate the structure vector matrix S for an ordered stereotype model.
#'
#' @param y an ordinal outcome variable. Should be an ordered factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels? 
#' @param rational should the structure vectors been given in rational format?
#'
#'
#'@details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix) or as a data frame, they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. OSM checks do not need to consider the reference category. 
#' 
#' @return a matrix of structure vectors with or without labels
#'
struc_vec_os <- function(y, X, label=TRUE, rational=FALSE){
  X <- as.matrix(X)
  y <- droplevels(as.factor(y))

  # Remove intercept
  intind<-grep("*ntercept*",colnames(X))
  if(isTRUE(any(intind>0)) & isTRUE(all(X[,intind]==1))) X <- X[,-intind,drop=FALSE]
  labs <- colnames(X)

  if(is.null(row.names(X))) row.names(X) <- seq(1,dim(X)[1],by=1)

  israt <- rat_cols(X)
  if(israt) {
    rational <- TRUE
    X <- rcdd::q2d(X)
  }
  ## for the x_i
  a <- as.numeric(y)
  n.cat <- length(unique(a))
  n.obs <- length(y)
  yi <- rep(a, each=n.cat)
  ks <- rep(1:n.cat, n.obs)
  t1 <- sign(yi - ks)
  t2 <- kronecker(X, rep(1, n.cat))
  out <- t2 * t1
  colnames(out) <- labs
  ## for the intercepts
  A <- matrix(0, nrow=nrow(out), ncol=n.cat-1)
  colnames(A) <- paste(levels(y)[1:(n.cat-1)],"Intercept",sep="::")
  mask_y <- yi < n.cat
  if(any(mask_y)) {
    idx_y <- cbind(which(mask_y), yi[mask_y])
    A[idx_y] <- 1
  }
  mask_k <- ks < n.cat
  if(any(mask_k)) {
    idx_k <- cbind(which(mask_k), ks[mask_k])
    A[idx_k] <- A[idx_k] - 1
  }

  out <- cbind(A, out)
  row.names(out) <- paste0(rep(row.names(X), each=n.cat), ".", ks)
  out <- out[rowSums(abs(out)) > 0, , drop=FALSE]
  out <- -1*out
  if(rational) out <- rcdd::d2q(out)
  if(!isTRUE(label)) attr(out, "dimnames") <- NULL
  return(out)
} 
