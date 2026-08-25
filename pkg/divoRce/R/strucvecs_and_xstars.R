#' Function to calculate a list of structure vector matrices S for a sequential link model.
#'
#' @param y an ordinal outcome variable with K categories. Should be an ordered factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials). For this function we also assume the X to either have an intercept column labeled with a string as *ntercept*, or not having an intercept column.  
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#'
#'@details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix) or as a data frame, they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#'
#' This function returns a list of structure vector matrices. Each list element corresponds sequentially to the categories of y, starting with the lowest and ending with the (K-1)-th category. At each category k, we consider all observations with category k or higher for a binary structure vector matrix.
#' 
#' @return a list of matrices of structure vectors with or without labels. 
#' @noRd
struc_vec_sl<-function(y, X, label=TRUE, rational=FALSE)
{
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y = y, X = X)
  svsl <- lapply(splitdat,function(l) struc_vec_b(y=l$y, X=l$X, label=label, rational=rational))
  return(svsl)
}

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
#' @noRd 
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

#' Function to calculate the structure vector matrix S for baseline-category outcomes.
#'
#' @param y a nominal or binary outcome variable. Works best if it is a factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. 
#'
#' @return a matrix of structure vectors with or without labels.
#' @noRd 
struc_vec_bcl <- function(y, X, label=TRUE, rational=FALSE){
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
   if(rational) out <- rcdd::d2q(out)
   if(!isTRUE(label)) colnames(out) <- row.names(out) <- NULL 
   return(out)
}


#' Function to calculate the negative structure vector matrix X* for an adjacent-category link model.
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
#' @noRd 
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


#' Function to calculate the structure vector matrix S for an adjacent-category link model.
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
#' @noRd 
struc_vec_acl <- function(y,X,label=TRUE,rational=FALSE)
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
  out <- -out
  colnames(out) <- labs
  if(rational) out <- rcdd::d2q(out) 
  if(!isTRUE(label)) attr(out,"dimnames") <- NULL     
  return(out)
}




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
#' @noRd
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
#' @noRd
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


#' Function to calculate the negative structure vector matrix X* for a cumulative link model.
#'
#' @param y an ordinal outcome variable. Should be an ordered factor else we order increasingly in an alpha-numeric fashion.
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials). For this function we also assume the X to either have an intercept column labeled with a string as *ntercept*, or not having an intercept column.  
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#'
#'@details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix) or as a data frame, they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#' 
#' @return a matrix of negative structure vectors with or without labels
#' @noRd
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


#' Function to calculate the structure vector matrix S for a cumulative link model.
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
#' @noRd
struc_vec_cl <- function(y, X, label=TRUE, rational=FALSE){
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
   out <- -1*out
   if(rational) out <- rcdd::d2q(out) 
   if(!isTRUE(label)) attr(out,"dimnames") <- NULL     
   return(out)
}

#' Function to calculate the negative structure vector matrix X* for binary outcomes.
#'
#' @param y a binary outcome variable. Works best if it is a factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#' @return a matrix of negative structure vectors with or without labels.
#' @noRd
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


#' Function to calculate the structure vector matrix S for binary outcomes.
#'
#' @param y a binary outcome variable. Works best if it is a factor but can also be numeric, boolean or character.
#' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param label should the structure vector matrix have row and column labels?
#' @param rational should the structure vectors been given in rational format?
#'
#' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), they are returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}.
#'
#' @return a matrix of negative structure vectors with or without labels.
#' @noRd
struc_vec_b <- function(y, X, label=TRUE, rational=FALSE){
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
   out <- Xstar
   if(rational) out <- rcdd::d2q(out)
   if(!isTRUE(label)) colnames(out) <- row.names(out) <- NULL 
   return(out)
}




## #' Function to calculate the structure vector matrix S for categorical outcomes. This is an old version and superseded by struc_vec_bcl
## #'
## #' @param y an outcome variable. Should be a factor else we order increasingly in an alpha-numeric fashion.
## #' @param X a design matrix, e.g. generated via a call to \code{model.matrix} or via the function \code{make_yx}. This means we expect that \code{X} already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
## #' @param label should the structure vector matrix have row and column labels?
## #' @param rational should the matrices be returned in rational format?
## #'
## #' @details If \code{X} is given as the standard R object for design matrices (i.e., a numeric matrix), the structure vector matrix is returned the same way unless \code{rational=TRUE}; then it is returned as a character matrix of rational numbers. If \code{X} is given in rational format, it is also returned as rational format even if rational is set to \code{FALSE}. 
## #' 
## #' @return a matrix of structure vectors with or without labels
## #' @noRd
## struc_vec <- function(y, X, label=TRUE, rational=FALSE){
##    X <- as.matrix(X)
##    israt <- rat_cols(X)
##    if(israt) {
##        rational <- TRUE
##        X <- rcdd::q2d(X)
##        }
##    y <- as.factor(y)
##    refcat <- levels(y)[1]
##    a <- as.numeric(y)
##    n <- length(a) 
##    n.cat <- length(unique(a))
##    r <- function(i){
##       M <- -diag(n.cat-1)
##       if(a[i]!=1)  M[,a[i]-1] <- 1
##       sout <- kronecker(M, t(X[i,]))
##       ## We label the matrix of structure vectors this way:
##       ## Each column gets the category and a :: and the name of the effect column
##       ## So say the categories are a, b, c and a is reference we return b::Intercept and c::Intercept 
##       ## reference category is not returned clearly.
##       catnames <- levels(y)[-1]
##       colnames(sout) <- paste(rep(catnames,each=length(colnames(X))),rep(colnames(X),n.cat-1),sep="::")
##       rownames(sout) <- paste(rep(i,n.cat-1),seq(1,n.cat-1),sep=".")
##       sout
##    }
##    tmpp <- lapply(seq_len(n),r) 
##    out <- do.call("rbind", tmpp)
##    if(rational) out <- rcdd::d2q(out) 
##    if(!isTRUE(label)) colnames(out) <- rownames(out) <- NULL 
##    return(out)
## }


