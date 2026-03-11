#' Identify rows in the data matrix that cause separation for binary models. 
#'
#' This function checks which rows in X cause the separation (or none).
#'
#' 
#' @param y the binary outcome variable. Works best if it is a factor or ordered factor but can also be numeric, boolean or character. We coerce to factor internally. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used.
#'
#' @return a list with elements: 
#' \itemize{
#' \item offrows the submatrix of the matrix (X,y) with the rows responsible 
#' \item index the index of the rows responsible for separation 
#' }'
#' 
#' @export
#'
#' @examples
#' data(csepdat1)
#' y<-csepdat1$y
#' X<-cbind(1,csepdat1[,2:ncol(csepdat1)])
#' seprows_b(y,X) #separation
seprows_b<-function(y, X, rational=FALSE)
{
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(row.names(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE  
  y <- as.factor(y)
  #TODO: What with NA?  
  if(length(unique(y))>2) stop("This function needs binary outcomes.")   
  Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational)
  vrep <- cbind(0, 0, Xstar)
  if(rational && !rat_cols(Xstar)) vrep <- rcdd::d2q(vrep)
  lout <- rcdd::linearity(vrep, rep = "V") 
  #if(ratcols) X <- rcdd::q2d(X)
  idx <-seq(1,length(y),by=1)
  if (length(lout)==0){
      offrows <-  data.frame(X,y)
      idxo <- idx
      attr(offrows,"assign") <- NULL
  } else {
      lis0 <- row.names(Xstar)[lout]
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      idxo <- which(!(row.names(X)%in%unique(lis)))
      Xoffrows <- X[idxo,,drop=FALSE]
      yoffrows <- y[idxo]
      offrows <-  data.frame(Xoffrows,yoffrows)
      attr(offrows,"assign") <- NULL
      row.names(offrows) <- row.names(X)[idxo]
#old:      
#      Xoffrows <- X[-lout,,drop=FALSE]
#      yoffrows <- y[-lout]
#      offrows <-  data.frame(Xoffrows,yoffrows)
#      idxo <- idx[-lout] 
#      attr(offrows,"assign") <- NULL
    }
  colnames(offrows) <- c(colnames(X),"y")
  out <- list(offrows=offrows,index=idxo)
  out
  }

#' @rdname seprows_b
#' @export
detect_seprows_b <- seprows_b
