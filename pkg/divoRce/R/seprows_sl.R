#' Detect design matrix rows with separation for sequential (continuation-ratio) ordinal response models.
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we internally coerce to ordered factor interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' @param reduce should the results be pooled over all categories? Defaults to TRUE. 
#' 
#'
#' @return if reduce = TRUE a list with the list elements being a list of
#' \itemize{
#' \item offrows the submatrix of the matrix (X,y) with the rows responsible for separation over all categories
#' \item index the index of the rows responsible for separation over all categories 
#' }'
#' if reduce = FALSE a list of lists with a list for each category listing offrows and index as above but category specific
#' 
#' 
#' @export
#'
#' 
#' @examples
#' data(qcsepdato)
#' y<-qcsepdato$y
#' X<-qcsepdato[,2:ncol(qcsepdato)]
#' seprows_sl(y,X)
#' 
seprows_sl<-function(y,X,rational=FALSE,reduce=TRUE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  if(is.null(row.names(X))) row.names(X) <-  seq(1,dim(X)[1],by=1)
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y,X)
  seqout <- lapply(splitdat,function(l) seprows_b(l$y,l$X,rational=rational))
  #seqoutt
  if(reduce){
      labos <- unique(Reduce(c,lapply(seqout,function(x) row.names(x$offrows))))
      idxo <- which(row.names(X)%in%labos)
      Xoffrows <- X[idxo,,drop=FALSE]
      yoffrows <- y[idxo]
      offrows <-  data.frame(Xoffrows,yoffrows)
      attr(offrows,"assign") <- NULL
      #row.names(offrows) <- row.names(X)[idxo]
      colnames(offrows) <- c(colnames(X),"y")
      seqout <-  list(offrows=offrows,index=idxo)
  }
  seqout
}


#' @rdname seprows_sl
#' @export
detect_seprows_sl <- seprows_sl
