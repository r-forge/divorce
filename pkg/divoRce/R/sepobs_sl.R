#' Detect observations with separation for sequential (continuation-ratio) ordinal response models.
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
#' \item offobs the submatrix of the matrix (X,y) with the observations responsible over all categories
#' \item index the index of the separated observations over all categories 
#' }'
#' if reduce = FALSE a list of lists with a list for each category listing offobs and index as above but category specific
#' 
#' 
#' @export
#'
#' 
#' @examples
#' data(qcsepdato)
#' y<-qcsepdato$y
#' X<-qcsepdato[,2:ncol(qcsepdato)]
#' sepobs_sl(y,X)
#' 
sepobs_sl<-function(y,X,rational=FALSE,reduce=TRUE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  if(is.null(row.names(X))) row.names(X) <-  seq(1,dim(X)[1],by=1)
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  splitdat <- create_bseq(y,X)
  seqout <- lapply(splitdat,function(l) sepobs_b(l$y,l$X,rational=rational))
  #seqoutt
  if(reduce){
      labos <- unique(Reduce(c,lapply(seqout,function(x) row.names(x$offobs))))
      idxo <- which(row.names(X)%in%labos)
      Xoffobs <- X[idxo,,drop=FALSE]
      yoffobs <- y[idxo]
      offobs <-  data.frame(Xoffobs,yoffobs)
      attr(offobs,"assign") <- NULL
      #row.names(offobs) <- row.names(X)[idxo]
      colnames(offobs) <- c(colnames(X),"y")
      seqout <-  list(offobs=offobs,index=idxo)
  }
  seqout
}

#' @rdname sepobs_sl
#' @export
detect_sepobs_sl <- sepobs_sl
