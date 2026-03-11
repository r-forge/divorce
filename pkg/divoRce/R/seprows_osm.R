#' Identify the rows in the data matrix that cause separation in ordered stereotype models.
#'
#' This function checks which rows in the data matrix given by X (e.g., observations) are responsible for separation on any category. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we corece to ordered factor and interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' 
#'
#' @return a list with elements: 
#' \itemize{
#' \item offrows the submatrix of the matrix (X,y) with the rows responsible for separation 
#' \item index the index of the rows responsible for separation
#' }
#' 
#' @export
#'
#' 
#'
#' @examples
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-qcsepdatm[,2:ncol(qcsepdatm)]
#' seprows_osm(y,X)
#' 
seprows_osm<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(row.names(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE
  y <- as.factor(y)
  Xstar <- osm_Xstar(y,X,label=TRUE,rational=rational) 
  lout <- linearities_osm(y,X,rational=rational)$index
  idx <-seq(1,length(y),by=1)
  if (length(lout)==0){
#      if(rational) X <- rcdd::d2q(X)
      offrows <-  data.frame(X,y)
      idxo <- idx
      attr(offrows,"assign") <- NULL
  } else {
     if (length(lout)==dim(Xstar)[1]){
      # All structure vectors are linearities = overlap and no rows are returned
         idxo <- integer(0)
         offrows <-  data.frame(X,y)[idxo,]
         attr(offrows,"assign") <- NULL
     } else {
      lis0 <- row.names(Xstar)[-lout]
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis <- unique(lis1)
      #lis <- names(lis2)[!lis2]
      idxo <- which(row.names(X)%in%unique(lis))
      Xoffrows <- X[idxo,,drop=FALSE]
 #     if(rational) Xoffrows <- rcdd::d2q(Xoffrows)
      yoffrows <- y[idxo]
      offrows <-  data.frame(Xoffrows,yoffrows)
      attr(offrows,"assign") <- NULL
      row.names(offrows) <- row.names(X)[idxo]
     }
  }
  colnames(offrows) <- c(colnames(X),"y")
  out <- list(offrows=offrows,index=idxo)
  out
}


#' @rdname seprows_osm
#' @export
detect_seprows_osm <- seprows_osm
