#' Identify the observations that cause separation in ordered stereotype models.
#'
#' This function checks which observations are responsible for separation on any category. 
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we corece to ordered factor and interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' 
#'
#' @return a list with elements: 
#' \itemize{
#' \item offobs the submatrix of the matrix (X,y) with the observations responsible 
#' \item index the index of the separated observations  
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
#' sepobs_osm(y,X)
#' 
sepobs_osm<-function(y,X,rational=FALSE)
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
      offobs <-  data.frame(X,y)
      idxo <- idx
      attr(offobs,"assign") <- NULL
  } else {
     if (length(lout)==dim(Xstar)[1]){
      # All structure vectors are linearities = overlap and no observations are returned
         idxo <- integer(0)
         offobs <-  data.frame(X,y)[idxo,]
         attr(offobs,"assign") <- NULL
     } else {
      lis0 <- row.names(Xstar)[-lout]
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis <- unique(lis1)
      #lis <- names(lis2)[!lis2]
      idxo <- which(row.names(X)%in%unique(lis))
      Xoffobs <- X[idxo,,drop=FALSE]
 #     if(rational) Xoffobs <- rcdd::d2q(Xoffobs)
      yoffobs <- y[idxo]
      offobs <-  data.frame(Xoffobs,yoffobs)
      attr(offobs,"assign") <- NULL
      row.names(offobs) <- row.names(X)[idxo]
     }
  }
  colnames(offobs) <- c(colnames(X),"y")
  out <- list(offobs=offobs,index=idxo)
  out
}


#' @rdname sepobs_osm
#' @export
detect_sepobs_osm <- sepobs_osm
