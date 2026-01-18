#' Identify the observations that cause separation in baseline-category link categorical response models.
#'
#' This function checks which observations are responsible for separation on any category. The observations need not separate the same categories.
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we corece to ordered factor and interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' 
#'
#' @return a list with elements: 
#' \itemize{
#' \item offobs the submatrix of the matrix (X,y) with the observations responsible for separaton on in any category.
#' \item index the index of the observations.
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
#' sepobs_bcl(y,X)
#' 
sepobs_bcl<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(row.names(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE
  if(!is.factor(y)) y <- as.factor(y)
  Xstar <- bcl_Xstar(y,X,label=TRUE,rational=rational) #for all nominal and binary
  lout <- linearities_bcl(y,X,rational=rational)$index # these are the structure vectors that are linearities
  if (length(lout)==0){
      # No structure vectors are linearities = complete separation and all observations are returned
      offobs <-  data.frame(X,y)
      idxo <- rn
      attr(offobs,"assign") <- NULL
  } else {
      if (length(lout)==dim(Xstar)[1]){
      # All structure vectors are linearities = overlap and no observations are returned
      idxo <- integer(0)
      offobs <-  data.frame(X,y)[idxo,]
          attr(offobs,"assign") <- NULL
      } else {
      # some structure vectors but not all are linearities = quasi-complete separation and all the obs that have at least one non-linearity get returned
      #lis0 <- row.names(Xstar)[lout]
      #lis1 <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis2 <- table(lis1)<(nlevels(y)-1)
      #lis <- names(lis2)[!lis2]
      #idxo <- which(row.names(X)%in%lis)
      lis0 <- row.names(Xstar)[-lout] #These are the struc vecs that are not linearities 
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$")) #we remove the suffix of the category, so we have the obs names of the struc vecs that are not linearities TODO: Separate by category?
      idxo <- which(row.names(X)%in%unique(lis)) #now we select all the observations in X (row names) that had struc vecs that are not linearities
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


#' @rdname sepobs_bcl
#' @export
detect_sepobs_bcl <- sepobs_bcl
