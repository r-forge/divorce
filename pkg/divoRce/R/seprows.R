#' Identify the rows in the design/structure vector matrix that cause separation.  It calls lower level functions if given an argument or chooses based on the response type.
#'
#' This function checks which rows in X/Xstar are responsible for separation on any category. The observations need not separate the same categories.
#
#' 
#' @param y the categorical outcome variable. Can be binary, categorial or ordinal. Works best if it is an ordered or unordered factor but can also be numeric, boolean or character. If y is not a factor, it is treated as a nominal (categorical) outcome. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param S a matrix of structure vectors. If given, \code{y} and \code{X} and \code{model} are ignored. 
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#'
#' @export
seprows<-function(y, X, S, rational=FALSE, model = c("bcl","b","cl","acl","sl","osm")){
 if(missing(S))
    {
    if(length(unique(y))<2) stop("There is only one value in y.")
    if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
    ratcols <- rat_cols(X)
    if(ratcols) rational <- TRUE 
    if(missing(model)) model <- NULL
    if(is.null(model))
    {
       warning("Default model class used.","\n")
       if(is.ordered(y) & length(unique(y))>2)
        {
            return(seprows_cl(y=y,X=X,rational=rational))
        } else {
            return(seprows_bcl(y=y,X=X,rational=rational))
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           b= seprows_b(y=y,X=X,rational=rational),
           bcl= seprows_bcl(y=y,X=X,rational=rational),
           cl= seprows_cl(y=y,X=X,rational=rational),
           acl=seprows_acl(y=y,X=X,rational=rational),       
           sl=seprows_sl(y=y,X=X,rational=rational),
           osm=seprows_osm(y=y,X=X,rational=rational)
           )
    } else {
        # for S given
        if(!is.matrix(S)) stop("S must be a matrix.")
        ratcols <- rat_cols(S)
        if(ratcols) rational <- TRUE
        if(ratcols) {
            # to turn a rational S into a rational Xstar we need to convert to floating and multiply with -1
            Stmp <- rcdd::q2d(S) 
            Xstar <- -1*Stmp
            Xstar <- rcdd::d2q(Xstar)
            #row.names(Xstar) <- row.names(S)
            #colnames(Xstar) <- colnames(S)
        } else {
            Xstar <- -1*S
        }
        lout <- linearities(S=S,rational=rational)$index # these are the structure vectors that are linearities
        if (length(lout)==0){
            # No structure vectors are linearities = complete separation and all rows are returned
            offrows <-  S
            idxo <-  seq(1,dim(Xstar)[1],by=1)
            attr(offrows,"assign") <- NULL
        } else {
               if (length(lout)==dim(Xstar)[1]){
                   # All structure vectors are linearities = overlap and no rows are returned
                   idxo <- integer(0)
                   offrows <-  S[idxo,]
                   attr(offrows,"assign") <- NULL
               } else {
                       # some structure vectors but not all are linearities = quasi-complete separation and all the rows that are non-linearities get returned
                       idxo <-  seq(1,dim(Xstar)[1],by=1)[-lout]
                       Soffrows <- S[idxo,,drop=FALSE]
                       offrows <- Soffrows
               }
        }
        colnames(offrows) <- colnames(S)
        out <- list(offrows=offrows,index=idxo)
        return(out)
   }
}


#' @rdname seprows
#' @export
detect_seprows <- seprows

#' Identify the rows in data matrix that cause separation in adjacent-category link ordinal response models.
#'
#' This function checks which rows in X (e.g. observations) are responsible for separation.  It does this over all categories, so the observations need not separate the same categories.
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
#' seprows_acl(y,X)
#' 
seprows_acl<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(row.names(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  Xstar <- acl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
  lout <- linearities_acl(y=y,X=X,rational=rational)$index
  idx <-seq(1,length(y),by=1)
  if (length(lout)==0){
#      if(rational) X <- rcdd::d2q(X)
      offrows <-  data.frame(X,y)
      idxo <- idx
      attr(offrows,"assign") <- NULL
  } else {
      if (length(lout)==dim(Xstar)[1]){
      # All structure vectors are linearities = overlap and no rowservations are returned
      idxo <- integer(0)
      offrows <-  data.frame(X,y)[idxo,]
          attr(offrows,"assign") <- NULL
      } else {
      #lis0 <- row.names(Xstar)[-lout]
      #lis1 <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis2 <- table(lis1)<(nlevels(y)-1)
      #lis <- names(lis2)[!lis2]
      #idxo <- which(row.names(X)%in%lis)
      lis0 <- row.names(Xstar)[-lout]
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis2 <- table(lis1)<(nlevels(y)-1)
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

#' @rdname seprows_acl
#' @export
detect_seprows_acl <- seprows_acl

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
  Xstar <- bcl_Xstar(y=y, X=X, label=TRUE, rational=rational)
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

#' Identify the rows in the data matrix that cause separation in baseline-category link categorical response models.
#'
#' This function checks which rows in X/Xstar are responsible for separation on any category. The observations need not separate the same categories.
#'
#' 
#' @param y the ordinal outcome variable. Works best if it is an ordered factor but can also be numeric, boolean or character. In the latter case we corece to ordered factor and interpret the ordering as alphanumerically increasing (just as as.ordered is doing).
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' 
#'
#' @return a list with elements: 
#' \itemize{
#' \item offrows the submatrix of the matrix (X,y) with the rows responsible for separation on any category.
#' \item index the index of the rows responsible for separation.
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
#' seprows_bcl(y,X)
#' 
seprows_bcl<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(row.names(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE
  if(!is.factor(y)) y <- as.factor(y)
  Xstar <- bcl_Xstar(y=y,X=X,label=TRUE,rational=rational) #for all nominal and binary
  lout <- linearities_bcl(y=y,X=X,rational=rational)$index # these are the structure vectors that are linearities
  if (length(lout)==0){
      # No structure vectors are linearities = complete separation and all rows are returned
      offrows <-  data.frame(X,y)
      idxo <- rn
      attr(offrows,"assign") <- NULL
  } else {
      if (length(lout)==dim(Xstar)[1]){
      # All structure vectors are linearities = overlap and no rows are returned
      idxo <- integer(0)
      offrows <-  data.frame(X,y)[idxo,]
          attr(offrows,"assign") <- NULL
      } else {
      # some structure vectors but not all are linearities = quasi-complete separation and all the rows that have at least one non-linearity get returned
      #lis0 <- row.names(Xstar)[lout]
      #lis1 <- unlist(strsplit(x=lis0,split="([.][^.]*)$"))
      #lis2 <- table(lis1)<(nlevels(y)-1)
      #lis <- names(lis2)[!lis2]
      #idxo <- which(row.names(X)%in%lis)
      lis0 <- row.names(Xstar)[-lout] #These are the struc vecs that are not linearities 
      lis <- unlist(strsplit(x=lis0,split="([.][^.]*)$")) #we remove the suffix of the category, so we have the rows names of the struc vecs that are not linearities TODO: Separate by category?
      idxo <- which(row.names(X)%in%unique(lis)) #now we select all the rowservations in X (row names) that had struc vecs that are not linearities
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


#' @rdname seprows_bcl
#' @export
detect_seprows_bcl <- seprows_bcl


#' Identify the rows in the data matrix that cause separation in cumulative link ordinal response models.
#'
#' This function checks which rows in the data matrix X are responsible for separation. It does this over all categories, so the observations need not separate the same categories.
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
#' @examples
#' data(qcsepdato)
#' y<-qcsepdato$y
#' X<-qcsepdato[,2:ncol(qcsepdato)]
#' seprows_cl(y,X)
#' 
seprows_cl<-function(y,X,rational=FALSE)
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  rn <- seq(1,dim(X)[1],by=1)
  if(is.null(rownames(X))) row.names(X) <- rn
  if(ratcols) rational <- TRUE
  y <- as.ordered(y)
  Xstar <- cl_Xstar(y=y,X=X,label=TRUE,rational=rational) #ordinal
  lout <- linearities_cl(y=y,X=X,rational=rational)$index
  idx <-seq(1,length(y),by=1)
  if (length(lout)==0){
       # No structure vectors are linearities = complete separation and all rows are returned
      offrows <-  data.frame(X,y)
      idxo <- idx
      attr(offrows,"assign") <- NULL
  } else {
      if (length(lout)==dim(Xstar)[1]) { 
      # All structure vectors are linearities = overlap and no rows are returned
      idxo <- integer(0)
      offrows <-  data.frame(X,y)[idxo,]
      attr(offrows,"assign") <- NULL
      } else {
      lis <- row.names(Xstar)[-lout]
      lis <- unlist(strsplit(x=lis,split="([.][^.]*)$")) #We need this if we relabel the cumulative Xstar as the other Xstars
      idxo <- which(row.names(X)%in%unique(lis))
      Xoffrows <- X[idxo,,drop=FALSE]
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

#' @rdname seprows_cl
#' @export
detect_seprows_cl <- seprows_cl

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
  Xstar <- osm_Xstar(y=y,X=X,label=TRUE,rational=rational) 
  lout <- linearities_osm(y=y,X=X,rational=rational)$index
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
  splitdat <- create_bseq(y=y,X=X)
  seqout <- lapply(splitdat,function(l) seprows_b(y=l$y,X=l$X,rational=rational))
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
