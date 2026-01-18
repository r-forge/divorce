#' Detailed separation diagnostic for all categorical outcomes. 
#'
#' This function checks whether there is (quasi-) complete separation, which type if any, gives the dimension of the recession cone, lists the number of columns in the design matrix that give rise to the separation as well as the columns names and lists the observations for which we have separation.   
#'
#' 
#' @param y the outcome variable. Can be binary, categorial or ordinal. Works best if it is an ordered or unordered factor but can also be numeric, boolean or character. If y is not a factor, it is treated as a nominal (categorical) outcome. 
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#' @param rational should rational arithmetic be used?
#' @param model what model class is intended to be fitted? Can be any of "b" for binary, "bcl" for baseline-category link, "cl" for cumulative link, "acl" for adjacent-category link. "sl" for sequential link, "osm" for ordered stereotype model. If missing it defaults to cumulative link for ordinal y and baseline-category for everything else.  
#'
#' @return an object of class 'sepmod' that is a list with the components:
#' \itemize{
#' \item separation boolean whether there is separation ('TRUE' means separation)
#' \item septype which type of separation (or not). A string of either "Overlap", "Quasi-Complete Separation" or "Complete Separation".
#' \item reccdim dimension of recession cone
#' \item offobs offending observations 
#' \item nr.offcols number of columns of the design matrix that have separation
#' \item offcols columns of the design matrix that have separation. It is given as category::effect.  
#' }
#' 
#' @export
#'
#' @examples
#' data(qcsepdatm)
#' y<-qcsepdatm$y
#' X<-cbind(1,qcsepdatm[,2:ncol(qcsepdatm)])
#' diagsep(y,X,model="bcl")
#' 
diagsep<-function(y,X,rational=FALSE, model=c("bcl","cl","acl","sl","osm"))
{
  if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
  ratcols <- rat_cols(X)
  if(ratcols) rational <- TRUE
  if(length(unique(y))<2) stop("There is only one value in y.") 
  if(missing(model)) model <- NULL
    if(is.null(model))
    {
        warning("I'm not sure which model you want to fit, so I default to the most common ones.","\n")
        if(is.ordered(y) & length(unique(y))>2)
        {
            diagsep_cl(y,X,rational=rational)
        } else {
            diagsep_bcl(y,X,rational=rational)
        }
    }
    model <- match.arg(model,several.ok=FALSE)
    switch(model,
           bcl= diagsep_bcl(y,X,rational=rational),
           cl= diagsep_cl(y,X,rational=rational),
           acl= diagsep_acl(y,X,rational=rational),       
           sl=diagsep_sl(y,X,rational=rational),
           osm=diagsep_osm(y,X,rational=rational)
           )
 }



## diagsepOLD<-function(y,X,rational=FALSE)
## {
##   if(!isTRUE(all.equal(length(y),dim(X)[1]))) stop("The length of vector y does not match the number of rows in matrix X.")
##   ratcols <- rat_cols(X)
##   if(ratcols) rational <- TRUE
##   if(is.ordered(y) & length(unique(y))>2) { #TODO: Do we have any other way to check whether y is ordinal?
##         Xstar <- cl_Xstar(y, X, label=TRUE, rational=rational) # for ordinal
##     } else {
##         Xstar <- bcl_Xstar(y, X, label=TRUE, rational=rational) #for all nominal and binary
##   }
##   lout <- linearities(y,X,rational=rational)$index
##   if(ratcols(Xstar)) Xstar <- rcdd::q2d(Xstar)
##   if (length(lout)==0){
##       offobs <-  Xstar
##       attr(offobs,"assign") <- NULL
##     } else {
##       offobs <- Xstar[-lout,,drop=FALSE]
##       attr(offobs,"assign") <- NULL
##     }
##   typ<-ifelse(length(lout)>0,ifelse(length(lout)==dim(Xstar)[1],"Overlap","Quasi-Complete Separation"),"Complete Separation")
##   reccdim <- reccone(y,X,rational=rational)$reccdim
##   offcols <- detect_sepcols(y,X,rational=rational)$offcols 
##   out <- list(separation=(typ!="Overlap"),septype=typ,nr.offobs=dim(offobs)[1],reccdim=reccdim,offobs=offobs,nr.offcols=length(offcols),offcols=offcols)
##   class(out) <- out$class <- "sepmod"
##   out
## }
