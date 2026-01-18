#' Creates a list of data for a sequence of binary models from a sequential (continuation ratio) model. It splits the data according to the forward sequential mechanism. 
#'
#' 
#' 
#' @param y the ordinal outcome variable with m categories. Works best if it is a factor but can also be numeric, boolean or character. In the later case we interpret the ordering as alphanumerically increasing (just like as.ordered is doing). 
#'
#' @param X a design matrix, e.g. generated via a call to 'model.matrix'. This means we expect that X already contains the desired contrasts for factors (e.g., dummies) and any other expanded columns (e.g., for polynomials).
#'
#' @return a list of length m-1 that separates the input into y and X tupels according to the sequential model mechanism. 
#'
#' @details The sequential mechanism is so that or category k (running from 1 to m-1) all i for which y_i>=k are selected. All y_i>k are given a value of 0 and y_i=k a value of 1. 
#'
create_bseq <-  function(y,X)
{
  #currently uses the forward formulation   
  y <- as.ordered(y)  
  cats <- levels(y)
  m <- nlevels(y)
  seqlist <- vector("list", (m-1)) 
  for(i in seq(1,(m-1)))
  {
    ind1 <- which(y>=cats[i]) #should work because we have an ordered factor
    yss <- y[ind1]
    xss <- X[ind1,]
    newy <- ifelse(yss==cats[i],1,0)
    seqlist[[i]] <- list("y"=newy,"X"=xss)
  }
  names(seqlist) <- cats[seq(1,(m-1),by=1)]
  seqlist
}
