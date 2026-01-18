#' General separation check.
#'
#' This function checks for (quasi-) complete separation. It is high level wrapper to check_sep and takes a formula and data combination, a model object or a response y and a design matrix X.  
#'
#' @param arg1 first argument. Either a formula, or a model object or the outcome variable.
#' @param arg2 second argument. Needs to be a data frame if arg1 is a formula, nothing if arg1 is a model, or a design matrix if arg1 is the outcome variable.  
#' @param ... additional arguments passed to model.frame() or model.matrix() if formula and data are given.
#'
#' @return a character string; either "Separation" if we detected separation or "Overlap" if not.
#'
#' @export
#'
#' @importFrom stats model.frame model.matrix terms
#'
#' @examples
#' data(csepdat1)
#' y<-csepdat1$y
#' X<-csepdat1[,2:ncol(csepdat1)]
#' 
#' #y, X version (pre fit/no fit)
#' check_separation(y,X) 
#'
#' #formula version (pre fit/no fit)
#' check_separation(y~x1+x2,csepdat1) 
#'
#' #object version (post fit)
#' m1<-glm(y~x1+x2,data=csepdat1,family=binomial())
#' check_separation(m1)
#'
## check_separation <- function(arg1=NULL, arg2=NULL, ...)
##   ## ... for model.frame and model.matrix extra args  
## {
##  ## Set up the design matrix and the outcome for the different argument cases   
##  if(inherits(arg1,"formula")){
##    ## TODO: I use the model.frame -> model.matrix sequence as in the man pages. Ask Kurt whether we should do/need that.
##      m <- model.frame(arg1,data=arg2, ...)
##      X <- model.matrix(arg1, m, ...)
##      y <- m[,attr(terms(m),"response")]
##   }   
##   if(any(class(arg1))%in%c("glm","clm")) { #TODO: works with glm and clm atm; check with other model classes, e.g. clm, mlogit, multinom.
##       X <- model.matrix(arg1)
##       y <- arg1$y #TODO: works with glm; 
##   }
##   if(any(class(arg1))%in%c("multinom")) { #TODO: doesn't work with glm atm
##       stop("I can't do this for objects of this class as they do not return the response in the object. Supply the formula and data or y and X separately.")
##      # X <- model.matrix(arg1)
##      # y <- [,attr(terms(arg1),"response")]  #TODO: Can't get the y from the object. Maybe in $terms somewhere? Ask Kurt. 
##   }
##    if(any(class(arg1))%in%c("mlogit")) { 
##      X <- model.matrix(arg1)
##      y <- arg1$model[[1]] ##looks like the response is always the first element in $model. It is binary here so works fine 
##    }
##    if(is.null(arg1)) stop("We need a dependent variable y, a formula and data, or a model object with the response in slot $y.")
##    if(is.null(arg2)) stop("We need a design matrix X, a formula and data, or a model object with a model.matrix() method.")
##  ## We currently have order y,X < formula < model. Better the other way round?
##    out <- check_sep(y, X)
##    names(out) <- "Separation"
##    return(out)
## }



