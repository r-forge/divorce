#' Summary generic for sepmodb classes
#'
#' @param object object of class sepmodb
#' @param ... additional arguments
#' @export
summary.sepmodb <- function(object,...)
{
     obj <- object
     #if(missing(info)) info <- "minimal"    
     cat("Separation Check for Model:","\n")
     print(obj$model$call)
     cat("\n")
     cat("Separation:", obj$separation, "\n")
     cat("Separation Type:", obj$septype,"\n")
     #if(info=="partial")
     #{    
     cat("Dimension of Recession Cone:", obj$reccdim,"\n")
     cat("\n")
     cat("Number of Offending Variables:", length(obj$offvars),"\n")
     cat("Number of Offending Observations:", obj$nr.offobs,"\n")
     #}
     #if(info=="full")
     #{    
     cat("Dimension of Recession Cone:", obj$reccdim,"\n")
     cat("\n")
     cat("Number of Offending Variables:", length(obj$offvars),"\n")
     cat("Offending Variables:","\n") 
     print(obj$offvars)
     cat("\n")
     cat("Number of Offending Observations:", obj$nr.offobs,"\n")
     cat("Offending Observations:","\n")
     print(obj$offobs)
     #}
     }

