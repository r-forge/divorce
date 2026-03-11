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
     cat("Number of Offending Columns:", length(obj$offcols),"\n")
     cat("Number of Offending Rows:", obj$nr.offrows,"\n")
     #}
     #if(info=="full")
     #{    
     cat("Dimension of Recession Cone:", obj$reccdim,"\n")
     cat("\n")
     cat("Number of Offending Columns:", length(obj$offcols),"\n")
     cat("Offending Columns:","\n") 
     print(obj$offcols)
     cat("\n")
     cat("Number of Offending Rows:", obj$nr.offrows,"\n")
     cat("Offending Rows:","\n")
     print(obj$offrows)
     #}
     }

