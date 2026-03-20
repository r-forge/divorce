#' Print generic for sepmod classes
#'
#' @param x object of class sepmod
#' @param info how much info is printed. Possibilities are 'minimal' (default) and 'full'.
#' @param ... additional arguments
#' @export
print.sepmod <- function(x, info = c("minimal","full"),...)
{
     obj <- x
     if(missing(info)) info <- "minimal"   
     cat("Separation Check for Model:","\n")
     print(obj$modelcall)
     cat("Model Class:", switch(obj$modelclass,
                                b = "Binary",
                                bcl = "Baseline-Category",
                                cl = "Cumulative",
                                acl = "Adjacent-Category",
                                sl = "Sequential",
                                osm = "Ordered Stereotype")
       , "\n")
     cat("\n")
     cat("Separation:", obj$separation, "\n")
     cat("Separation Type:", obj$septype,"\n")
     if(info=="full")
     {
     cat("\n")    
     cat("Dimension of Recession Cone:", obj$reccdim,"\n")
     cat("Number of Offending Columns:", length(obj$offcols),"\n")
     cat("Number of Offending Rows:", obj$nr.offrows,"\n")
     }
}

#' Print generic for sepmod_sl classes
#'
#' @param x object of class sepmod
#' @param info how much info is printed. Possibilities are 'minimal' (default) and 'full'.
#' @param ... additional arguments
#' @export
print.sepmod_sl<- function(x, info = c("minimal","full"),...)
{
     obj <- x
     if(missing(info)) info <- "minimal"
     cat("Separation Check for Model:","\n")
     print(obj[[1]]$modelcall)
     cat("Model Class: Sequential","\n")
     cat("\n")
     for(i in 1:(length(obj))){
     #cat(rep("-",30),"\n")
     cat("\n")        
     cat("CATEGORY ",names(obj)[i],"\n")  
     cat("Separation:", obj[[i]]$separation, "\n")
     cat("Separation Type:", obj[[i]]$septype,"\n")
     if(info=="full")
     {
     cat("\n")        
     cat("Dimension of Recession Cone:", obj[[i]]$reccdim,"\n")
     cat("Number of Offending Columns:", length(obj[[i]]$offcols),"\n")
     cat("Number of Offending Rows:", obj[[i]]$nr.offrows,"\n")
     }    
    }
}
