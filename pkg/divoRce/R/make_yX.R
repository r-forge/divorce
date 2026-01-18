#' This function is meant to set up a response variable and a design matrix from a formula-data combination for the pre-fit separation check functions.
#'
#' @details For standard data frames and formulas this function returns a list with the first element being the response variable (as specified by the left hand side of the formula) and the second element being a design matrix corresponding to the right hand side of the formula and the given optional contrasts. The design matrix is created in exactly the same way as in \code{\link[stats]{glm}}. For a data frame/matrix given as rational numbers in the rcdd definition, it returns a list with the first element being the response variable in rational format (as specified by the left hand side of the formula) and the second element being the character matrix in rational format corresponding to the right hand side of the formula. Note that in the latter the formula does not get expanded and is taken literally, so e.g. variables in formula must match exactly with the column names in data, or factors need to be converted to dummies before that (wouldn't be possible in the rational format in any other way anyway).  
#'
#' @param formula An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted.  The details of model specification are given under ‘Details’ in \code{\link[stats]{glm}}.
#' @param data Either a standard data frame, list or environment (or object coercible by as.data.frame to a data frame) containing variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which \code{make\_yx} is called. Alternatively, data can be a data frame or matrix containing rational numbers as per the definition in \code{rcdd} (i.e. columns are characters, the entries are either integer numbers or ratios of integer numbers, e.g. "1", or "-234/19008". This is checked internally; see the Details for what happens when this structure is discovered,    
#' @param contrasts contrasts: an optional list. See the  \code{contrasts.arg} of \code{model.matrix.default}. Only effective for standard data frames.
#'
#' @return A list with two elements, $y being the response variable (as specified by the left hand side of the formula) and the second element $X being a design matrix corresponding to the right hand side of the formula.
#'
#' @examples
#' ## standard data frame
#' data(nsduh2019)
#' frml <- her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex
#' mk <- make_yx(frml,nsduh2019)
#' str(mk)
#' 
#' ## rational structure
#' yr <- as.character(as.numeric(mk$y))
#' Xr <- apply(mk$X,2,as.character)
#' datr1 <- data.frame(yr,Xr) #data frame with rational data
#' frmlr1 <- yr~X.Intercept.+ alc_agefirst + demog_sexFemale
#' mkr1 <- make_yx(frmlr1,datr1)
#' str(mkr1)
#' 
#' datr2 <- cbind(yr,Xr) #character matrix with rational data  
#' frmlr2 <- yr~(Intercept) + alc_agefirst #note the intercept column is labeled differently here 
#' mkr2 <- make_yx(frmlr2,datr2)
#' str(mkr2)
#'
#' @importFrom stats model.response is.empty.model model.matrix
#' 
#' @export
make_yx <- function(formula, data, contrasts=NULL)
{
 ## if we get the data as rational arithmetic we do not set up a model.matrix and just create the two objects.
   if(rat_cols(data)) {
        frmlc <- as.character(formula)
        yvar <- frmlc[2]
        Xvars1 <- strsplit(frmlc[3]," + ",fixed=TRUE)
        Xvars2 <- gsub(" ", "", Xvars1[[1]])
        y <- data[,yvar]
        X <- data[,Xvars2]
        X <- as.matrix(X)
        if(is.data.frame(data)) attr(X,"dimnames")[[1]] <- row.names(data)
        if(is.matrix(data) && !is.null(attr(X,"dimnames")[[1]])) attr(X,"dimnames")[[1]] <- attr(data,"dimnames")[[1]]
        out <- list(y=y,X=X)
        return(out)
    }
#this is just like in glm
mf <- match.call(expand.dots = FALSE)
m <- match(c("formula", "data"), names(mf), 0L) #which ones to use? only data, formula, subset, na.action. 
mf <- mf[c(1L, m)]
mf$drop.unused.levels <- TRUE
mf[[1L]] <- quote(stats::model.frame)
mf <- eval(mf, parent.frame())
mt <- attr(mf, "terms")
y <- model.response(mf, "any")
if (length(dim(y)) == 1L) {
        nm <- rownames(y)
        dim(y) <- NULL
        if (!is.null(nm)) 
            names(y) <- nm
}
X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts)
else matrix(, NROW(y), 0L)
attr(X,"contrasts") <- NULL
attr(X,"assign") <- NULL
out <- list(y=y,X=X)
out   
}


