#' Psychiatric Cases Classification based on GHQ of Silvapulle (1981) 
#'
#'  These data are from a psychiatric study of for 120 patients on the relation between the value of the score on the 12-item General Health Questionnaire (GHQ) and classification as a psychiatric case ("yes" or "no"). The patients attended as general practitioner's surgery and were administered the GHQ, resulting in a theoretical score between 0 and 12 (there were no cases or non-cases with GHQ scores of 11 or 12). Subsequently the patinets were given a full psychiatric examination by a psychiatrist who did not know the patient's GHQ score.  The patient was classified by the psychiatrist as either a clinical case requiring psychiatric treatment (case="yes"), or as non-clinial (case="no"). The biological sex of the patients was also recorded.
#'
#' @format A 120 x 3 data frame:  
#' \describe{
#'   \item{sex}{A factor with levels 'male' and 'female'.}
#'   \item{ghq}{The GHQ score on a scale from 0 to 12 (numeric).}
#'   \item{case}{Diagnosis as a clinical case ('yes') or not ('no').} 
#' }
#'
#' @source Silvapulle, M. J. (1981), "On the existence of maximum likelihood estimators for the binomial response model", J. Roy. Statist. Soc. B., 43, 310-313.
#' @name Silvapulle
#' @docType data
#' @keywords datasets
#' @usage data(Silvapulle)
NULL
