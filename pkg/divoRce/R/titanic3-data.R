#' All people of age 3 in the Kaggle Titanic data set
#'
#'  This data set contains the subset of people in the Titanic data sets that were aged 3. It shows various separation types depending on the model fitted.  
#' 
#' @format A 6 x 12 data frame:  
#' \describe{
#'   \item{passengerId}{ID of the passenger}
#'   \item{Survived}{Survived? (1=yes, 0=no)}
#'   \item{Pclass}{Passenger Class (1=upper, 2=middle, 3=lower).} 
#'   \item{Name}{Name.}
#'   \item{Sex}{Sex of the person.}
#'   \item{Age}{Age of the person. All are 3 yrs.} 
#'   \item{SibSp}{Number of siblings or spouses on board the ship.}
#'   \item{Parch}{Number of parents or children on board the ship.}
#'   \item{Ticket}{Ticket number}.
#'   \item{Fare}{Passenger fare.}
#'   \item{Cabin}{Cabin number.}
#'   \item{Embarked}{Port of embarkation (C=Cherbourg, Q=Queenstown, S=Southampton).}
#' }
#'
#' @source Will Cukierski. Titanic - Machine Learning from Disaster. https://kaggle.com/competitions/titanic, 2012. Kaggle.
#' @name titanic3
#' @docType data
#' @keywords datasets
#' @usage data(titanic3)
#' @examples
#' with(titanic3,ftable(Survived~Sex+Pclass)) 

NULL
