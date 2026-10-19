#' Voters in Ohio 
#'
#'  This is a cleaned-up and anonymized random snapshot (N=6544) of the data set used in Rusch, Lee, Hornik, Jank and Zeileis (2013).
#'
#' #' @format A data frame with 6544 rows and 83 variables:
#' \describe{
#'   \item{\code{id}}{Unique identifier of the person.}
#'   \item{\code{gender}}{Factor with 2 levels: \code{"F"} female, \code{"M"} male.}
#'   \item{\code{partyAffiliation}}{Factor with 3 levels: \code{"D"} Democrat, \code{"R"} Republican, \code{"U"} unaffiliated/independent.}
#'   \item{\code{partyMix}}{Factor with 8 levels describing household party composition: \code{"unknown"}, \code{"allR"} all Republican, \code{"allD"} all Democrats, \code{"noneRorD"} none Republican or Democrat, \code{"noneD"} none Democrat, \code{"onlyRorD"} only Republican or Democrat, \code{"noneR"} none Republican, \code{"allRorDorLegal"} all Republican, Democrat, or any other legal value.}
#'   \item{\code{householdHead}}{Factor with 2 levels: \code{"H"} household head, \code{"M"} household member.}
#'   \item{\code{householdRank}}{Ordered factor \code{"3+"} < \code{"2"} < \code{"1"}; rank in the household.}
#'   \item{\code{income}}{Factor with 12 levels indicating estimated household income: \code{"A"} under 15,000; \code{"B"} 15,000--24,999; \code{"C"} 25,000--34,999; \code{"D"} 35,000--49,999; \code{"E"} 50,000--74,999; \code{"F"} 75,000--99,999; \code{"G"} 100,000--124,999; \code{"H"} 125,000--149,999; \code{"I"} 150,000--174,999; \code{"J"} 175,000--199,999; \code{"K"} 200,000--249,999; \code{"L"} 250,000+.}
#'   \item{\code{homeOwner}}{Factor with 5 levels: \code{"1"} renter, \code{"A"} renter, \code{"B"} probable renter, \code{"C"} probable homeowner, \code{"D"} homeowner.}
#'   \item{\code{mailOrder}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; mail order buyer.}
#'   \item{\code{compOwner}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; owns a computer.}
#'   \item{\code{educationLevel}}{Factor with 7 levels \code{"A"}--\code{"G"} after ISCED (A=0, G=6); see \url{http://www.unesco.org/education/information/nfsunesco/doc/isced_1997.htm}.}
#'   \item{\code{donateReligion}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to religious causes.}
#'   \item{\code{donatePolitics}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to political causes.}
#'   \item{\code{donateArts}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to arts.}
#'   \item{\code{donateAnimals}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to animal causes.}
#'   \item{\code{donateChild}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to children's causes.}
#'   \item{\code{donateHealth}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to health causes.}
#'   \item{\code{donatePoverty}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to poverty causes.}
#'   \item{\code{donateEnvironment}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; donated to environmental causes.}
#'   \item{\code{fec_01_02}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 2001--2002.}
#'   \item{\code{fec_99_00}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 1999--2000.}
#'   \item{\code{fec_97_98}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 1997--1998.}
#'   \item{\code{fec_95_96}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 1995--1996.}
#'   \item{\code{fec_93_94}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 1993--1994.}
#'   \item{\code{fec_03_04}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 2003--2004.}
#'   \item{\code{fec_05_06}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; federal contributor in 2005--2006.}
#'   \item{\code{dontPhone}}{Factor with 3 levels: \code{"N"} no, \code{"U"} unknown, \code{"Y"} yes; on the federal do-not-call list.}
#'   \item{\code{ageD}}{Age in days.}
#'   \item{\code{cnt}}{Number of attended elections.}
#'   \item{\code{gen90}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1990.}
#'   \item{\code{pri90}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1990.}
#'   \item{\code{oth90}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1990.}
#'   \item{\code{gen91}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1991.}
#'   \item{\code{pri91}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1991.}
#'   \item{\code{oth91}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1991.}
#'   \item{\code{gen92}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1992.}
#'   \item{\code{ppp92}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended presidential primary in 1992.}
#'   \item{\code{oth92}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1992.}
#'   \item{\code{gen93}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1993.}
#'   \item{\code{pri93}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1993.}
#'   \item{\code{oth93}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1993.}
#'   \item{\code{gen94}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1994.}
#'   \item{\code{pri94}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1994.}
#'   \item{\code{oth94}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1994.}
#'   \item{\code{gen95}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1995.}
#'   \item{\code{pri95}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1995.}
#'   \item{\code{oth95}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1995.}
#'   \item{\code{gen96}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1996.}
#'   \item{\code{ppp96}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended presidential primary in 1996.}
#'   \item{\code{oth96}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1996.}
#'   \item{\code{gen97}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1997.}
#'   \item{\code{pri97}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1997.}
#'   \item{\code{oth97}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1997.}
#'   \item{\code{gen98}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1998.}
#'   \item{\code{pri98}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1998.}
#'   \item{\code{oth98}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1998.}
#'   \item{\code{gen99}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 1999.}
#'   \item{\code{pri99}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 1999.}
#'   \item{\code{oth99}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 1999.}
#'   \item{\code{gen00}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 2000.}
#'   \item{\code{ppp00}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended presidential primary in 2000.}
#'   \item{\code{oth00}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 2000.}
#'   \item{\code{gen01}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 2001.}
#'   \item{\code{pri01}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 2001.}
#'   \item{\code{oth01}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 2001.}
#'   \item{\code{gen02}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 2002.}
#'   \item{\code{pri02}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 2002.}
#'   \item{\code{oth02}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 2002.}
#'   \item{\code{pri03}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended primary election in 2003.}
#'   \item{\code{gen03}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 2003.}
#'   \item{\code{oth03}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 2003.}
#'   \item{\code{ppp04}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended presidential primary in 2004.}
#'   \item{\code{oth04}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended other election in 2004.}
#'   \item{\code{gen04}}{Factor with 2 levels: \code{"0"} no, \code{"1"} yes; attended general election in 2004.}
#'   \item{\code{cntEligible}}{Number of elections the person was eligible to vote in.}
#'   \item{\code{percentAttended}}{\code{cnt/cntEligible}.}
#'   \item{\code{ageY}}{Age in years (\code{ageD/365.25}).}
#'   \item{\code{pProb}}{Predicted probabilities from the \code{y~s|e} model.}
#'   \item{\code{pNodes}}{Predicted node number.}
#'   \item{\code{ageC}}{Factor with 4 levels; age category.}
#'   \item{\code{incC}}{Factor with 3 levels: \code{"< 35k"} less than 35,000 USD/year, \code{"35k-75k"} 35,001--75,000 USD/year, \code{">75k"} more than 75,000 USD/year.}
#'   \item{\code{eduC}}{Factor with 3 levels: \code{"primary"}, \code{"secondary"}, \code{"postsec"}; education categories (collapsed \code{educationLevel}).}
#'   \item{\code{pSegs}}{Factor with 7 levels; predicted segment number.}
#'   }
#'
#' @source Rusch, T., Lee, I., Hornik, K., Jank, W., & Zeileis, A. (2013). 
#'   Influencing elections with statistics: Targeting voters with logistic 
#'   regression trees. \emph{The Annals of Applied Statistics}, \strong{7}(3), 
#'   1612--1639. \doi{10.1214/13-AOAS648}
#' @name ohiovoters
#' @docType data
#' @keywords datasets
#' @usage data(ohiovoters)
NULL
