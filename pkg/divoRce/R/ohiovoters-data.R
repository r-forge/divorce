#' Voters in Ohio 
#'
#'  This is a cleaned-up and anonymized random snapshot (N=6544) of the data set used in Rusch, Lee, Hornik, Jank and Zeileis (2013).
#'
#' @format A 6544 x 83 data frame including:  
#' * gender           : Factor w/ 2 levels "F" for female,"M" for male
#' * partyAffiliation : Factor w/ 3 levels "D" for Democrat,"R" for Republican,"U" for unaffiliated/independent
#' * partyMix         : Factor w/ 8 levels "unknown" for unkown,"allR" for all are Republican, "allD" all are Democr#'                      ats, "noneRorD" none are Republican or Democrat, "noneD" none are Democrat, "onlyRorD" only #'                      Republican or Democrat, "noneR" none are Republican, "allRorDorLegal" all are either Republi#'                      can or Democrats or any other legal value (e.g., other party affiliation; describes the hous#'                      ehold composition in terms of party affiliation
#' * householdHead    : Factor w/ 2 levels "H" for household head,"M" for household member
#' * householdRank    : Factor w/ 3 levels "3+" < "2" < "1" rank in the household
#' * income           : Factor w/ 12 levels Level of estimated household income from census or estimated using demog#'                      raphic and census data.  "A" - Under 15,000;  "B" - 15,000-24,999; "C" - 25,000-34,999; "D" #'                      - 35,000-49,999; "E"- 50,000-74,999; "F" - 75,000-99,999; "G" - 100,000-124,999; "H" - 125,0#'                      00-149,999; "I" - 150,000-174,999; "J" - 175,000-199,999; "K" - 200,000-249,999; "L"- 250,00#'                      0+
#' * homeOwner        : Factor w/ 5 levels "1" renter,"A" renter,"B" probable renter,"C" probable homeowner,"D" home#'                      owner (note: it is not clear how 1 is different from A)
#' * mailOrder        : Factor w/ 2 levels "0" no ,"1" yes; indicates mail order buyer
#' * compOwner        : Factor w/ 2 levels "0"no ,"1" yes; owns a computer
#' * educationLevel   : Factor w/ 7 levels "A","B","C","D","E","F","G" after ISCED (http://www.unesco.org/education/#'                      information/nfsunesco/doc/isced_1997.htm; A=0 and G=6)
#' * donateXXX     : Factor w/ 2 levels "0" no,"1" yes, donated to cause XXX (arts, politics, children, animals, hea#'                   lth, poverty, environment, religion)
#' * fec_XX_YY        : Factor w/ 2 levels "0" for no,"1" for yes; federal contributor in years XX and YY
#' * dontPhone        : Factor w/ 3 levels "N" for no ,"U" for unknown,"Y" for yes; person is on the federal do not call list
#' * ageD             : age in days
#' * cnt              : number of attended elections
#' * genXX            : Factor w/ 2 levels "0" for no,"1" for yes; attended general elections in year XX
#' * priXX            : Factor w/ 2 levels "0" for no,"1" for yes; attended primary elections in year XX
#' * othXX            : Factor w/ 2 levels "0" for no,"1" for yes; attended other elections in year XX
#' * pppXX            : Factor w/ 2 levels "0" for no,"1" for yes; attended presidential primary elections in year XX (is the priXX variable for years with presidential elections)
#' * cntEligible      : Number of election the person was eligible to vote
#' * percentAttended  : cnt/cntEligible
#' * ageY             : age in years (ageD/365.25)
#' * pProb            : predicted probabilities from the y~s|e model
#' * pNodes           : predicted node number
#' * ageC             : Factor w/ 4 levels; age category
#' * incC             : Factor w/ 3 levels "< 35k" less than 35,000 USD a year, "35k-75k" 35,001 to 75,000 USD per y#'                      ear ">75k more than 75,000 USD per year"; income categories
#' * eduC             : Factor w/ 3 levels "primary" primary education,"secondary" secondary education,"postsec" pos#'                      tsecondary eucation; education categories (collapsed educationLevel)
#' * pSegs            : Factor w/ 7 levels; predicted segment number
#'
#'
#' @source Rusch, Lee, Hornik, Jank, Zeileis (2013). Influencing elections with statistics: Targeting voters with logistic regression trees. The Annals of Applied Statistics, 7(3), pp. 1612-1639. doi: 10.1214/13-AOAS648
#' @name ohiovoters
#' @docType data
#' @keywords datasets
#' @usage data(ohiovoters)
NULL
