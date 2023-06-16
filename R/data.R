#' Sample Characteristics
#'
#' @description
#' The characteristics of the samples for the studies.
#'
#' @format
#' ## `comms_demo`
#' A data frame with 158 rows and 7 columns.
#' @source Primary data
#' @name samples
#' @aliases samples comms_demo conval_demo
"comms_demo"

#' @rdname samples
#' @format
#' ## `conval_demo`
#' A data frame with 1,465 rows and 7 columns.
#' ## Variables
#' \describe{
#'   \item{person}{Person number}
#'   \item{gender}{Gender coded as 1 = Male, 2 = Female, 3 = Divers}
#'   \item{age}{Age in years}
#'   \item{state}{One of the 16 German states the person is living
#'     in alphabetical order
#'     or 17 = living outside from Germany}
#'   \item{studstatus}{Student status used in the FernUniversität
#'     official statistics ([Berichtswesen, 2022a, 2022b][source])}
#'   \item{noexpert}{Knowledge of a theory of psychological sense of community
#'     coded as 0 = Knowledge, 1 = No knowledge}
#'   \item{numcomm}{The number of communities the person mentioned}
#' }
#' @source 
#' Berichtswesen. (2022a, March 25).
#' *Studierendenstatistik für Sommersemester 2018*
#' \[Student statistics for summer semester 2018\].
#' FernUniversität in Hagen.
#' <https://www.fernuni-hagen.de/uniintern/organisation/statistik/semesterstatistik/sose2018.shtml>.
#' 
#' Berichtswesen. (2022b, April 8).
#' *Studierendenstatistik für Wintersemester 2019/20*
#' \[Student statistics for winter semester 2019/20\].
#' FernUniversität in Hagen.
#' <https://www.fernuni-hagen.de/uniintern/organisation/statistik/semesterstatistik/wise2019-20.shtml>.
"conval_demo"

#' Classification Scheme of Communities
#'
#' @description
#' Phrases for the communities in German
#' where persons have experienced themselves sense of community.
#'
#' @format
#' ## `communities`
#' A data frame with 1,580 rows and 6 columns.
#' ## Variables
#' \describe{
#'   \item{person}{Person number from \code{comms_demo}}
#'   \item{field}{Which of the 10 fields was used for the phrase}
#'   \item{k_start}{The phrases before the classification process}
#'   \item{k_iter1}{The phrases after iteration 1}
#'   \item{k_iter2}{The phrases after iteration 2}
#'   \item{k_iter3}{The phrases after iteration 3}
#' }
#' @source Primary data
"comms_data"

#' Community phrases for the Dataset conval_data
#'
#' @description
#' The phrases of the communities in German the persons could choose from
#'
#' @format
#' ## `commnames`
#' A data frame with 22 rows and 2 columns.
#' ## Variables
#' \describe{
#'   \item{long}{The original phrases used in the study material}
#'   \item{short}{Short phrases used in text, tables, and graphs}
#' }
#' @source Primary data
"conval_comms"

#' Content Validity of Tests for Psychological Sense of Community
#'
#' @description
#' The rated content validity of the items of 4 different tests
#' in specific community contexts.
#'
#' @format
#' ## `contentvalidity`
#' A data frame with 5,609 rows and 79 columns.
#' ## Variables
#' \describe{
#'   \item{person}{Person number from \code{conval_demo}}
#'   \item{comm}{The specific community from \code{conval_comms}
#'     for which the test items were rated}
#'   \item{value}{The grade to which the community is a
#'     community of shared values}
#'   \item{purpose}{The grade to which the community is a
#'     community of purpose}
#'   \item{value_x_purpose}{Interaction variable}
#'   \item{chavis_\[1-25\]}{The grade to which the test item \[1-25\] from
#'     Chavis et al. (2008) is necessary to characterize sense of community}
#'   \item{omoto_\[1-18\]}{The grade to which the test item \[1-18\] from
#'     Omoto and Snyder (2010) is necessary to characterize sense of community}
#'   \item{jason_\[1-9\]}{The grade to which the test item \[1-9\] from
#'     Jason et al. (2015) is necessary to characterize sense of community}
#'   \item{halamova_\[1-22\]}{The grade to which the test item \[1-22\] from
#'     Halamová et al. (2018) is necessary to characterize sense of community}
#' }
#' @source Primary data
#' @references
#' Chavis, D. M., Lee, K. S., & Acosta, J. D. (2008).
#' *The Sense of Community Index (SCI) revised:*
#' *The reliability and validity of the SCI-2.*
#' Paper presented at the 2nd International Community Psychology Conference.
#' Lisboa, Portugal.
#' Retrieved October 17, 2018, from <https://senseofcommunity.com>.
#' 
#' Halamová, J. Kanovsky, M., & Naništová, E. (2018).
#' Development and psychometric analysis of the
#' sense of community descriptors scale.
#' *Psychological Intervention, 27(1), 44-55.*
#' <https://doi.org/10.5093/pi2018a8>.
#' 
#' Jason, L. A., Stevens, E., & Ram, D. (2015).
#' Development of a three-factor psychological sense of community scale.
#' *Journal of Community Psychology, 43(8), 973-985.*
#' <https://doi.org/10.1002/jcop.21726>.
#' 
#' Omoto, A. M., & Snyder, M. (2010).
#' Influences of psychological sense of community
#' on voluntary helping and prosocial action.
#' In S. Stürmer & M. Snyder (Eds.),
#' *The psychology of prosocial behavior:*
#' *Group processes, intergroup relations, and helping (pp. 223-243).*
#' Wiley-Blackwell.
#' <https://doi.org/10.1002/9781444307948.ch12>.
"conval_data"

#' Distance Matrices of Community Effects
#'
#' @description
#' The Matrices contain the pairwise energy distances
#' of the distributions of community effects in the ratings.
#' The item ratings are aggregated at the test and the dimension level.
#'
#' @format
#' ## `chavis_dist`
#' A named list of length 6 containing 22 x 22 distance matrices.
#' @source Primary data.
#' @name distmat
#' @aliases distmat chavis_dist omoto_dist jason_dist halamova_dist
#' @inherit conval_data references 
"chavis_dist"

#' @rdname distmat
#' @format
#' ## `omoto_dist`
#' A named list of length 10 containing 22 x 22 distance matrices.
"omoto_dist"

#' @rdname distmat
#' @format
#' ## `jason_dist`
#' A named list of length 4 containing 22 x 22 distance matrices.
"jason_dist"

#' @rdname distmat
#' @format
#' ## `halamova_dist`
#' A named list of length 4 containing 22 x 22 distance matrices.
"halamova_dist"
