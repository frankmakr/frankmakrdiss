#' Samples of the studies
#'
#' @description
#' The sample characteristics
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
#'   \item{gender}{Gender coded as male = 1, female = 2, divers = 3}
#'   \item{age}{Age in years}
#'   \item{state}{One of the 16 German states the person is living
#'     in alphabetical order
#'     or 17 = living outside from Germany}
#'   \item{studstatus}{Student status used in the FernUniversität
#'     official statistics}
#'   \item{noexpert}{Knowledge of a theory of psychological sense of community
#'     coded as 0 = Knowledge, 1 = No knowledge}
#'   \item{numcomm}{The number of communities the person mentioned}
#' }
"conval_demo"

#' Classification scheme of communities
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

#' Phrases of the communities in the dataset conval_data
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

#' Content validity of tests for psychological sense of community
#'
#' @description
#' The content validity of the items of 4 different tests
#' was rated in specific community contexts.
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
"conval_data"

