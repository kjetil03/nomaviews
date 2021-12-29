

#' Update DTCC-data
#'
#' @import magrittr
#'
#' @export
#'
#' @examples
UpdateData <- function(start_date = "2021-09-01") {

  load("F:/MB/MOA/Likviditet/Analyser/Dataprosjekt/App/NoMaDataHub - Kjetil/data/historisk_data.Rda")

  new_data = GetNewIssueData(start_date)

  new_data = dplyr::anti_join(new_data, data, by = "SettlementDate")

  data = dplyr::bind_rows(data, new_data)

  return(data)


  }





