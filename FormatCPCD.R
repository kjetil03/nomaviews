#' Plots CP/CD on top of LIBOR
#'
#' Plots the interest rate of issued CP/CDs on
#' top of 3M Libor.
#' @param data Data set to plot.
#' @return Plot of 3M Libor with CP/CD transactions of
#' comparable maturity (80-100 days) on top.
#' @import dplyr readxl
#' @export
#' @examples
#' PlotCPCDAgainstLIBOR(FastLoadCPCD("2020-01-01", "2020-06-01"))
#' PlotCPCDAgainstLIBOR(dtcc_df)

EstimateIssueDateCPCD <- function(data) {

  # Mark the maximum time to maturity
  data <- data %>%
    group_by(CUSIP, MaturityDate) %>%
    mutate(EstIssued = max(TimeToMaturity)) %>%
    ungroup()
  data$EstIssued <- ifelse(data$TimeToMaturity == data$EstIssued, 1, 0)

  # Add estimated issue date if observation does not have an issue date
  data$EstIssueDate <- data$SettlementDate #ifelse(data$SettlementDate < as.Date("2021-07-01"), data$SettlementDate, data$IssueDate)

  return(data)
}


#' Adds maturity categories
#'
#' Adds a dummy indicating whether an CP/CD issuer is a bank.
#' @param data (CP/CD) data set.
#' @return Data set with column indicating whether issuer is a bank.
#' @import dplyr
#' @export

AddMaturityBuckets <- function(data) {

  data$Maturity <- cut(data$OriginalMaturity,
                       c(0, 10, 30, 60, 80, 100, 150, 250, 381, 550, 10000),
                       c("0-10 d", "11-25 d", "25-60 d", "60-80 d", "81-100 d", "101-150 d", "151-250 d", "251-380 d", "381-550 d", "550+ d"))

  #La til dette, siden koden failer for data der maturity date er absurd høy
  data = data[!is.na(as.character(data$Maturity)),]

  return(data)
}


#' Tags CP/CD issuers as bank/non-bank
#'
#' Adds a dummy indicating whether an CP/CD issuer is a bank.
#' @param data (CP/CD) data set.
#' @return Data set with column indicating whether issuer is a bank.
#' @import dplyr readxl
#' @export

TagBanksCPCD <- function(data) {

  issuers <- readxl::read_xlsx("F:/MB/MOA/Likviditet/Analyser/Dataprosjekt/App/NoMaDataHub - Kjetil/data/cpcd_issuers.xlsx")
  issuers$issuername <- trimws(issuers$issuername)
  issuers$issuername <- gsub("[[:space:]]", " ", issuers$issuername)

  data$IssuerName = trimws(data$IssuerName)

  data <- merge(data, issuers, by.x = "IssuerName", by.y = "issuername", all.x = TRUE)

  # Tag NIBOR banks
  nibor_banks_cpcd <- c("DNB BANK ASA", "DNB BANK ASA, NY BRANCH", # DNB
                        "NORDEA BANK ABP", "NORDEA BK AB (PUBL) NY", "NORDEA BK ABP NY", # Nordea
                        "SKANDIN ENS BANKEN", "SKANDIN ENS BANKEN AG", # SEB
                        "SVENSKA HANDLSBNKN AB", # Handelsbanken
                        "DANSKE BANK A/S", # Danske
                        "SWEDBANK (SPARBANK)", "SWEDBANK AB")

  # Remove leading/trailing white spaces from issuer names
  data$IssuerName <- trimws(data$IssuerName)
  data$NiborBank <- ifelse(data$IssuerName %in% nibor_banks_cpcd, "NIBOR bank", "Other")
  data$ParentName <- ifelse(is.na(data$ParentName), data$IssuerName, data$ParentName)
  data$ParentName <- data$ParentName <- gsub("[[:space:]]", " ", data$ParentName)
  data$ParentName <- trimws(data$ParentName)

  return(data)
}

#' Lists CP/CD issuers and country/sector codes
#'
#' Adds information about country and sector from
#' the newest data entries to the historical data.
#' @return List of issuers and country/sector.
#' @import dplyr data.table
#' @export

AddCodesCPCD <- function(data, update_info = TRUE) {

  if(update_info) {
    # Load info from SQL
    channel <- "driver={SQL Server};server=P-127-230-010\\PS010;database = NBDataHub;trusted_connection=true"
    connection <- odbcDriverConnect(channel, DBMSencoding = "ISO8859-1")
    sql_srch <- paste0("SELECT DISTINCT [IssuerName], [SectorCode], [CountryCode]
                      FROM [NBDataHub].[DTCC].[CPCDtransactions]")
    issuer_info <- as.data.frame(sqlQuery(connection, sql_srch))
    odbcClose(connection)

    # Drop if neither sector or country is given
    issuer_info <- issuer_info %>% filter(!(is.na(SectorCode) & is.na(CountryCode)))

    # Fix name formatting
    issuer_info$IssuerName <- trimws(issuer_info$IssuerName)

    # Make one entry per issuer
    issuer_info <- data.table::as.data.table(issuer_info) %>%
      data.table::melt(id.vars = "IssuerName") %>%
      data.table::dcast(`IssuerName` ~ variable,
                        value.var = "value",
                        fun.aggregate = function(x) {
                          trimws(x) %>%
                            unique() %>%
                            paste(collapse = "|")}) %>%
      rename(ListedCountryCodes = CountryCode, ListedSectorCodes = SectorCode)

    # Remove | from sectors
    issuer_info$ListedSectorCodes <- gsub("\\|", "", issuer_info$ListedSectorCodes)

    # Add not unique for country
    issuer_info$ListedCountryCodes <- gsub("\\|", "", issuer_info$ListedCountryCodes)
    issuer_info$ListedCountryCodes <- ifelse(nchar(issuer_info$ListedCountryCodes) > 3, "Multinational :)", issuer_info$ListedCountryCodes)


    # Save data set
    save(issuer_info, file="data/issuer_info_cpcd.RData")
  }

  # Add info to data set
  load("data/issuer_info_cpcd.RData")
  data <- merge(data, issuer_info, by = "IssuerName",  all.x = TRUE)

  return(data)
}

#' Formats CP/CD data
#'
#' Formats CP/CD data. Adds additional columns
#' that indicate (1) whether it is an issuance and
#' (2) whether the issuer is a bank.
#' @param data (CP/CD) data set.
#' @return Formatted CP/CD data.
#' @import dplyr readxl
#' @export

FormatDataCPCD <- function(data, update_info = FALSE) {

  # Fix name formatting
  data$IssuerName <- trimws(data$IssuerName)

  # Other formatting
  data <- EstimateIssueDateCPCD(data)
  data <- AddCodesCPCD(data, update_info = T)
  data <- TagBanksCPCD(data)
  data <- AddMaturityBuckets(data)

  return(data)
}




