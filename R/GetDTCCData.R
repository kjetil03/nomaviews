
#' Get CPCD-data from DTCC
#'
#'
#' @importFrom  dplyr filter mutate left_join rename select if_else group_by
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @import magrittr
#'
#' @export
#'
#'
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#' dtcc_data = GetDTCCData()
#' }
#' 
GetDTCCData <- function() {
  
  
  sql_srch = "set nocount on

  SELECT  a.[CUSIP]
      ,[MaturityDate]
	  ,max(datediff(day, settlementdate, maturitydate)) as max_maturity
	  into #maxmaturities2
  FROM [NBDataHub].[DTCC].[CPCDtransactions] as a 
  group by a.cusip, MaturityDate
			 
  select a.[CUSIP]
		,[ProductType]
		,[IssuerName]
		,[IssueDate]
		,[SettlementDate]
		,a.[MaturityDate]
		,sum([PrincipalAmount]) as PrincipalAmount
		,sum([SettlementAmount]) as SettlementAmount
		,avg([InterestRate]) as InterestRate
		,[InterestRateType]
		,IncomePaymentType
		,DATEDIFF(day, SettlementDate, a.MaturityDate) AS TimeToMaturity
		,#maxmaturities2.max_maturity
			FROM [NBDataHub].[DTCC].[CPCDtransactions] as a
			left join #maxmaturities2 on a.cusip = #maxmaturities2.cusip and a.MaturityDate = #maxmaturities2.MaturityDate
					
			group by a.CUSIP, ProductType, IssuerName, SettlementDate, a.MaturityDate, InterestRateType, IncomePaymentType, max_maturity, IssueDate, DATEDIFF(day, SettlementDate, a.MaturityDate)  
"
  

  dbhandle <- odbcDriverConnect('driver={SQL Server};SERVER=wm-x-s-31;database = NBDataHub;trusted_connection=true')
  
  dtcc_data <-  sqlQuery(dbhandle, sql_srch)
  
  close(dbhandle)
  
  dtcc_data = dtcc_data %>%
    mutate(IssuerName = trimws(IssuerName),
           CUSIP = as.character(CUSIP),
           SettlementDate = anytime::anydate(SettlementDate),
           MaturityDate = anytime::anydate(MaturityDate),
           IssueDate = anytime::anydate(IssueDate),
           TimeToMaturity = as.double(MaturityDate - SettlementDate),
           Yield = InterestRate,
           Yield = if_else(InterestRateType == "F" & IncomePaymentType == "Z",
                           ((PrincipalAmount - SettlementAmount)/SettlementAmount)*(360/TimeToMaturity)*100, Yield),
           Yield = if_else(InterestRateType == "F" & IncomePaymentType == "I",
                           
                           (((1+(InterestRate/100)/360*max_maturity)*PrincipalAmount - SettlementAmount)/SettlementAmount)*360/TimeToMaturity*100, Yield),
           IssueDate = if_else(is.na(IssueDate), SettlementDate, IssueDate))
  
  
  dtcc_data = dtcc_data %>%
    group_by(CUSIP, max_maturity) %>%
    mutate(EstIssueDate = min(SettlementDate)) %>%
    ungroup() %>%
    mutate(OriginalMaturity = as.double(MaturityDate - EstIssueDate))
  
  
  dtcc_data = dtcc_data %>%
    AddMaturityBuckets() %>%
    TagBanksCPCD()
  
  return(dtcc_data)
  
  
  
}