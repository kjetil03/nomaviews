#' Get historic issue data from DTCC
#'
#' @param start_date start-dato for uttrekk
#'
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @import magrittr
#'
#'
#' @export
#'
#' @examples
#' \dontrun{data = GetHistoricIssueData()}
GetHistoricIssueData <- function(start_date = "2010-01-01") {


  #Hent data fra databasen
  sql_srch = "

  set nocount on

select cusip
		,max(maturitydate) as max_maturity_date
		,settlementdate
		into #maxmaturity
		 FROM [NBDataHub].[DTCC].[CPCDtransactions]
  where settlementdate >= 'start_date'
  group by cusip, settlementdate



  select cusip
		--,settlementdate
		,max_maturity_date
		 ,min(settlementdate) as est_issuedate
		 into #estissuedates
		 from #maxmaturity
		 where settlementdate >= 'start_date'
		 group by cusip, max_maturity_date--, SettlementDate
		 order by cusip



select CUSIP
 ,SettlementDate
 ,MaturityDate
 ,ProductType
 ,IssueDate
 ,InterestRateType
 ,IncomePaymentType
 , sum(SettlementAmount) as SettlementAmount
 , sum(principalamount) as PrincipalAmount
 , avg(interestrate) as InterestRate
 , avg(yield) as Yield
 , avg(timetomaturity) as TimeToMaturity
 , IssuerName
 , max_maturity_date

  from(
	SELECT d.[CUSIP]
      ,[ProductType]
      ,[IssuerName]
     ,d.[SettlementDate]
      ,[IssueDate]
      ,MaturityDate
      ,[PrincipalAmount]
      ,[SettlementAmount]
      ,[InterestRate]
      ,[InterestRateType]
      ,[IncomePaymentType]
      ,max_maturity_date
	  ,new_issue = (case when est_issuedate = d.settlementdate then 1 else 0 end)
	  ,timetomaturity = datediff(day, d.settlementdate, d.maturitydate)
	  ,yield = ((PrincipalAmount - SettlementAmount)/PrincipalAmount)*365/datediff(day, d.settlementdate, d.maturitydate)*100

	  FROM [NBDataHub].[DTCC].[CPCDtransactions] as d
	  left join #maxmaturity on d.cusip = #maxmaturity.cusip and d.SettlementDate = #maxmaturity.settlementdate
	  left join #estissuedates on d.cusip = #estissuedates.cusip and #maxmaturity.max_maturity_date = #estissuedates.max_maturity_date

	  ) as d
	  where (DATEDIFF(day, d.SettlementDate, MaturityDate) >= 1)
	  and  settlementdate >= 'start_date'
	  and new_issue = 1
	  group by CUSIP, ProductType, IssuerName, SettlementDate, IssueDate, MaturityDate, InterestRateType, new_issue, InterestRateType
    ,IncomePaymentType, max_maturity_date
	  order by SettlementDate"


  sql_srch = gsub("start_date", start_date, sql_srch)



  dbhandle <- odbcDriverConnect('driver={SQL Server};server=P-127-230-020\\PS010;database = NBDataHub;trusted_connection=true')

  data <-  sqlQuery(dbhandle, sql_srch)

  #Lukk tilkobling
  close(dbhandle)

  data = data %>%
    mutate(IssuerName = trimws(IssuerName),
           CUSIP = as.character(CUSIP),
           SettlementDate = anytime::anydate(SettlementDate),
           MaturityDate = anytime::anydate(MaturityDate),
           IssueDate = anytime::anydate(IssueDate),
           TimeToMaturity = as.double(MaturityDate - SettlementDate),
           Yield = ((PrincipalAmount - SettlementAmount)/PrincipalAmount)*(360/TimeToMaturity)*100,
           IssueDate = if_else(is.na(IssueDate), EstIssueDate, IssueDate))



  return(data)



}
