library(RODBC)
library(tidyverse)

GetHistoricSum = function() {

  sql_srch = "

    set nocount on

  SELECT  [CUSIP]
      ,[MaturityDate]
	  ,max(datediff(day, settlementdate, maturitydate)) as max_maturity
	  into #maxmaturities
  FROM [NBDataHub].[DTCC].[CPCDtransactions]
  group by cusip, MaturityDate




  select [CUSIP]
		,[ProductType]
		,[IssuerName]
		,[IssueDate]
		,[SettlementDate]
		,[MaturityDate]
		,sum([PrincipalAmount]) as PrincipalAmount
		,sum([SettlementAmount]) as SettlementAmount
		,avg([InterestRate]) as InterestRate
		,[InterestRateType]
		,IncomePaymentType
		,TimeToMaturity
		,max_maturity
		from(
			SELECT  a.[CUSIP]
					,[ProductType]
					,[IssuerName]
					,[IssueDate]
					,[SettlementDate]
					,a.[MaturityDate]
					,[PrincipalAmount]
					,[SettlementAmount]
					,[InterestRate]
					,[InterestRateType]
					,[IncomePaymentType]
					,#maxmaturities.max_maturity
					--,[PartiesToTransactionClassification]
					--,[TransactionID]
					,DATEDIFF(day, SettlementDate, a.MaturityDate) AS TimeToMaturity
					FROM [NBDataHub].[DTCC].[CPCDtransactions] as a
					left join #maxmaturities on a.cusip = #maxmaturities.cusip and a.MaturityDate = #maxmaturities.MaturityDate
					)
			as d
			group by CUSIP, ProductType, IssuerName, SettlementDate, MaturityDate, InterestRateType, IncomePaymentType, max_maturity, IssueDate, TimeToMaturity

"

  dbhandle <- odbcDriverConnect('driver={SQL Server};server=P-127-230-020\\PS010;database = NBDataHub;trusted_connection=true')

  historic_sum <-  sqlQuery(dbhandle, sql_srch)

  close(dbhandle)

  historic_sum = historic_sum %>%
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


  historic_sum = historic_sum %>%
    group_by(CUSIP, max_maturity) %>%
    mutate(EstIssueDate = min(SettlementDate)) %>%
    ungroup() %>%
    mutate(OriginalMaturity = as.double(MaturityDate - EstIssueDate))


  historic_sum = historic_sum %>%
    AddMaturityBuckets() %>%
    TagBanksCPCD()

  return(historic_sum)

}








