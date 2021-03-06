# GetInvestorBalances ------------------------------------------------------------------------------

#' GetInvestorBalances API Call
#'
#' This function queries the Archway API for equity account balances from the GL for a given period
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @param StartDate  Date for beginning of period in yyyy-mm-dd format
#' @param EndDate  Date for end of period in yyyy-mm-dd format
#' @import tidyverse
#' @import DBI
#' @import xml2
#' @import dplyr
#' @import purrr
#' @export

GetInvestorBalances <- function(username, password, enterpriseID, StartDate, EndDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )

  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  getInvBal <- glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetInvestorBalances</a:Action>
        		<a:ReplyTo>
        			<a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
        		</a:ReplyTo>
        		<a:To s:mustUnderstand="1">https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc</a:To>
        		<o:Security s:mustUnderstand="1" xmlns:o="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">
        			<u:Timestamp u:Id="_0">
        				<u:Created>{created}</u:Created>
        				<u:Expires>{ended}</u:Expires>
        			</u:Timestamp>
        			<o:UsernameToken u:Id="uuid-4ec07a4e-b63a-42e2-b8ab-76beb035c7fd-2">
        				<o:Username>{UserID}</o:Username>
        				<o:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">{SessionID}</o:Password>
        			</o:UsernameToken>
        		</o:Security>
        	</s:Header>
        	<s:Body>
        <GetInvestorBalances xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
              <StartDate>{StartDate}</StartDate>
              <EndDate>{EndDate}</EndDate>
        		</GetInvestorBalances>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(getInvBal, tmp_call, options = "format")
  InvestorBalances <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                           body = httr::upload_file(tmp_call),
                           httr::content_type('application/soap+xml; charset=utf-8'),
                           httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  ############################################################################

  InvestorBalances_result <- InvestorBalances$content %>%
    xml2::read_xml() %>% as_list()

  InvestorBalances_list <- InvestorBalances_result$Envelope$Body$GetInvestorBalancesResponse$GetInvestorBalancesResult$Investors

  (InvestorBalances_df <- tibble(Investors = InvestorBalances_list) %>%
      unnest_wider(Investors) %>%
      unnest_longer(InvestorBalances) %>%
      unnest_wider(InvestorBalances) %>%
      unnest(cols = c(EntityID, InvestorAssociatedID, InvestorAssociatedIDType, InvestorBookAccount,
                      InvestorDisparityAccount, InvestorEquityAccountName, InvestorEquityAccountNumber,
                      InvestorNav, InvestorOwnershipPercent, InvestorTaxAccount,
                      InvestorUnits, ManagementFeesAccrued, ManagementFeesCharged,
                      PerformanceFeesAccrued, PerformanceFeesCharged)) %>%
      unnest(cols = c(EntityID, InvestorAssociatedID, InvestorAssociatedIDType, InvestorBookAccount,
                      InvestorDisparityAccount, InvestorEquityAccountName, InvestorEquityAccountNumber,
                      InvestorNav, InvestorOwnershipPercent, InvestorTaxAccount,
                      InvestorUnits, ManagementFeesAccrued, ManagementFeesCharged,
                      PerformanceFeesAccrued, PerformanceFeesCharged)) %>%
      select(-InvestorBalances_id) %>%
      type_convert(cols(
        EntityID = col_integer(),
        InvestorAssociatedID = col_integer(),
        InvestorAssociatedIDType = col_character(),
        InvestorBookAccount = col_double(),
        InvestorDisparityAccount = col_double(),
        InvestorEquityAccountName = col_character(),
        InvestorEquityAccountNumber = col_integer(),
        InvestorNav = col_double(),
        InvestorOwnershipPercent = col_double(),
        InvestorTaxAccount = col_double(),
        InvestorUnits = col_double(),
        ManagementFeesAccrued = col_double(),
        ManagementFeesCharged = col_double(),
        PerformanceFeesAccrued = col_double(),
        PerformanceFeesCharged = col_double()
        )
      ) %>%
      mutate(StartDate = as_date(StartDate),
             EndDate = as_date(EndDate),
             UploadDate = Sys.time())

  )

  return(InvestorBalances_df)

}
