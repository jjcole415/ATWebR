#GetPortfolioTransactionsRaw-------------------------------------------------------------------------------------------

#' GetPortfolioTransactionsRaw API Call
#'
#' This function queries the Archway API for information on Portfolio Transactions for a given period
#' and returns xml
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
#' @import glue
#' @import tidyr
#' @export
GetPortfolioTransactionsRaw <- function(username, password, enterpriseID, StartDate, EndDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  call_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPortfolioTransactions</a:Action>
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
              <GetPortfolioTransactions xmlns="http://www.atweb.us/ATWebAPI">
        			  <enterpriseID>{enterpriseID}</enterpriseID>
        			  <entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
                    <StartDate>{StartDate}</StartDate>
                    <EndDate>{EndDate}</EndDate>
        		</GetPortfolioTransactions>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(call_body, tmp_call, options = "format")

  api_resp <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                        body = httr::upload_file(tmp_call),
                        httr::content_type('application/soap+xml; charset=utf-8'),
                        httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  # GLTrans_result <- GLTrans$content %>%
  #   read_xml()


  return(api_resp)
}

#' GetPortfolioTransactionsRaw API Call
#'
#' This function queries the Archway API for information on Portfolio Transactions for a given period
#' and returns xml
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
#' @import glue
#' @import tidyr
#' @export
GetPortfolioTransactions <- function(username, password, enterpriseID, StartDate, EndDate){
  PortTrans <-  GetPortfolioTransactionsRaw(username, password, enterpriseID, StartDate, EndDate)

  port_trans_result <- PortTrans$content %>%
    read_xml() %>% as_list()
  port_trans_list <- port_trans_result$Envelope$Body$GetPortfolioTransactionsResponse$GetPortfolioTransactionsResult$Entities
  port_trans_df <- tibble(Entities = port_trans_list) %>%
    unnest_wider(Entities) %>%
    unnest_longer(Portfolios) %>%
    unnest_wider(Portfolios) %>%
    unnest_longer(Transactions) %>%
    unnest_wider(Transactions) %>%
    unnest(cols = c(EntityID, PortfolioID, BuySell, Commission, Commission2, Commission3,
                    Commission4, Commission5, ContributionDate, Currency, DirectAssignUG,
                    EffectiveYield, ExchangeRate, InKindTransaction, InKindUG,
                    PurchasedAccruedInterest, Quantity, SecurityID, SecurityPrimaryID,
                    SettleDate, SettleExchangeRate, SettleGLID, TotalCost, TradeDate,
                    TradeGLID, UnitPrice, UnitaryAveragePrice, ReferenceCode)) %>%
    unnest(cols = c(EntityID, PortfolioID, BuySell, Commission, Commission2, Commission3,
                    Commission4, Commission5, ContributionDate, Currency, DirectAssignUG,
                    EffectiveYield, ExchangeRate, InKindTransaction, InKindUG,
                    PurchasedAccruedInterest, Quantity, SecurityID, SecurityPrimaryID,
                    SettleDate, SettleExchangeRate, SettleGLID, TotalCost, TradeDate,
                    TradeGLID, UnitPrice, UnitaryAveragePrice, ReferenceCode)) %>%
    select(EntityID, PortfolioID, BuySell, Commission, Commission2, Commission3,
           Commission4, Commission5, ContributionDate, Currency, DirectAssignUG,
           EffectiveYield, ExchangeRate, InKindTransaction, InKindUG,
           PurchasedAccruedInterest, Quantity, SecurityID, SecurityPrimaryID,
           SettleDate, SettleExchangeRate, SettleGLID, TotalCost, TradeDate,
           TradeGLID, UnitPrice, UnitaryAveragePrice, ReferenceCode) %>%
    type_convert(cols(
      .default = col_double(),
      EntityID = col_integer(),
      PortfolioID = col_integer(),
      SecurityID = col_integer(),
      BuySell = col_character(),
      ContributionDate = col_datetime(format = ""),
      Currency = col_character(),
      InKindTransaction = col_logical(),
      SecurityPrimaryID = col_character(),
      SettleDate = col_datetime(format = ""),
      TradeDate = col_datetime(format = ""),
      ReferenceCode = col_character())) %>%
    dplyr::mutate(StartDate = StartDate, EndDate = EndDate, UploadDate = Sys.time())
}
