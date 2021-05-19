# GetIncomeStatement------------------------------------------------------------------------------

#' GetBalanceSheet API Call
#'
#' This function queries the Archway API for Balance Sheet data for all Entities.
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @param AsOfDate  Date for end of period in yyyy-mm-dd format
#' @import tidyverse
#' @import DBI
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export

GetBalanceSheet <- function(username, password, enterpriseID, AsOfDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )

  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  getBS <- glue::glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetBalanceSheet</a:Action>
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
        <GetBalanceSheet xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
              <AsOfDate>{AsOfDate}</AsOfDate>
        		</GetBalanceSheet>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(getBS, tmp_call, options = "format")
  BalanceSheet <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                                body = httr::upload_file(tmp_call),
                                httr::content_type('application/soap+xml; charset=utf-8'),
                                httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  ############################################################################

  BalanceSheet_result <-  BalanceSheet$content %>%
    xml2::read_xml() %>% as_list()

  BalanceSheet_list <- BalanceSheet_result$Envelope$Body$GetBalanceSheetResponse$GetBalanceSheetResult$Entities

  BalanceSheet_df <- tibble(Entities = BalanceSheet_list)  %>%
    unnest_wider(Entities) %>%
    unnest_longer(COAs) %>%
    unnest_wider(COAs) %>%
    unnest(cols = c(EntityID, AccountCode, COAAccountName, COAID, COAAccountValue)) %>%
    unnest(cols = c(EntityID, AccountCode, COAAccountName, COAID, COAAccountValue)) %>%
    select(EntityID, AccountCode, COAAccountName,  COAID, COAAccountValue) %>%
    type_convert(cols(
      EntityID = col_integer(),
      AccountCode = col_integer(),
      COAAccountName = col_character(),
      COAID = col_integer(),
      COAAccountValue = col_double()
    )) %>%
    mutate(COAAccountValue = replace_na(COAAccountValue, 0),
           COAAccountValue = round(COAAccountValue, 6),
           AsOfDate = as_date(AsOfDate),
           UploadDate = Sys.time())


  return(BalanceSheet_df)

}
