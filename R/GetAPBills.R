#GetAPBillsRaw-------------------------------------------------------------------------------------------

#' GetAPBillsRaw API Call
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
GetAPBillsRaw <- function(username, password, enterpriseID, BillStartDate, BillEndDate){ # , PostStartDate = NULL, PostEndDate = NULL
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
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetAPBills</a:Action>
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
              <GetAPBills xmlns="http://www.atweb.us/ATWebAPI">
        			  <enterpriseID>{enterpriseID}</enterpriseID>
        			  <entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
                    <vendorBillStartDate>{BillStartDate}</vendorBillStartDate>
                    <vendorBillEndDate>{BillEndDate}</vendorBillEndDate>
        		</GetAPBills>
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

  return(api_resp)
}


#' GetAPBills API Call
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
GetAPBills <- function(username, password, enterpriseID, BillStartDate, BillEndDate){
  APBills <-  GetAPBillsRaw(username, password, enterpriseID, BillStartDate, BillEndDate)

  apbills_result <- APBills$content %>%
    read_xml() %>% as_list()
  apbills_list <- apbills_result$Envelope$Body$GetAPBillsResponse$GetAPBillsResult$APBillEntities
  apbills__df <- tibble(Entities = apbills_list) %>%
    unnest_wider(Entities) %>%
    unnest_longer(APBills) %>%
    select(-EntityID) %>%
    unnest_wider(APBills) %>%
    unnest_longer(APBillLines) %>%
    select(-BillID) %>%
    unnest_wider(APBillLines) %>%
    unnest() %>%
    unnest() %>%
    type_convert(cols(
      .default = col_double(),
      EntityID = col_integer(),
      PortfolioID = col_integer(),
      SecurityID = col_integer(),
      BillLineName = col_character(),
      APBillLines_id = col_character(),
      BillNumber = col_character(),
      BillRecurrence = col_character(),
      BillRecurring = col_logical(),
      BillUnlimitedLife = col_logical(),
      DueDate = col_datetime(format = ""),
      Gift = col_logical(),
      PostDate = col_datetime(format = ""),
      VendorBillDate = col_datetime(format = ""),
      BillNotes = col_character(),
      BillAdditionalNotes = col_character(),
      APBills_id = col_character()
    ))
}
