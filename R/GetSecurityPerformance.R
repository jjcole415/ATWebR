# GetSecurityPerformance ------------------------------------------------------------------------

#' GetSecurityPerformance API Call
#'
#' This function queries the Archway API for Performance data. It is programmed to fetch performance for all entities
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
GetSecurityPerformance <- function(username, password, enterpriseID, StartDate, EndDate, Metric = c("TWR", "IRR", "XIRR"), Annualized = FALSE){
  base_URL <- "archwayplatform.seic.com"
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )

  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  xml_body <- glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetSecurityPerformance</a:Action>
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
            <GetSecurityPerformance xmlns="http://www.atweb.us/ATWebAPI">
            			<enterpriseID>{enterpriseID}</enterpriseID>
            			<entityIDs i:nil = "true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
              			<b:int>816219</b:int>
            			</entityIDs>
            			<protfolioIDs i:nil = "true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
            			  <b:int>856632</b:int>
            			</protfolioIDs>
            			<securityIDs i:nil = "true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
            			  <b:int>1962871</b:int>
            			</securityIDs>
            			<PerformanceMetricType>{Metric}</PerformanceMetricType>
            			<annulized>false</annulized>
            			<StartDate>{StartDate}</StartDate>
                  <EndDate>{EndDate}</EndDate>
                  <PerformanceMetricType>{Metric}</PerformanceMetricType>
            		</GetSecurityPerformance>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(xml_body, tmp_call, options = "format")
  performance <- POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                      body = upload_file(tmp_call),
                      content_type('application/soap+xml; charset=utf-8'),
                      add_headers(Expect = "100-continue"), verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)
  performance_result <- performance$content %>%
    read_xml() %>% as_list()

  performance_raw <- performance_result$Envelope$Body$GetSecurityPerformanceResponse$GetSecurityPerformanceResult$Entities
  performance_df <- tibble(performance = performance_raw) %>%
    unnest_wider(performance) %>%
    unnest_longer(SecurityPerformances) %>%
    unnest_wider(SecurityPerformances) %>%
    unnest(cols = c(EntityID, PerformanceReturnValue, PortfolioID, SecurityID,
                    SecurityPrimaryID)) %>%
    unnest(cols = c(EntityID, PerformanceReturnValue, PortfolioID, SecurityID,
                    SecurityPrimaryID)) %>%
    select(-SecurityPerformances_id) %>%
    type_convert() %>%
    mutate(StartDate = as_date(StartDate), EndDate = as_date(EndDate), UploadDate = Sys.Date())

  return(performance_df)
}


#' GetSecurityPerformanceByEntity API Call
#'
#' This function queries the Archway API for Performance data. It is programmed to fetch performance for a single entity
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
GetSecurityPerformanceByEntity <- function(username, password, enterpriseID, EntityID = 0, StartDate, EndDate, Metric = c("TWR", "IRR", "XIRR"), Annualized = FALSE){
  base_URL <- "archwayplatform.seic.com"
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  nil_param <- if_else(EntityID == 0, true = "true", false = "false")
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  xml_body <- glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetSecurityPerformance</a:Action>
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
            <GetSecurityPerformance xmlns="http://www.atweb.us/ATWebAPI">
            			<enterpriseID>{enterpriseID}</enterpriseID>
            			<entityIDs i:nil = "{nil_param}" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
              			<b:int>{EntityID}</b:int>
            			</entityIDs>
            			<protfolioIDs i:nil = "true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
            			  <b:int>856632</b:int>
            			</protfolioIDs>
            			<securityIDs i:nil = "true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
            			  <b:int>1962871</b:int>
            			</securityIDs>
            			<PerformanceMetricType>{Metric}</PerformanceMetricType>
            			<annulized>false</annulized>
            			<StartDate>{StartDate}</StartDate>
                  <EndDate>{EndDate}</EndDate>
                  <PerformanceMetricType>{Metric}</PerformanceMetricType>
            		</GetSecurityPerformance>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(xml_body, tmp_call, options = "format")
  performance <- POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                      body = upload_file(tmp_call),
                      content_type('application/soap+xml; charset=utf-8'),
                      add_headers(Expect = "100-continue"), verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)
  performance_result <- performance$content %>%
    read_xml() %>% as_list()

  performance_raw <- performance_result$Envelope$Body$GetSecurityPerformanceResponse$GetSecurityPerformanceResult$Entities
  performance_df <- tibble(performance = performance_raw) %>%
    unnest_wider(performance) %>%
    unnest_longer(SecurityPerformances) %>%
    unnest_wider(SecurityPerformances) %>%
    unnest(cols = c(EntityID, PerformanceReturnValue, PortfolioID, SecurityID,
                    SecurityPrimaryID)) %>%
    unnest(cols = c(EntityID, PerformanceReturnValue, PortfolioID, SecurityID,
                    SecurityPrimaryID)) %>%
    select(-SecurityPerformances_id) %>%
    type_convert() %>%
    mutate(StartDate = as_date(StartDate), EndDate = as_date(EndDate), UploadDate = Sys.Date())

  return(performance_df)
}


