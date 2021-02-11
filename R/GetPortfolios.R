# GetPortfolioList ----------------------------------------------------------------------

#' GetPortfolios API Call
#'
#' This function queries the Archway API for information on portfolios
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @export
GetPortfolioList <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  xml_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPortfolioList</a:Action>
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
                <GetPortfolioList xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
        		</GetPortfolioList>
        	</s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(xml_body, tmp_call, options = "format")

  Portfolios <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                     body = httr::upload_file(tmp_call),
                     httr::content_type('application/soap+xml; charset=utf-8'),
                     httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  portfolios_result <- Portfolios$content %>%
   xml2::read_xml() %>% xml2::as_list()
  portfolios_list <- portfolios_result$Envelope$Body$GetPortfolioListResponse$GetPortfolioListResult$Entities
  portfolios_df <- dplyr::tibble(portfolios = portfolios_list) %>%
     tidyr::unnest_wider(portfolios) %>%
     tidyr::unnest_longer(EntityID) %>%
     tidyr::unnest_longer(Portfolios) %>%
     tidyr::unnest_wider(Portfolios) %>%
     tidyr::unnest_longer(DefaultPortfolio) %>%
     tidyr::unnest_longer(ExcludeFromDisplay) %>%
     tidyr::unnest_longer(ExcludeFromRebalance) %>%
     tidyr::unnest_longer(IncomeOffsetNotional) %>%
     tidyr::unnest_longer(PortfolioCurrency) %>%
     tidyr::unnest_longer(PortfolioGLCashAccount) %>%
     tidyr::unnest_longer(PortfolioGLDueToDueFromAccount) %>%
     tidyr::unnest_longer(PortfolioGLInvestmentAccount) %>%
     tidyr::unnest_longer(PortfolioGLNestedEntityCostAccount) %>%
     tidyr::unnest_longer(PortfolioGLNestedEntityDisparity) %>%
     tidyr::unnest_longer(PortfolioGLNestedEntityProfitLoss) %>%
     tidyr::unnest_longer(PortfolioID) %>%
     tidyr::unnest_longer(PortfolioInventoryMethod) %>%
     tidyr::unnest_longer(PortfolioName) %>%
     tidyr::unnest_longer(PortfolioNotes) %>%
     tidyr::unnest_longer(WashSales) %>%
     tidyr::unnest_longer(PrimaryAccountNumberList) %>%
     tidyr::unnest_longer(PrimaryAccountNumberList) %>%
     tidyr::unnest_longer(PrimaryDataProvider) %>%
     tidyr::unnest_longer(DateOpened) %>%
    dplyr::select(-PortfolioClasses) %>%
    readr::type_convert()

  return(portfolios_df)
}

#' GetPortfolioClasses API Call
#'
#' This function queries the Archway API for information on portfolio classes
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @export
GetPortfolioClasses <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  xml_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPortfolioList</a:Action>
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
                <GetPortfolioList xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
        		</GetPortfolioList>
        	</s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(xml_body, tmp_call, options = "format")

  Portfolios <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                     body = httr::upload_file(tmp_call),
                     httr::content_type('application/soap+xml; charset=utf-8'),
                     httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  #############################################################

  doc <- Portfolios$content %>%
    xml2::read_xml()

  #############################################################

  portfolios_result <- Portfolios$content %>%
    xml2::read_xml() %>% xml2::as_list()
  portfolios_list <- portfolios_result$Envelope$Body$GetPortfolioListResponse$GetPortfolioListResult$Entities
  portfolios_df <- dplyr::tibble(portfolios = portfolios_list) %>%
     tidyr::unnest_wider(portfolios) %>%
     tidyr::unnest_longer(EntityID) %>%
     tidyr::unnest_longer(Portfolios) %>%
     tidyr::unnest_wider(Portfolios) %>%
    #  tidyr::unnest_longer(DefaultPortfolio) %>%
    #  tidyr::unnest_longer(ExcludeFromDisplay) %>%
    #  tidyr::unnest_longer(ExcludeFromRebalance) %>%
    #  tidyr::unnest_longer(IncomeOffsetNotional) %>%
    #  tidyr::unnest_longer(PortfolioCurrency) %>%
    #  tidyr::unnest_longer(PortfolioGLCashAccount) %>%
    #  tidyr::unnest_longer(PortfolioGLDueToDueFromAccount) %>%
    #  tidyr::unnest_longer(PortfolioGLInvestmentAccount) %>%
    #  tidyr::unnest_longer(PortfolioGLNestedEntityCostAccount) %>%
    #  tidyr::unnest_longer(PortfolioGLNestedEntityDisparity) %>%
    #  tidyr::unnest_longer(PortfolioGLNestedEntityProfitLoss) %>%
     tidyr::unnest_longer(PortfolioID) %>%
    #  tidyr::unnest_longer(PortfolioInventoryMethod) %>%
     tidyr::unnest_longer(PortfolioName) %>%
    #  tidyr::unnest_longer(PortfolioNotes) %>%
    #  tidyr::unnest_longer(WashSales) %>%
    #  tidyr::unnest_longer(PrimaryAccountNumberList) %>%
    #  tidyr::unnest_longer(PrimaryAccountNumberList) %>%
    #  tidyr::unnest_longer(PrimaryDataProvider) %>%
    #  tidyr::unnest_longer(DateOpened) %>%
     tidyr::unnest_longer(PortfolioClasses) %>%
     tidyr::unnest_wider(PortfolioClasses) %>%
     tidyr::unnest_longer(PortfolioClassCode) %>%
     tidyr::unnest_longer(PortfolioClassEffectiveDate) %>%
     tidyr::unnest_longer(PortfolioClassID) %>%
     tidyr::unnest_longer(PortfolioClassName) %>%
     tidyr::unnest_longer(PortfolioClassNotes) %>%
    dplyr::select(-...1) %>%
     tidyr::unnest_wider(PortfolioClassOptions) %>%
     tidyr::unnest_longer(GetPortfolioListPortfolioClassOption) %>%
     tidyr::unnest_longer(GetPortfolioListPortfolioClassOption) %>%
     tidyr::pivot_wider(names_from = GetPortfolioListPortfolioClassOption_id, values_from = GetPortfolioListPortfolioClassOption) %>%
    dplyr::selectselect(EntityID, PortfolioID, PortfolioName, PortfolioClassName, PortfolioClassID,  PortfolioClassCode, PortfolioClassOptionName) %>%
    readr::type_convert()


  return(portfolios_df)
}

