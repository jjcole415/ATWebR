# GetEntities -------------------------------------------------------------------------

#' GetEntities API Call
#'
#' This function queries the Archway API for information on entities
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @export
GetEntities <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

    GetEntities_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetEntitiesList</a:Action>
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
                <GetEntitiesList xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
        		</GetEntitiesList>
        	</s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(GetEntities_body, tmp_call, options = "format")

  Entities <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                   body = httr::upload_file(tmp_call),
                   httr::content_type('application/soap+xml; charset=utf-8'),
                   httr::add_headers(Expect = "100-continue"),
                   httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  entities_result <- Entities$content %>%
    xml2::read_xml() %>% xml2::as_list()
  entities_list <- entities_result$Envelope$Body$GetEntitiesListResponse$GetEntitiesListResult$Entities
  entities_df <- tidyr::tibble(entities = entities_list) %>%
    tidyr::unnest_wider(entities) %>%
    tidyr::unnest_longer(CashManagement) %>%
    tidyr::unnest_longer(DatePerformanceLocked) %>%
    tidyr::unnest_longer(EntityAccountingPeriodFrequency) %>%
    tidyr::unnest_longer(EntityAccrualMethod) %>%
    tidyr::unnest_longer(EntityAccrueFees) %>%
    tidyr::unnest_longer(EntityCurrency) %>%
    tidyr::unnest_longer(EntityDisparityMethod) %>%
    tidyr::unnest_longer(EntityFiscalYearEnd) %>%
    tidyr::unnest_longer(EntityGainAllocationFrequency) %>%
    tidyr::unnest_longer(EntityGainAllocationHandling) %>%
    tidyr::unnest_longer(EntityGainAllocationMethod) %>%
    tidyr::unnest_longer(EntityID) %>%
    tidyr::unnest_longer(EntityIncomeAllocationFrequency ) %>%
    tidyr::unnest_longer(EntityIncomeAllocationMethod    ) %>%
    tidyr::unnest_longer(EntityNAV) %>%
    tidyr::unnest_longer(EntityName) %>%
    tidyr::unnest_longer(EntityTaxID) %>%
    tidyr::unnest_longer(EntityUnits) %>%
    tidyr::unnest_longer(ExcludeFromDisplay) %>%
    tidyr::unnest_longer(ForExGainGLAccount) %>%
    tidyr::unnest_longer(HarvestLosses) %>%
    tidyr::unnest_longer(IncludeToSideAuthorizers) %>%
    tidyr::unnest_longer(InterCompanyGLAccount) %>%
    tidyr::unnest_longer(LastClosedAccountingPeriod) %>%
    tidyr::unnest_longer(OpenAccountingPeriod) %>%
    tidyr::unnest_longer(TaxLossCarryForward) %>%
    tidyr::unnest_longer(XfileOffsetGLAccount) %>%
    tidyr::unnest_longer(EntityDateStarted) %>%
    dplyr::select(-EntityClasses) %>%
    readr::type_convert()

  return(entities_df)
}


#' GetEntities API Call for Class Information
#'
#' This function queries the Archway API for information on entity classes
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @export
GetEntityClasses <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  GetEntities_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetEntitiesList</a:Action>
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
                <GetEntitiesList xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
        		</GetEntitiesList>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(GetEntities_body, tmp_call, options = "format")

  Entities <- httr:POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                   body = upload_file(tmp_call),
                   content_type('application/soap+xml; charset=utf-8'),
                   add_headers(Expect = "100-continue"), verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  entities_result <- Entities$content %>%
    xml2::read_xml() %>% as_list()
  entities_list <- entities_result$Envelope$Body$GetEntitiesListResponse$GetEntitiesListResult$Entities
  entities_df <- tidyr::tibble(entities = entities_list) %>%
    tidyr::unnest_wider(entities) %>%
    dplyr::select(EntityID, EntityName, EntityClasses) %>%
    tidyr::unnest_longer(EntityID) %>%
    tidyr::unnest_longer(EntityName) %>%
    tidyr::unnest_longer(EntityClasses) %>%
    tidyr::unnest_wider(EntityClasses) %>%
    tidyr::unnest_longer(EntityClassID) %>%
    tidyr::unnest_longer(EntityClassName) %>%
    tidyr::unnest_longer(EntityClassOptions) %>%
    dplyr::select(-...1) %>%
    tidyr::unnest_wider(EntityClassOptions) %>%
    tidyr::unnest_longer(EntityClassOptionID) %>%
    tidyr::unnest_longer(EntityClassOptionName) %>%
    dplyr::select(EntityID, EntityName, EntityClassID, EntityClassName, EntityClassOptionID, EntityClassOptionName) %>%
    readr::type_convert()

  return(entities_df)
}
