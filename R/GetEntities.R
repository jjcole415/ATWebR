# GetEntities -------------------------------------------------------------------------

#' GetEntitiesRaw API Call
#'
#' This function queries the Archway API for information on entities
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import DBI
#' @import xml2
#' @export
GetEntitiesRaw <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"
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
        </s:Envelope>')

  # %>%
  #   xml2::read_xml()
  #
  # tmp_call <- tempfile(fileext = ".xml")
  # xml2::write_xml(GetEntities_body, tmp_call, options = "format")

  Entities <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                         body = GetEntities_body, # httr::upload_file(tmp_call),
                         httr::content_type('application/soap+xml; charset=utf-8'),
                         httr::add_headers(Expect = "100-continue"),
                         httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  # file.remove(tmp_call)

  return(Entities)
}

#' GetEntities API Call
#'
#' This function queries the Archway API for information on entities
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import tidyr
#' @import xml2
#' @import dplyr
#' @import purrr
#' @import DBI
#' @export
GetEntities <- function(username, password, enterpriseID){
  Entities <- GetEntitiesRaw(username, password, enterpriseID)
  entities_result <- Entities$content %>%
    xml2::read_xml() %>% xml2::as_list()
  entities_list <- entities_result$Envelope$Body$GetEntitiesListResponse$GetEntitiesListResult$Entities
  entities_df <- tidyr::tibble(entities = entities_list) %>%
    tidyr::unnest_wider(entities) %>%
    select(-EntityClasses) %>%
    tidyr::unnest(cols = c(CashManagement, DatePerformanceLocked, EntityAccountingPeriodFrequency,
                           EntityAccrualMethod, EntityAccrueFees, EntityCurrency,
                           EntityDisparityMethod, EntityFiscalYearEnd, EntityGainAllocationFrequency,
                           EntityGainAllocationHandling, EntityGainAllocationMethod,
                           EntityID, EntityIncomeAllocationFrequency, EntityIncomeAllocationMethod,
                           EntityNAV, EntityName, EntityTaxID, EntityUnits, ExcludeFromDisplay,
                           ForExGainGLAccount, HarvestLosses, IncludeToSideAuthorizers,
                           InterCompanyGLAccount, LastClosedAccountingPeriod, OpenAccountingPeriod,
                           PayToProfile, TaxLossCarryForward, TrackWithdrawalByContribution,
                           XfileOffsetGLAccount, LastClosedAccountingPeriodMarkedFinal,
                           EntityDateStarted)) %>%
    tidyr::unnest(cols = c(CashManagement, DatePerformanceLocked, EntityAccountingPeriodFrequency,
                           EntityAccrualMethod, EntityAccrueFees, EntityCurrency,
                           EntityDisparityMethod, EntityFiscalYearEnd, EntityGainAllocationFrequency,
                           EntityGainAllocationHandling, EntityGainAllocationMethod,
                           EntityID, EntityIncomeAllocationFrequency, EntityIncomeAllocationMethod,
                           EntityNAV, EntityName, EntityTaxID, EntityUnits, ExcludeFromDisplay,
                           ForExGainGLAccount, HarvestLosses, IncludeToSideAuthorizers,
                           InterCompanyGLAccount, LastClosedAccountingPeriod, OpenAccountingPeriod,
                           PayToProfile, TaxLossCarryForward, TrackWithdrawalByContribution,
                           XfileOffsetGLAccount, LastClosedAccountingPeriodMarkedFinal,
                           EntityDateStarted)) %>%
    readr::type_convert(cols(
      .default = col_character(),
      CashManagement = col_logical(),
      DatePerformanceLocked = col_datetime(format = ""),
      EntityAccrueFees = col_logical(),
      EntityFiscalYearEnd = col_double(),
      EntityGainAllocationFrequency = col_double(),
      EntityGainAllocationHandling = col_double(),
      EntityID = col_double(),
      EntityIncomeAllocationFrequency = col_double(),
      EntityNAV = col_double(),
      EntityUnits = col_double(),
      ExcludeFromDisplay = col_logical(),
      HarvestLosses = col_logical(),
      IncludeToSideAuthorizers = col_logical(),
      LastClosedAccountingPeriod = col_datetime(format = ""),
      OpenAccountingPeriod = col_datetime(format = ""),
      TaxLossCarryForward = col_double(),
      TrackWithdrawalByContribution = col_logical(),
      LastClosedAccountingPeriodMarkedFinal = col_datetime(format = ""),
      EntityDateStarted = col_datetime(format = "")
    ))

  return(entities_df)
}


#' GetEntities API Call for Class Information
#'
#' This function queries the Archway API for information on entity classes
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import xml2
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import DBI
#' @export
GetEntityClasses <- function(username, password, enterpriseID){
  Entities <- GetEntitiesRaw(username, password, enterpriseID)
  entities_result <- Entities$content %>%
    xml2::read_xml() %>% xml2::as_list()
  entities_list <- entities_result$Envelope$Body$GetEntitiesListResponse$GetEntitiesListResult$Entities
  entities_df <- tidyr::tibble(entities = entities_list) %>%
    tidyr::unnest_wider(entities) %>%
    dplyr::select(EntityID, EntityName, EntityClasses) %>%
    tidyr::unnest_longer(EntityClasses) %>%
    tidyr::unnest_wider(EntityClasses) %>%
    tidyr::unnest_longer(EntityClassOptions)  %>%
    tidyr::unnest_wider(EntityClassOptions) %>%
    tidyr::unnest(cols = c(EntityID, EntityName, EntityClassAllowMultiple, EntityClassID,
                    EntityClassName, EntityClassNotes, EntityClassEffectiveDate,
                    EntityClassOptionID, EntityClassOptionName, EntityClassOptionCode,
                    EntityClassOptionNotes, EntityClassCode)) %>%
    tidyr::unnest(cols = c(EntityID, EntityName, EntityClassAllowMultiple, EntityClassID,
                    EntityClassName, EntityClassNotes, EntityClassEffectiveDate,
                    EntityClassOptionID, EntityClassOptionName, EntityClassOptionCode,
                    EntityClassOptionNotes, EntityClassCode)) %>%
    # dplyr::select(EntityID, EntityName, EntityClassID, EntityClassName, EntityClassOptionID, EntityClassOptionName) %>%
    readr::type_convert(cols(
      EntityID = col_double(),
      EntityName = col_character(),
      EntityClassAllowMultiple = col_logical(),
      EntityClassID = col_double(),
      EntityClassName = col_character(),
      EntityClassNotes = col_character(),
      EntityClassEffectiveDate = col_datetime(format = ""),
      EntityClassOptionID = col_double(),
      EntityClassOptionName = col_character(),
      EntityClassOptionCode = col_character(),
      EntityClassOptionNotes = col_character(),
      EntityClassOptions_id = col_character(),
      EntityClassCode = col_character(),
      EntityClasses_id = col_character()
    ))

  return(entities_df)
}
