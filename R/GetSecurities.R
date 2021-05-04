# GetSecuritiesList ---------------------------------------------------------------------


#' GetSecuritiesRaw API Call
#'
#' This function queries the Archway API for information on securities
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @import purrr
#' @import xml2
#' @import dplyr
#' @export
GetSecuritiesListRaw <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  GetSecuritiesList_body <- glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetSecuritiesList</a:Action>
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
              <GetSecuritiesList xmlns="http://www.atweb.us/ATWebAPI">
        			  <enterpriseID>{enterpriseID}</enterpriseID>
        			  <entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
        		</GetSecuritiesList>
        	</s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(GetSecuritiesList_body, tmp_call, options = "format")

  Securities <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                           body = upload_file(tmp_call),
                           content_type('application/soap+xml; charset=utf-8'),
                           add_headers(Expect = "100-continue"), verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  return(Securities)

}

#' GetSecurities API Call
#'
#' This function queries the Archway API for information on securities
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @import purrr
#' @export
GetSecuritiesList <- function(username, password, enterpriseID){
  Securities <- GetSecuritiesListRaw(username, password, enterpriseID)

  securities_list <- Securities$content %>%
    xml2::read_xml() %>%
    as_list()

  securities_result <- securities_list$Envelope$Body$GetSecuritiesListResponse$GetSecuritiesListResult$Securities

  securities_df <- tibble(Securities = securities_result) %>%
    unnest_wider(Securities) %>%
    unnest_longer(SecurityIdentifiers) %>%
    unnest_wider(SecurityIdentifiers) %>%
    select(-SecurityClasses) %>%
    unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals, SecurityDefaultTransactionType,
                    SecurityID, SecurityIdentifierName, SecurityIdentifierPrimary,
                    SecurityIdentifierValue, SecurityMultiplier, SecurityName,
                    SecurityReportingCurrency, SecurityType, SecurityTypeID,
                    SecurityTypeName, SecurityNotes)) %>%
    unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals, SecurityDefaultTransactionType,
                    SecurityID, SecurityIdentifierName, SecurityIdentifierPrimary,
                    SecurityIdentifierValue, SecurityMultiplier, SecurityName,
                    SecurityReportingCurrency, SecurityType, SecurityTypeID,
                    SecurityTypeName, SecurityNotes)) %>%
    type_convert(cols(
      CashOrEquivalent = col_logical(),
      NotionalValue = col_logical(),
      RecordIncomeAccruals = col_logical(),
      SecurityDefaultTransactionType = col_character(),
      SecurityID = col_double(),
      SecurityIdentifierName = col_character(),
      SecurityIdentifierPrimary = col_logical(),
      SecurityIdentifierValue = col_character(),
      SecurityIdentifiers_id = col_character(),
      SecurityMultiplier = col_double(),
      SecurityName = col_character(),
      SecurityReportingCurrency = col_character(),
      SecurityType = col_double(),
      SecurityTypeID = col_double(),
      SecurityTypeName = col_character(),
      SecurityNotes = col_character()
    )) %>%
    select(-SecurityIdentifiers_id)


  ##################################################################
  # doc <- Securities$content %>%
  #   xml2::read_xml()
  #
  #
  # (securities_data <- doc %>%
  #    xml2::xml_find_all('.//b:GetSecuritiesListSecurity', ns = xml2::xml_ns(doc)))
  #
  # (securities_rows <- tibble(
  #   row = seq_along(securities_data),
  #   CashOrEquivalent = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:CashOrEquivalent', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   NotionalValue = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:NotionalValue', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   RecordIncomeAccruals = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:RecordIncomeAccruals', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityClasses = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityClasses', ns = xml2::xml_ns(doc))) %>% map(~ xml_children(.x)),
  #   SecurityDefaultTransactionType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityDefaultTransactionType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityMultiplier = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityMultiplier', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityNotes = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityNotes', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityPrimaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityPrimaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityReportingCurrency = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityReportingCurrency', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecuritySecondaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecuritySecondaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityTypeID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityTypeName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityIdentifiers = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:GetSecuritiesListSecurityIdentifier', ns = xml2::xml_ns(doc))) %>% map(~ as_list(.x)) %>% map(~ tibble(data =.x)) %>% map( ~ unnest_wider(data = .x, col = data)) %>% map( ~ unnest(.x, cols = c(SecurityIdentifierName, SecurityIdentifierPrimary, SecurityIdentifierValue)))
  # ))
  #
  # (securities_df <- securities_rows %>%
  #     tidyr::unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals,
  #                     SecurityDefaultTransactionType, SecurityID, SecurityIdentifiers)) %>%
  #     dplyr::select(-SecurityClasses) %>%
  #     tidyr::unnest(cols = c(SecurityMultiplier, SecurityName, SecurityNotes, SecurityPrimaryContactProfileID,
  #                            SecurityReportingCurrency, SecuritySecondaryContactProfileID,
  #                            SecurityType, SecurityTypeID, SecurityTypeName, SecurityIdentifierName,
  #                            SecurityIdentifierPrimary, SecurityIdentifierValue))
  # )
#######################################################################


  return(securities_df)

}


#' GetSecurityClasses API Call
#'
#' This function queries the Archway API for information on security classes
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @import tidyverse
#' @import DBI
#' @import xml2
#' @import dplyr
#' @import purrr
#' @export
GetSecurityClasses <- function(username, password, enterpriseID){
  Securities <- GetSecuritiesListRaw(username, password, enterpriseID)

  securities_list <- Securities$content %>%
    xml2::read_xml() %>%
    as_list()

  securities_result <- securities_list$Envelope$Body$GetSecuritiesListResponse$GetSecuritiesListResult$Securities

  securities_df <- tibble(Securities = securities_result) %>%
    unnest_wider(Securities) %>%
    unnest_longer(SecurityIdentifiers) %>%
    unnest_wider(SecurityIdentifiers) %>%
    unnest_longer(SecurityClasses) %>%
    unnest_wider(SecurityClasses) %>%
    unnest_longer(SecurityClassOptions) %>%
    select(-SecurityClasses_id, -SecurityClassOptions_id, -...1) %>%
    unnest_wider(SecurityClassOptions) %>%
    unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals, SecurityDefaultTransactionType,
                    SecurityID, SecurityIdentifierName, SecurityIdentifierPrimary,
                    SecurityIdentifierValue, SecurityMultiplier, SecurityName,
                    SecurityReportingCurrency, SecurityType, SecurityTypeID,
                    SecurityTypeName, SecurityClassCode, SecurityClassEffectiveDate,
                    SecurityClassID, SecurityClassName, SecurityClassNotes, SecurityClassOptionCode,
                    SecurityClassOptionID, SecurityClassOptionName, SecurityNotes)) %>%
    unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals, SecurityDefaultTransactionType,
                    SecurityID, SecurityIdentifierName, SecurityIdentifierPrimary,
                    SecurityIdentifierValue, SecurityMultiplier, SecurityName,
                    SecurityReportingCurrency, SecurityType, SecurityTypeID,
                    SecurityTypeName, SecurityClassCode, SecurityClassEffectiveDate,
                    SecurityClassID, SecurityClassName, SecurityClassNotes, SecurityClassOptionCode,
                    SecurityClassOptionID, SecurityClassOptionName, SecurityNotes)) %>%
    type_convert(cols(
      .default = col_character(),
      CashOrEquivalent = col_logical(),
      NotionalValue = col_logical(),
      RecordIncomeAccruals = col_logical(),
      SecurityID = col_double(),
      SecurityIdentifierPrimary = col_logical(),
      SecurityMultiplier = col_double(),
      SecurityType = col_double(),
      SecurityTypeID = col_double(),
      SecurityClassEffectiveDate = col_datetime(format = ""),
      SecurityClassID = col_double(),
      SecurityClassOptionCode = col_double(),
      SecurityClassOptionID = col_double()
    )) %>%
    select(-SecurityIdentifiers_id)

  return(securities_df)

}
