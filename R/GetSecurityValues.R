# GetSecurityValues ---------------------------------------------------------------------

#' GetSecurityValuesRaw API Call
#'
#' This function queries the Archway API for information from the Security Master for a given period, in raw form
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
#' @export
GetSecurityValuesRaw <- function(username, password, enterpriseID, StartDate = "1900-01-01", EndDate = Sys.Date()){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )

  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  xml_body <- glue::glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetSecurityValues</a:Action>
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
        <GetSecurityValues xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
              <StartDate>{StartDate}</StartDate>
              <EndDate>{EndDate}</EndDate>
        		</GetSecurityValues>
        	</s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(xml_body, tmp_call, options = "format")
  Pricing <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                  body = httr::upload_file(tmp_call),
                  httr::content_type('application/soap+xml; charset=utf-8'),
                  httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  return(Pricing)
}

# GetSecurityValues ---------------------------------------------------------------------

#' GetSecurityValues API Call
#'
#' This function queries the Archway API for information from the Security Master for a given period
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
#' @export
GetSecurityValues <- function(username, password, enterpriseID, StartDate = "1900-01-01", EndDate = Sys.Date()){
  Pricing <- GetSecurityValuesRaw(username, password, enterpriseID, StartDate, EndDate)

  pricing_result <- Pricing$content %>%
    xml2::read_xml() %>% as_list()
  pricing_raw <- pricing_result$Envelope$Body$GetSecurityValuesResponse$GetSecurityValuesResult$Securities
  pricing_df <- tibble(Securities = pricing_raw) %>%
    unnest_wider(Securities) %>%
    unnest_longer(SecurityValuesList) %>%
    unnest_wider(SecurityValuesList) %>%
    unnest(cols = c(SecurityID, SecurityPrimaryID, PriceDate, PricePortfolioID,
                    SecurityValue1, SecurityValueNotes)) %>%
    unnest(cols = c(SecurityID, SecurityPrimaryID, PriceDate, PricePortfolioID,
                    SecurityValue1, SecurityValueNotes)) %>%
    type_convert(cols(
      SecurityID = col_double(),
      SecurityPrimaryID = col_character(),
      PriceDate = col_datetime(format = ""),
      PricePortfolioID = col_double(),
      SecurityValue1 = col_double(),
      SecurityValueNotes = col_character(),
      SecurityValuesList_id = col_character()
    )) %>%
    select(SecurityID, SecurityPrimaryID, PriceDate, PricePortfolioID, SecurityValue1,
           SecurityValueNotes) %>%
    dplyr::mutate(UploadDate = Sys.time())


  ################################################
  # doc <- Pricing$content %>%
  #   xml2::read_xml()
  #
  # (pricing_data <- doc %>%
  #     xml2::xml_find_all('.//b:GetSecurity', ns = xml2::xml_ns(doc)))
  #
  # (pricing_rows <- tibble(
  #   row = seq_along(pricing_data),
  #   SecurityID = pricing_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityPrimaryID = pricing_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityPrimaryID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
  #   SecurityValuesList = pricing_data %>%  map(~ xml2::xml_find_all(.x, './/b:GetSecurityValue', ns = xml2::xml_ns(doc))) %>% map(~ xml_children(.x)))
  # )
  #
  # (pricing_df <- pricing_rows %>%
  #     dplyr::mutate(cols = SecurityValuesList %>% purrr::map(~ xml2::xml_name(.)),
  #                   vals = SecurityValuesList %>% purrr::map(~ xml2::xml_text(.)),
  #                   i = SecurityValuesList %>% purrr::map(~ seq_along(.))
  #     ) %>%
  #     dplyr::select(row, SecurityID, SecurityPrimaryID, cols, vals, i) %>%
  #     tidyr::unnest(cols = c(SecurityID, SecurityPrimaryID, cols, vals, i)) %>%
  #     tidyr::pivot_wider(names_from = cols, values_from = vals, id_cols = c(SecurityID, SecurityPrimaryID)) %>%
  #     unnest(cols = c(PriceDate, PricePortfolioID, SecurityAnnualizedPayment, SecurityOutstandingShares,
  #                     SecurityTradingVolume, SecurityValue1, SecurityValue2, SecurityValue3,
  #                     SecurityValueNotes)) %>%
  #     readr::type_convert(col_types = cols(
  #       SecurityID = col_double(),
  #       SecurityPrimaryID = col_character(),
  #       PriceDate = col_datetime(format = ""),
  #       PricePortfolioID = col_double(),
  #       SecurityAnnualizedPayment = col_double(),
  #       SecurityOutstandingShares = col_double(),
  #       SecurityTradingVolume = col_double(),
  #       SecurityValue1 = col_double(),
  #       SecurityValue2 = col_double(),
  #       SecurityValue3 = col_double(),
  #       SecurityValueNotes = col_character()
  #     )) %>%
  #     replace_na(list(PricePortfolioID = 0, SecurityValue2 = 0, SecurityValue3 = 0, SecurityValueNotes = "")) %>%
  #     dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date())
  # )
  ################################################

  return(pricing_df)
}
