# GetSecuritiesList ---------------------------------------------------------------------

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

  doc <- Securities$content %>%
    xml2::read_xml()


  #################################
  (securities_data <- doc %>%
     xml2::xml_find_all('.//b:GetSecuritiesListSecurity', ns = xml2::xml_ns(doc)))

  (securities_rows <- tibble(
    row = seq_along(securities_data),
    CashOrEquivalent = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:CashOrEquivalent', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    NotionalValue = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:NotionalValue', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    RecordIncomeAccruals = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:RecordIncomeAccruals', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityClasses = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityClasses', ns = xml2::xml_ns(doc))) %>% map(~ xml_children(.x)),
    SecurityDefaultTransactionType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityDefaultTransactionType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityMultiplier = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityMultiplier', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityNotes = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityNotes', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityPrimaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityPrimaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityReportingCurrency = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityReportingCurrency', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecuritySecondaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecuritySecondaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityTypeID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityTypeName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityIdentifiers = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:GetSecuritiesListSecurityIdentifier', ns = xml2::xml_ns(doc))) %>% map(~ as_list(.x)) %>% map(~ tibble(data =.x)) %>% map( ~ unnest_wider(data = .x, col = data)) %>% map( ~ unnest(.x, cols = c(SecurityIdentifierName, SecurityIdentifierPrimary, SecurityIdentifierValue)))
  ))

  (securities_df <- securities_rows %>%
      tidyr::unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals,
                      SecurityDefaultTransactionType, SecurityID, SecurityIdentifiers)) %>%
      dplyr::select(-SecurityClasses) %>%
      tidyr::unnest(cols = c(SecurityMultiplier, SecurityName, SecurityNotes, SecurityPrimaryContactProfileID,
                             SecurityReportingCurrency, SecuritySecondaryContactProfileID,
                             SecurityType, SecurityTypeID, SecurityTypeName))
  )

  # (securities_df <- securities_rows %>%
  #     dplyr::mutate(cols = SecurityIdentifiers %>% purrr::map(~ xml2::xml_name(.)),
  #                   vals = SecurityIdentifiers %>% purrr::map(~ xml2::xml_text(.)),
  #                   i = SecurityIdentifiers %>% purrr::map(~ seq_along(.))
  #     ) %>%
  #     dplyr::select(-SecurityClasses, -SecurityIdentifiers) %>%
  #     tidyr::unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals,
  #                            SecurityDefaultTransactionType, SecurityID,
  #                            SecurityMultiplier, SecurityName, SecurityNotes, SecurityPrimaryContactProfileID,
  #                            SecurityReportingCurrency, SecuritySecondaryContactProfileID,
  #                            SecurityType, SecurityTypeID, SecurityTypeName, cols, vals,
  #                            i)) %>%
  #     tidyr::pivot_wider(names_from = cols, values_from = vals,
  #                        id_cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals,
  #                                    SecurityDefaultTransactionType, SecurityID,
  #                                    SecurityMultiplier, SecurityName, SecurityNotes, SecurityPrimaryContactProfileID,
  #                                    SecurityReportingCurrency, SecuritySecondaryContactProfileID,
  #                                    SecurityType, SecurityTypeID, SecurityTypeName)) %>%
  #     # unnest(cols = c()) %>%
  #     readr::type_convert() %>%
  #     dplyr::filter(SecurityIdentifierPrimary == T) %>%
  #     replace_na(list())
  # )

  #################################
  # (securities_data <- doc %>%
  #     xml2::xml_find_all('//b:Securities/b:GetSecuritiesListSecurity', ns = xml2::xml_ns(doc)) %>%
  #     purrr::map(~ xml2::xml_children(.x)))
  #
  # (security_data_rows <- tibble(
  #   row = seq_along(securities_data),
  #   security_nodeset = securities_data,
  #   securityID_nodeset = securities_data %>%
  #     purrr::map(~ xml2::xml_children(.x)) %>%
  #     purrr::map(~ xml2::xml_children(.x))
  # ))
  #
  # (security_data_cells <- security_data_rows %>%
  #     mutate(sec_cols = security_nodeset %>% purrr::map(~ xml2::xml_name(.)),
  #            sec_vals = security_nodeset %>% purrr::map(~ xml2::xml_text(.)),
  #            sec_i = security_nodeset %>% purrr::map(~ seq_along(.))
  #     ) %>%
  #     dplyr::select(row, sec_cols, sec_vals, sec_i) %>%
  #     tidyr::unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
  #     tidyr::pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
  #     readr::type_convert()
  # )
  #
  # (securityID_data_cells <- security_data_rows %>%
  #     dplyr::mutate(sec_cols = securityID_nodeset %>% purrr::map(~ xml2::xml_name(.)),
  #            sec_vals = securityID_nodeset %>% purrr::map(~ xml2::xml_text(.)),
  #            sec_i = securityID_nodeset %>% purrr::map(~ seq_along(.))
  #     ) %>%
  #     dplyr::select(row, sec_cols, sec_vals, sec_i) %>%
  #     tidyr::unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
  #     tidyr::pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
  #     readr::type_convert() %>%
  #     dplyr::select(row, SecurityIdentifierName, SecurityIdentifierPrimary, SecurityIdentifierValue)
  # )
  #
  # securities_df <- full_join(security_data_cells, securityID_data_cells) %>%
  #   tidyr::unnest(cols = c(SecurityIdentifierName, SecurityIdentifierPrimary, SecurityIdentifierValue))  %>%
  #   dplyr::filter(SecurityIdentifierPrimary == "true") %>%
  #   dplyr::select(-SecurityClasses, -SecurityIdentifiers)


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
#' @export
GetSecurityClasses <- function(username, password, enterpriseID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(httr::content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  GetSecuritiesList_body <- glue::glue(
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
  write_xml(GetSecuritiesList_body, tmp_call, options = "format")

  Securities <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                     body = httr::upload_file(tmp_call),
                     httr::content_type('application/soap+xml; charset=utf-8'),
                     httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)


  #################################
  doc <- Securities$content %>%
    xml2::read_xml()

  (securities_data <- doc %>%
     xml2::xml_find_all('.//b:GetSecuritiesListSecurity', ns = xml2::xml_ns(doc)))

  (securities_rows <- tibble(
    row = seq_along(securities_data),
    CashOrEquivalent = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:CashOrEquivalent', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    NotionalValue = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:NotionalValue', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    RecordIncomeAccruals = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:RecordIncomeAccruals', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityClasses = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:GetSecuritiesListSecurityClass', ns = xml2::xml_ns(doc))) %>% map(~ as_list(.x)) %>% map(~ tibble(data =.x)) %>% map( ~ unnest_wider(data = .x, col = data)) %>% map( ~ unnest(.x, )),
    SecurityDefaultTransactionType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityDefaultTransactionType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityMultiplier = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityMultiplier', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityNotes = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityNotes', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityPrimaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityPrimaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityReportingCurrency = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityReportingCurrency', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecuritySecondaryContactProfileID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecuritySecondaryContactProfileID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityType = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityType', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityTypeID = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeID', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityTypeName = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:SecurityTypeName', ns = xml2::xml_ns(doc))) %>% map(~ xml_text(.x)),
    SecurityIdentifiers = securities_data %>%  map(~ xml2::xml_find_all(.x, './/b:GetSecuritiesListSecurityIdentifier', ns = xml2::xml_ns(doc))) %>% map(~ as_list(.x)) %>% map(~ tibble(data =.x)) %>% map( ~ unnest_wider(data = .x, col = data)) %>% map( ~ unnest(.x, cols = c(SecurityIdentifierName, SecurityIdentifierPrimary, SecurityIdentifierValue)))
  ))

  (securities_df <- securities_rows %>%
      tidyr::unnest(cols = c(SecurityIdentifiers)) %>%
      tidyr::unnest(cols = c(CashOrEquivalent, NotionalValue, RecordIncomeAccruals, SecurityClasses,
                             SecurityDefaultTransactionType, SecurityID, SecurityMultiplier,
                             SecurityName, SecurityNotes, SecurityPrimaryContactProfileID,
                             SecurityReportingCurrency, SecuritySecondaryContactProfileID,
                             SecurityType, SecurityTypeID, SecurityTypeName, SecurityIdentifierName,
                             SecurityIdentifierPrimary, SecurityIdentifierValue)) %>%
      tidyr::unnest_wider(SecurityClassOptions) %>%
      tidyr::unnest(cols = c(SecurityClassCode, SecurityClassEffectiveDate, SecurityClassID,
                             SecurityClassName, SecurityClassNotes, SecurityClassOptionCode,
                             SecurityClassOptionID, SecurityClassOptionName)) %>%
      dplyr::filter(SecurityIdentifierPrimary == "true")   %>%
      dplyr::select(PrimaryIdentifier = SecurityIdentifierValue,
                    SecurityID,
                    SecurityName,
                    SecurityType = SecurityTypeName,
                    SecurityClass= SecurityClassName,
                    SecurityClassOption = SecurityClassOptionName,
                    EffectiveDate = SecurityClassEffectiveDate
      ) %>%
      unnest(cols = c(SecurityClassOption)) %>%
      readr::type_convert(cols(
        PrimaryIdentifier = col_character(),
        SecurityID = col_double(),
        SecurityName = col_character(),
        SecurityType = col_character(),
        SecurityClass = col_character(),
        EffectiveDate = col_datetime(format = "")
      )) %>%
      dplyr::group_by(PrimaryIdentifier, SecurityClass) %>%
      dplyr::mutate(HistoricalSetting = case_when(
        is.na(SecurityClassOption) ~ "",
        EffectiveDate == max(EffectiveDate) ~ "FALSE",
        TRUE ~ "TRUE"
      )
  ))
  #################################

  # securities_result <- Securities$content %>%
  #   xml2::read_xml() %>% xml2::as_list()
  # securities_list <- securities_result$Envelope$Body$GetSecuritiesListResponse$GetSecuritiesListResult$Securities
  # securities_df <- tibble(securities = securities_list) %>%
  #   tidyr::unnest_wider(securities) %>%
  #   tidyr::unnest_longer(SecurityClasses) %>%
  #   tidyr::unnest_wider(SecurityClasses) %>%
  #   tidyr::unnest_longer(SecurityClassCode              ) %>%
  #   tidyr::unnest_longer(SecurityClassEffectiveDate     ) %>%
  #   tidyr::unnest_longer(SecurityClassID                ) %>%
  #   tidyr::unnest_longer(SecurityClassName              ) %>%
  #   tidyr::unnest_longer(SecurityClassNotes             ) %>%
  #   tidyr::unnest_longer(SecurityClassOptions           ) %>%
  #   dplyr::select(-...1) %>%
  #   tidyr::unnest_wider(SecurityClassOptions           ) %>%
  #   tidyr::unnest_longer(SecurityClassOptionCode        ) %>%
  #   tidyr::unnest_longer(SecurityClassOptionID          ) %>%
  #   tidyr::unnest_longer(SecurityClassOptionName        ) %>%
  #   tidyr::unnest_longer(CashOrEquivalent) %>%
  #   tidyr::unnest_longer(NotionalValue) %>%
  #   tidyr::unnest_longer(RecordIncomeAccruals) %>%
  #   tidyr::unnest_longer(SecurityDefaultTransactionType) %>%
  #   tidyr::unnest_longer(SecurityDefaultTransactionType) %>%
  #   tidyr::unnest_longer(SecurityID) %>%
  #   tidyr::unnest_longer(SecurityIdentifiers) %>%
  #   tidyr::unnest_wider(SecurityIdentifiers) %>%
  #   tidyr::unnest_longer(SecurityIdentifierName) %>%
  #   tidyr::unnest_longer(SecurityIdentifierPrimary) %>%
  #   tidyr::unnest_longer(SecurityIdentifierValue) %>%
  #   tidyr::unnest_longer(SecurityMultiplier) %>%
  #   tidyr::unnest_longer(SecurityName) %>%
  #   tidyr::unnest_longer(SecurityType) %>%
  #   tidyr::unnest_longer(SecurityTypeID) %>%
  #   tidyr::unnest_longer(SecurityTypeName) %>%
  #   tidyr::unnest_longer(SecurityReportingCurrency) %>%
  #   tidyr::unnest_longer(SecurityNotes) %>%
  #   dplyr::filter(SecurityIdentifierPrimary == "true")   %>%
  #   dplyr::select(PrimaryIdentifier = SecurityIdentifierValue,
  #          SecurityID,
  #          SecurityName,
  #          SecurityType = SecurityTypeName,
  #          SecurityClass= SecurityClassName,
  #          SecurityClassOption = SecurityClassOptionName,
  #          EffectiveDate = SecurityClassEffectiveDate
  #   ) %>%
  #   readr::type_convert(cols(
  #     PrimaryIdentifier = col_character(),
  #     SecurityID = col_double(),
  #     SecurityName = col_character(),
  #     SecurityType = col_character(),
  #     SecurityClass = col_character(),
  #     SecurityClassOption = col_character(),
  #     EffectiveDate = col_datetime(format = "")
  #   )) %>%
  #   dplyr::group_by(PrimaryIdentifier, SecurityClass) %>%
  #   dplyr::mutate(HistoricalSetting = case_when(
  #     is.na(SecurityClassOption) ~ "",
  #     EffectiveDate == max(EffectiveDate) ~ "FALSE",
  #     TRUE ~ "TRUE"
  #   ))

  return(securities_df)

}
