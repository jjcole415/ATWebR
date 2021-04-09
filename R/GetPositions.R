# GetPositions ------------------------------------------------------------------------

#' GetPositions API Call
#'
#' This function queries the Archway API for information on Open Positions. It is programmed to fetch positions for all entities
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
GetPositions <- function(username, password, enterpriseID, StartDate, EndDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  if(httr::status_code(call) == 200) print("success")
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  GetPositions_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
            <s:Header>
            	<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPositions</a:Action>
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
                <GetPositions xmlns="http://www.atweb.us/ATWebAPI">
            		<enterpriseID>{enterpriseID}</enterpriseID>
            		<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
                    <StartDate>{StartDate}</StartDate>
                    <EndDate>{EndDate}</EndDate>
            	</GetPositions>
            </s:Body>
        </s:Envelope>') %>%
    xml2::read_xml()



  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(GetPositions_body, tmp_call, options = "format")
  Positions <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                    body = httr::upload_file(tmp_call),
                    httr::content_type('application/soap+xml; charset=utf-8'),
                    httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  doc <- Positions$content %>%
    xml2::read_xml()


  #######################################
  (positions_data <- doc %>%
     xml2::xml_find_all('.//b:GetPositionPortfolio', ns = xml2::xml_ns(doc)))

  (positions_rows <- tibble(
    row = seq_along(positions_data),
    PortfolioID = map(.x = positions_data, ~ xml2::xml_find_all(.x, './/b:PortfolioID')) %>% map(~ xml_text(.x)),
    Securities = map(.x = positions_data, ~ xml2::xml_find_all(.x, './/b:Securities'))
    ) %>%
      unnest(PortfolioID) %>%
      mutate(GetPositionSecurity = map(.x = Securities, ~ xml2::xml_find_all(.x, './b:GetPositionSecurity'))) %>%
      mutate(Currency = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Currency')) %>% map(~ xml_text(.x)),
             Dividends = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Dividends')) %>% map(~ xml_text(.x)),
             EndingExchangeRate = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:EndingExchangeRate')) %>% map(~ xml_text(.x)),
             Interest = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Interest')) %>% map(~ xml_text(.x)),
             OutstandingDividends = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:OutstandingDividends')) %>% map(~ xml_text(.x)),
             OutstandingInterest = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:OutstandingInterest')) %>% map(~ xml_text(.x)),
             Quantity = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Quantity')) %>% map(~ xml_text(.x)),
             SecurityID = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:SecurityID')) %>% map(~ xml_text(.x)),
             SecurityPrimaryID = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:SecurityPrimaryID')) %>% map(~ xml_text(.x)),
             UnitaryBookValue = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryBookValue')) %>% map(~ xml_text(.x)),
             UnitaryCostBasis = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryCostBasis')) %>% map(~ xml_text(.x)),
             UnitaryTaxBasis = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryTaxBasis')) %>% map(~ xml_text(.x)),
             UnrealizedGains = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnrealizedGains')) %>% map(~ xml_text(.x))
               ) %>%
      select(-Securities, -GetPositionSecurity) %>%
      unnest(cols = c(Currency, Dividends, EndingExchangeRate,
                      Interest, OutstandingDividends, OutstandingInterest, Quantity,
                      SecurityID, SecurityPrimaryID, UnitaryBookValue, UnitaryCostBasis,
                      UnitaryTaxBasis, UnrealizedGains))
  )

  positions_df <- positions_rows %>%
    dplyr::select(-row) %>%
    dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date())
  #######################################

#
#   (portfolio_data <- doc %>%
#       xml2::xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio', ns = xml2::xml_ns(doc)) %>%
#       purrr::map(~ xml2::xml_children(.x)))
#
#   # (security_data <- doc %>%
#   #     xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio/b:Securities', ns = xml_ns(doc)) %>%
#   #     map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))))
#
#   (security_data <- doc %>%
#       xml2::xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio/b:Securities', ns = xml2::xml_ns(doc)) %>%
#       purrr::map(~ xml2::xml_children(.x)))
#
#   (portfolio_data_rows <- tidyr::tibble(
#     row = seq_along(portfolio_data),
#     portfolio_nodeset = portfolio_data,
#     security_nodeset = security_data
#   ))
#
#   (portfolio_data_cells <- portfolio_data_rows %>%
#       dplyr::mutate(port_cols = portfolio_nodeset %>% purrr::map(~ xml2::xml_name(.)),
#              port_vals = portfolio_nodeset %>% purrr::map(~ xml2::xml_text(.)),
#              port_i = portfolio_nodeset %>% purrr::map(~ seq_along(.))
#       ) %>%
#       dplyr::select(row, port_cols, port_vals, port_i) %>%
#       tidyr::unnest(cols = c(port_cols, port_vals, port_i)) %>%
#       tidyr::pivot_wider(names_from = port_cols, values_from = port_vals, id_cols = c(row)) %>%
#       readr::type_convert() %>%
#       dplyr::select(-Securities)
#   )
#
#
#   (security_data_rows <- dplyr::tibble(
#     row = seq_along(security_data),
#     security_nodeset = security_data))
#
#   # (security_data_cells <- security_data_rows %>%
#   #     mutate(sec_cols = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ xml_name(.)),
#   #            sec_vals = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ xml_text(.)),
#   #            sec_i = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ seq_along(.))
#   #     ) %>%
#   #     select(row, sec_cols, sec_vals, sec_i) %>%
#   #     unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
#   #     pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
#   #     unnest(cols = c(Currency, Dividends, EndingExchangeRate, Interest, OutstandingDividends,
#   #                     OutstandingInterest, Quantity, SecurityID, SecurityPrimaryID,
#   #                     UnitaryBookValue, UnitaryCostBasis, UnitaryTaxBasis, UnrealizedGains)) %>%
#   #     type_convert()
#   # )
#
#   (security_data_cells <- security_data_rows %>%
#       dplyr::mutate(sec_cols = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ xml2::xml_name(.)),
#              sec_vals = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ xml2::xml_text(.)),
#              sec_i = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ seq_along(.))
#       ) %>%
#       dplyr::select(row, sec_cols, sec_vals, sec_i) %>%
#       tidyr::unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
#       tidyr::pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
#       tidyr::unnest(cols = c(Currency, Dividends, EndingExchangeRate, Interest, OutstandingDividends,
#                       OutstandingInterest, Quantity, SecurityID, SecurityPrimaryID,
#                       UnitaryBookValue, UnitaryCostBasis, UnitaryTaxBasis, UnrealizedGains)) %>%
#       readr::type_convert()
#   )
#
#   # securities_df <- GetSecuritiesList(username, password, enterpriseID)
#   # entities_df <- GetEntities(username, password, enterpriseID) %>% dplyr::select(EntityID, EntityName)
#   # portfolios_df <- GetPortfolioList(username, password, enterpriseID) %>% dplyr::select(EntityID, PortfolioID, PortfolioName)
#   #
#
#   positions_df <- dplyr::left_join(portfolio_data_cells, security_data_cells) %>%
#     # dplyr::left_join(securities_df) %>%
#     # dplyr::left_join(portfolios_df) %>%
#     # dplyr::left_join(entities_df) %>%
#     dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date()) %>%
#     dplyr::select(-row)


  return(positions_df)
}


#' GetPositionsByEntity API Call
#'
#' This function queries the Archway API for information on Open Positions. It is programmed to fetch positions for a single entity
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @param enterpriseID  Enterprise ID
#' @param EntityID  Archway Entity ID
#' @param StartDate  Date for beginning of period in yyyy-mm-dd format
#' @param EndDate  Date for end of period in yyyy-mm-dd format
#' @import tidyverse
#' @import DBI
#' @import xml2
#' @import stringr
#' @import dplyr
#' @import readr
#' @import purrr
#' @export
GetPositionsByEntity <- function(username, password, enterpriseID, EntityID, StartDate, EndDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  param_entity <- ifelse(EntityID == 0,
                         yes = glue('<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>'),
                         no = glue('<entityIDs i:nil="false" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
                                      <b:int>{EntityID}</b:int>>
                                   </entityIDs>'))
  GetPositions_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
            <s:Header>
            	<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPositions</a:Action>
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
                <GetPositions xmlns="http://www.atweb.us/ATWebAPI">
            		<enterpriseID>{enterpriseID}</enterpriseID>
                {param_entity}
                <StartDate>{StartDate}</StartDate>
                <EndDate>{EndDate}</EndDate>
            	</GetPositions>
            </s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(GetPositions_body, tmp_call, options = "format")
  Positions <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                    body = httr::upload_file(tmp_call),
                    httr::content_type('application/soap+xml; charset=utf-8'),
                    httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  #######################################

  doc <- Positions$content %>%
    xml2::read_xml()
  (positions_data <- doc %>%
     xml2::xml_find_all('.//b:GetPositionPortfolio', ns = xml2::xml_ns(doc)))

  (positions_rows <- tibble(
    row = seq_along(positions_data),
    PortfolioID = map(.x = positions_data, ~ xml2::xml_find_all(.x, './/b:PortfolioID')) %>% map(~ xml_text(.x)),
    Securities = map(.x = positions_data, ~ xml2::xml_find_all(.x, './/b:Securities'))
  ) %>%
      unnest(PortfolioID) %>%
      mutate(GetPositionSecurity = map(.x = Securities, ~ xml2::xml_find_all(.x, './b:GetPositionSecurity'))) %>%
      mutate(Currency = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Currency')) %>% map(~ xml_text(.x)),
             Dividends = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Dividends')) %>% map(~ xml_text(.x)),
             EndingExchangeRate = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:EndingExchangeRate')) %>% map(~ xml_text(.x)),
             Interest = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Interest')) %>% map(~ xml_text(.x)),
             OutstandingDividends = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:OutstandingDividends')) %>% map(~ xml_text(.x)),
             OutstandingInterest = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:OutstandingInterest')) %>% map(~ xml_text(.x)),
             Quantity = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:Quantity')) %>% map(~ xml_text(.x)),
             SecurityID = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:SecurityID')) %>% map(~ xml_text(.x)),
             SecurityPrimaryID = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:SecurityPrimaryID')) %>% map(~ xml_text(.x)),
             UnitaryBookValue = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryBookValue')) %>% map(~ xml_text(.x)),
             UnitaryCostBasis = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryCostBasis')) %>% map(~ xml_text(.x)),
             UnitaryTaxBasis = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnitaryTaxBasis')) %>% map(~ xml_text(.x)),
             UnrealizedGains = map(.x = GetPositionSecurity, ~ xml2::xml_find_all(.x, './b:UnrealizedGains')) %>% map(~ xml_text(.x))
      ) %>%
      select(-Securities, -GetPositionSecurity) %>%
      unnest(cols = c(Currency, Dividends, EndingExchangeRate,
                      Interest, OutstandingDividends, OutstandingInterest, Quantity,
                      SecurityID, SecurityPrimaryID, UnitaryBookValue, UnitaryCostBasis,
                      UnitaryTaxBasis, UnrealizedGains))
  )

  positions_df <- positions_rows %>%
    dplyr::select(-row) %>%
    dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date())
  #######################################

  return(positions_df)
}
