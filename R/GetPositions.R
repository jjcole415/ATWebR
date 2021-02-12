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
  if(status_code(call) == 200) print("success")
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

  # (portfolio_data <- doc %>%
  #     xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio', ns = xml_ns(doc)) %>%
  #     map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))))

  (portfolio_data <- doc %>%
      xml2::xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio', ns = xml2::xml_ns(doc)) %>%
      purrr::map(~ xml2::xml_children(.x)))

  # (security_data <- doc %>%
  #     xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio/b:Securities', ns = xml_ns(doc)) %>%
  #     map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))))

  (security_data <- doc %>%
      xml2::xml_find_all('//b:Entities/b:GetPositionEntity/b:Portfolios/b:GetPositionPortfolio/b:Securities', ns = xml2::xml_ns(doc)) %>%
      purrr::map(~ xml2::xml_children(.x)))

  (portfolio_data_rows <- tidyr::tibble(
    row = seq_along(portfolio_data),
    portfolio_nodeset = portfolio_data,
    security_nodeset = security_data
  ))

  (portfolio_data_cells <- portfolio_data_rows %>%
      dplyr::mutate(port_cols = portfolio_nodeset %>% purrr::map(~ xml2::xml_name(.)),
             port_vals = portfolio_nodeset %>% purrr::map(~ xml2::xml_text(.)),
             port_i = portfolio_nodeset %>% purrr::map(~ seq_along(.))
      ) %>%
      dplyr::select(row, port_cols, port_vals, port_i) %>%
      tidyr::unnest(cols = c(port_cols, port_vals, port_i)) %>%
      tidyr::pivot_wider(names_from = port_cols, values_from = port_vals, id_cols = c(row)) %>%
      readr::type_convert() %>%
      dplyr::select(-Securities)
  )


  (security_data_rows <- dplyr::tibble(
    row = seq_along(security_data),
    security_nodeset = security_data))

  # (security_data_cells <- security_data_rows %>%
  #     mutate(sec_cols = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ xml_name(.)),
  #            sec_vals = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ xml_text(.)),
  #            sec_i = security_nodeset %>% map(~ xml_find_all(.x, xpath = "./b:*", ns = xml_ns(doc))) %>% map(~ seq_along(.))
  #     ) %>%
  #     select(row, sec_cols, sec_vals, sec_i) %>%
  #     unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
  #     pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
  #     unnest(cols = c(Currency, Dividends, EndingExchangeRate, Interest, OutstandingDividends,
  #                     OutstandingInterest, Quantity, SecurityID, SecurityPrimaryID,
  #                     UnitaryBookValue, UnitaryCostBasis, UnitaryTaxBasis, UnrealizedGains)) %>%
  #     type_convert()
  # )

  (security_data_cells <- security_data_rows %>%
      dplyr::mutate(sec_cols = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ xml2::xml_name(.)),
             sec_vals = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ xml2::xml_text(.)),
             sec_i = security_nodeset %>% purrr::map(~ xml2::xml_children(.x)) %>% purrr::map(~ seq_along(.))
      ) %>%
      dplyr::select(row, sec_cols, sec_vals, sec_i) %>%
      tidyr::unnest(cols = c(sec_cols, sec_vals, sec_i)) %>%
      tidyr::pivot_wider(names_from = sec_cols, values_from = sec_vals, id_cols = c(row)) %>%
      tidyr::unnest(cols = c(Currency, Dividends, EndingExchangeRate, Interest, OutstandingDividends,
                      OutstandingInterest, Quantity, SecurityID, SecurityPrimaryID,
                      UnitaryBookValue, UnitaryCostBasis, UnitaryTaxBasis, UnrealizedGains)) %>%
      readr::type_convert()
  )

  securities_df <- GetSecuritiesList(username, password, enterpriseID)
  entities_df <- GetEntities(username, password, enterpriseID) %>% dplyr::select(EntityID, EntityName)
  portfolios_df <- GetPortfolioList(username, password, enterpriseID) %>% dplyr::select(EntityID, PortfolioID, PortfolioName)


  positions_df <- dplyr::left_join(portfolio_data_cells, security_data_cells) %>%
    dplyr::left_join(securities_df) %>%
    dplyr::left_join(portfolios_df) %>%
    dplyr::left_join(entities_df) %>%
    dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date()) %>%
    dplyr::select(-row)


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
                         no = glue('<entityIDs>
                                      <entityID value = "{EntityID}"/>
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
  positions_result <- Positions$content %>%
    read_xml() %>% as_list()

  positions_raw <- positions_result$Envelope$Body$GetPositionsResponse$GetPositionsResult$Entities

  positions_df <- dplyr::tibble(position = positions_raw) %>%
    tidyr::unnest_wider(position) %>%
    tidyr::unnest_longer(EntityID) %>%
    tidyr::unnest_longer(Portfolios) %>%
    tidyr::unnest_wider(Portfolios) %>%
    tidyr::unnest_longer(Securities) %>%
    tidyr::unnest_wider(Securities) %>%
    tidyr::unnest_longer(BeginningCash) %>%
    tidyr::unnest_longer(BeginningDueFrom) %>%
    tidyr::unnest_longer(BeginningDueTo) %>%
    tidyr::unnest_longer(BeginningInvestments) %>%
    tidyr::unnest_longer(BeginningNestedCost) %>%
    tidyr::unnest_longer(BeginningNestedDisparity) %>%
    tidyr::unnest_longer(EndingCash) %>%
    tidyr::unnest_longer(EndingDueFrom) %>%
    tidyr::unnest_longer(EndingDueTo) %>%
    tidyr::unnest_longer(EndingInvestments) %>%
    tidyr::unnest_longer(EndingNestedCost) %>%
    tidyr::unnest_longer(EndingNestedDisparity) %>%
    tidyr::unnest_longer(PortfolioID) %>%
    tidyr::unnest_longer(Quantity) %>%
    tidyr::unnest_longer(RealizedGainsLongTerm) %>%
    tidyr::unnest_longer(RealizedGainsShortTerm) %>%
    tidyr::unnest_longer(Currency) %>%
    tidyr::unnest_longer(Dividends) %>%
    tidyr::unnest_longer(EndingExchangeRate) %>%
    tidyr::unnest_longer(Interest) %>%
    tidyr::unnest_longer(OutstandingDividends) %>%
    tidyr::unnest_longer(OutstandingInterest) %>%
    tidyr::unnest_longer(SecurityID) %>%
    tidyr::unnest_longer(SecurityPrimaryID) %>%
    tidyr::unnest_longer(UnitaryBookValue) %>%
    tidyr::unnest_longer(UnitaryCostBasis) %>%
    tidyr::unnest_longer(UnitaryTaxBasis) %>%
    tidyr::unnest_longer(UnrealizedGains) %>%
    dplyr::select(-`...1`) %>%
    readr::type_convert() %>%
    dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date())


  return(positions_df)
}
