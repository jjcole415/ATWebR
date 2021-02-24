#GetGLTransactions-------------------------------------------------------------------------------------------

#' GetGLTransactions API Call
#'
#' This function queries the Archway API for information from the General ledger for a given period
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
GetGLTransactions <- function(username, password, enterpriseID, StartDate, EndDate){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )
  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% stringr::str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  GetGLTransactions_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetGLTransactions</a:Action>
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
              <GetGLTransactions xmlns="http://www.atweb.us/ATWebAPI">
        			  <enterpriseID>{enterpriseID}</enterpriseID>
        			  <entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
                    <StartDate>{StartDate}</StartDate>
                    <EndDate>{EndDate}</EndDate>
        		</GetGLTransactions>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  xml2::write_xml(GetGLTransactions_body, tmp_call, options = "format")

  GLTrans <- httr::POST(glue::glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                  body = httr::upload_file(tmp_call),
                  httr::content_type('application/soap+xml; charset=utf-8'),
                  httr::add_headers(Expect = "100-continue"), httr::verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  doc <- GLTrans$content %>%
    xml2::read_xml()



  (GL_data <- doc %>%
     xml2::xml_find_all('.//b:GetGLTransactionsEntity/b:GLTransactions/b:GetGLTransaction', ns = xml2::xml_ns(doc)) %>%
     purrr::map(~xml2::xml_children(.x))
  )

  (GL_data_rows <- dplyr::tibble(
    row = seq_along(GL_data),
    GLTrans_nodeset = GL_data)
  )

  (GLData_df <- GL_data_rows %>%
      dplyr::mutate(GL_cols = GLTrans_nodeset %>% purrr::map(~ xml2::xml_name(.)),
             GL_vals = GLTrans_nodeset %>% purrr::map(~ xml2::xml_text(.)),
             GL_i = GLTrans_nodeset %>% purrr::map(~ seq_along(.)),
      ) %>%
      dplyr::select(row, GL_cols, GL_vals, GL_i) %>%
      tidyr::unnest(cols = c(GL_cols, GL_vals, GL_i)) %>%
      tidyr::pivot_wider(names_from = GL_cols, values_from = GL_vals, id_cols = c(row)) %>%
      readr::type_convert() %>%
      dplyr::as_tibble()
  )

  (GLRows_df <- GL_data_rows %>%
      dplyr::mutate(GLRows_nodeset = GL_data %>% purrr::map(~ xml_children(.x)) %>% purrr::map(~ xml2::xml_children(.x)),
             GL_cols = GLRows_nodeset %>% purrr::map(~ xml2::xml_name(.)),
             GL_vals = GLRows_nodeset %>% purrr::map(~ xml2::xml_text(.)),
             GL_i = GLRows_nodeset %>% purrr::map(~ seq_along(.))
      ) %>%
      dplyr::select(row, GL_cols, GL_vals, GL_i) %>%
      tidyr::unnest(cols = c(GL_cols, GL_vals, GL_i)) %>%
      tidyr::pivot_wider(names_from = GL_cols, values_from = GL_vals, id_cols = c(row)) %>%
      readr::type_convert()
  )

  (GLEntity <- doc %>%
      xml2::xml_find_all('.//b:GetGLTransactionsEntity', ns = xml2::xml_ns(doc))
  )

    (GLEntity_rows <- dplyr::tibble(
      row = seq_along(GLEntity),
      GLEntity_nodeset = GLEntity %>% purrr::map(~ xml2::xml_find_all(.x, './b:EntityID', ns = xml2::xml_ns(doc))),
      GLID_nodeset = GLEntity %>% purrr::map(~ xml2::xml_find_all(.x, './b:GLTransactions/b:GetGLTransaction', ns = xml2::xml_ns(doc))) %>% purrr::map(~ xml_children(.x))  )
    )

  (GLEntity_df <- GLEntity_rows %>%
      dplyr::mutate(EntityID = GLEntity_nodeset %>% purrr::map(~ xml_text(.x)),
             GL_cols = GLID_nodeset %>% purrr::map(~xml2::xml_name(.x)),
             GL_vals = GLID_nodeset %>% purrr::map(~xml2::xml_text(.x))
      ) %>%
      tidyr::unnest(cols = c(EntityID)) %>%
      dplyr::select(row, GL_cols, GL_vals, EntityID) %>%
      tidyr::unnest(cols = c(GL_cols, GL_vals)) %>%
      tidyr::pivot_wider(names_from = GL_cols, values_from = GL_vals, id_cols = c(row, EntityID)) %>%
      tidyr::unnest(cols = c(GLID, GLJournalDate, GLTransactionRows)) %>%
      readr::type_convert() %>%
      dplyr::select(-GLTransactionRows) %>%
      dplyr::rename(entity_row = row) %>%
      dplyr::as_tibble()
  )


  GLTrans_df <- dplyr::full_join(GLData_df, GLRows_df) %>%
    tidyr::unnest(cols = c(GLAccountCode, GLAccountName, GLCOAID, GLCreditAmount, GLCurrency,
                    GLDebitAmount, GLExchangeRate, GLFunctionCode, GLNotes, GLPortfolioID,
                    GLReconciled, GLRowID, GLSecurityID, GLTransactionCodeBlocks)) %>%
    dplyr::select(-GLTransactionRows, -GLTransactionCodeBlocks) %>%
    dplyr::mutate(StartDate = lubridate::as_date(StartDate), EndDate = lubridate::as_date(EndDate), UploadDate = Sys.Date())

  GLData_df %>%
    dplyr::select(row, GLID, GLJournalDate) %>%
    dplyr::left_join(GLEntity_df, by = c("GLID" = "GLID"))


  return(GLTrans_df)
}
