
#GetGLTransactionsRaw-------------------------------------------------------------------------------------------

#' GetGLTransactionsRaw API Call
#'
#' This function queries the Archway API for information from the General ledger for a given period
#' and returns xml
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
#' @import glue
#' @import tidyr
#' @export
GetGLTransactionsRaw <- function(username, password, enterpriseID, StartDate, EndDate){
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

  # GLTrans_result <- GLTrans$content %>%
  #   read_xml()


  return(GLTrans)
}



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
#' @import purrr
#' @import glue
#' @import tidyr
#' @export
GetGLTransactions <- function(username, password, enterpriseID, StartDate, EndDate){

  GLTrans <- GetGLTransactionsRaw(username, password, enterpriseID, StartDate, EndDate)

  GLTrans_result <- GLTrans$content %>%
    read_xml() %>% as_list()
  GLTrans_list <- GLTrans_result$Envelope$Body$GetGLTransactionsResponse$GetGLTransactionsResult$GLTransactionsEntities
  GLTrans_df <- tibble(GLTrans = GLTrans_list) %>%
    unnest_wider(GLTrans) %>%
    unnest_longer(GLTransactions) %>%
    unnest_wider(GLTransactions) %>%
    unnest_longer(GLTransactionRows) %>%
    unnest_wider(GLTransactionRows) %>%
    # select(-GLTransactionCodeBlocks, -GLTransactionRows_id, -GLTransactions_id) %>%  # removed due to problems with lack of code block data for some period
    select(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
           GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
           GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
           GLSecurityID) %>%
    unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                    GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                    GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                    GLSecurityID)) %>%
    unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                    GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                    GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                    GLSecurityID)) %>%
    type_convert(cols(
      EntityID = col_double(),
      GLID = col_double(),
      GLJournalDate = col_datetime(format = ""),
      GLAccountCode = col_double(),
      GLAccountName = col_character(),
      GLCOAID = col_double(),
      GLCreditAmount = col_double(),
      GLCurrency = col_character(),
      GLDebitAmount = col_double(),
      GLExchangeRate = col_double(),
      GLFunctionCode = col_character(),
      GLNotes = col_character(),
      GLPortfolioID = col_double(),
      GLReconciled = col_logical(),
      GLRowID = col_double(),
      GLSecurityID = col_double()
    )) %>%
    mutate(ts = Sys.time())
  return(GLTrans_df)
}


#GetGLTransactionsCodeBlockDetail-------------------------------------------------------------------------------------------

#' GetGLTransactionsCodeBlockDetail API Call
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
#' @import purrr
#' @import glue
#' @import tidyr
#' @export
GetGLTransactionsCodeBlockDetail <- function(username, password, enterpriseID, StartDate, EndDate){

  GLTrans <- GetGLTransactionsRaw(username, password, enterpriseID, StartDate, EndDate)
  GLTrans_result <- GLTrans$content %>%
    read_xml() %>% as_list()
  GLTrans_list <- GLTrans_result$Envelope$Body$GetGLTransactionsResponse$GetGLTransactionsResult$GLTransactionsEntities

  GLTrans_df1 <- tibble(GLTrans = GLTrans_list) %>%
    unnest_wider(GLTrans) %>%
    unnest_longer(GLTransactions) %>%
    unnest_wider(GLTransactions) %>%
    unnest_longer(GLTransactionRows) %>%
    unnest_wider(GLTransactionRows)
  if("GLTransactionCodeBlocks" %in% c(names(GLTrans_df1))) {
    GLTrans_df2 <- GLTrans_df1 %>%
      unnest_longer(GLTransactionCodeBlocks) %>%
      unnest_wider(GLTransactionCodeBlocks) %>%
      select(-GLTransactionRows_id, -GLTransactions_id, -`...1`, -GLTransactionCodeBlocks_id) %>%
      unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                      GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                      GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                      GLSecurityID, GLCodeBlockID, GLCodeBlockName, GLCodeBlockOptionCode,
                      GLCodeBlockOptionID, GLCodeBlockOptionName, GLCodeBlockSequence)) %>%
      unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                      GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                      GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                      GLSecurityID, GLCodeBlockID, GLCodeBlockName, GLCodeBlockOptionCode,
                      GLCodeBlockOptionID, GLCodeBlockOptionName, GLCodeBlockSequence)) %>%
      type_convert(cols(
        .default = col_double(),
        GLJournalDate = col_datetime(format = ""),
        GLAccountName = col_character(),
        GLCurrency = col_character(),
        GLFunctionCode = col_character(),
        GLNotes = col_character(),
        GLReconciled = col_logical(),
        GLCodeBlockName = col_character(),
        GLCodeBlockOptionCode = col_character(),
        GLCodeBlockOptionName = col_character()
      )) %>%
      mutate(ts = Sys.time())
  } else {
    GLTrans_df2 <- GLTrans_df1 %>%
      select(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
           GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
           GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
           GLSecurityID) %>%
      unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                      GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                      GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                      GLSecurityID)) %>%
      unnest(cols = c(EntityID, GLID, GLJournalDate, GLAccountCode, GLAccountName,
                      GLCOAID, GLCreditAmount, GLCurrency, GLDebitAmount, GLExchangeRate,
                      GLFunctionCode, GLNotes, GLPortfolioID, GLReconciled, GLRowID,
                      GLSecurityID)) %>%
      type_convert(cols(
        EntityID = col_double(),
        GLID = col_double(),
        GLJournalDate = col_datetime(format = ""),
        GLAccountCode = col_double(),
        GLAccountName = col_character(),
        GLCOAID = col_double(),
        GLCreditAmount = col_double(),
        GLCurrency = col_character(),
        GLDebitAmount = col_double(),
        GLExchangeRate = col_double(),
        GLFunctionCode = col_character(),
        GLNotes = col_character(),
        GLPortfolioID = col_double(),
        GLReconciled = col_logical(),
        GLRowID = col_double(),
        GLSecurityID = col_double()
      )) %>%
      mutate(ts = Sys.time())
  }

  return(GLTrans_df2)

}
