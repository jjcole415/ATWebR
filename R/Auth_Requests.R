# Auth Requests -----------------------------------------------------------------------------------

#' Authentication Request
#'
#' This function queries the Archway API to login.
#' It is used at the beginning of other requests to the API
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @import magrittr
#' @import glue
#' @import tidyverse
#' @import httr
#' @export
ATWeb_Auth <- function(username, password) {
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  auth_request <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
        <s:Header>
                <a:Action s:mustUnderstand="1">http://tempuri.org/IATWebWSAuth/ManagerLogin</a:Action>
                <a:ReplyTo>
                                <a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
                </a:ReplyTo>
                <a:To s:mustUnderstand="1">https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc</a:To>
        </s:Header>
        <s:Body>
                <ManagerLogin xmlns="http://tempuri.org/">
                                <username>{username}</username>
                                <password>{password}</password>
                </ManagerLogin>
        </s:Body>
</s:Envelope>') %>%
    xml2::read_xml()

  tmp_auth <- tempfile(fileext = ".xml")
  xml2::write_xml(auth_request, tmp_auth, options = "format")

  # call <- httr::RETRY(verb = "POST",
  #                     url = glue("https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc"),
  #                     body = httr::upload_file(tmp_auth),
  #                     httr::content_type('application/soap+xml; charset=utf-8'),
  #                     httr::verbose(),
  #                     times = 3)

  call <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc"),
               body = httr::upload_file(tmp_auth),
               httr::content_type('application/soap+xml; charset=utf-8'),
               httr::verbose())

  file.remove(tmp_auth)
  return(call)
}


#' Logout Request
#'
#' This function queries the Archway API to logout.
#' It is used at the end of other requests to the API
#' and is neccessary due to the set limits on connections
#'
#' @param username  Username for the API
#' @param password  Password for the API
#' @export
ATWeb_Logout <- function(username, password, SessionID){
  base_URL <- "archwayplatform.seic.com"    # changed from "www.atweb.us" 12/12/2020
  logout_body <- glue::glue(
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
            <s:Header>
                    <a:Action s:mustUnderstand="1">http://tempuri.org/IATWebWSAuth/Logout</a:Action>
                      <a:ReplyTo>
                                      <a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
                      </a:ReplyTo>
                    <a:To s:mustUnderstand="1">https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc</a:To>
            </s:Header>
            <s:Body>
                      <Logout xmlns="http://tempuri.org/">
                                      <token>{SessionID}</token>
                      </Logout>
            </s:Body>
  </s:Envelope>') %>%
    xml2::read_xml()

  tmp_logout <- tempfile(fileext = ".xml")
  xml2::write_xml(logout_body, tmp_logout, options = "format")

  call <- httr::RETRY(verb = "POST",
                      url = glue("https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc"),
                      body = httr::upload_file(tmp_logout),
                      httr::content_type('application/soap+xml; charset=utf-8'),
                      httr::verbose(),
                      times = 3)

  # call <- httr::POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAuth.svc"),
  #              body = httr::upload_file(tmp_auth),
  #              httr::content_type('application/soap+xml; charset=utf-8'),
  #              httr::verbose())
  file.remove(tmp_logout)
  return(call)
}
