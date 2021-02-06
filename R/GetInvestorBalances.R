# GetInvestorBalances ------------------------------------------------------------------------------

GetInvestorBalances <- function(username, password, enterpriseID, StartDate, EndDate){
  call <- ATWeb_Auth(username = username, password = password)
  UserID = stringr::str_extract(content(call, as = "text"), "(?<=<b:UserID>).+(?=</b:UserID>)" )
  SessionID = stringr::str_extract(content(call, as = "text"), "(?<=<b:SessionID>).+(?=</b:SessionID>)" )

  UTC_time <-as.POSIXlt(Sys.time(), format = "%Y-%m-%d%H:%M:%S", tz = "UTC")
  created <- UTC_time %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")
  ended <- (UTC_time + 600) %>% as.character() %>% str_replace(pattern = " ", replacement = "T") %>% paste0(".000Z")

  getInvBal <- glue('
        <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
        	<s:Header>
        		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetInvestorBalances</a:Action>
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
        <GetInvestorBalances xmlns="http://www.atweb.us/ATWebAPI">
        			<enterpriseID>{enterpriseID}</enterpriseID>
        			<entityIDs i:nil="true" xmlns:b="http://schemas.microsoft.com/2003/10/Serialization/Arrays" xmlns:i="http://www.w3.org/2001/XMLSchema-instance"/>
              <StartDate>{StartDate}</StartDate>
              <EndDate>{EndDate}</EndDate>
        		</GetInvestorBalances>
        	</s:Body>
        </s:Envelope>') %>%
    read_xml()

  tmp_call <- tempfile(fileext = ".xml")
  write_xml(getInvBal, tmp_call, options = "format")
  InvestorBalances <- POST(glue("https://{base_URL}/ATWebWSAPI/ATWebWSAPI.svc"),
                           body = upload_file(tmp_call),
                           content_type('application/soap+xml; charset=utf-8'),
                           add_headers(Expect = "100-continue"), verbose())

  ATWeb_Logout(username = username, password = password, SessionID = SessionID)
  file.remove(tmp_call)

  ############################################################################

  doc <- InvestorBalances$content %>%
    read_xml()

  (InvBal_data <- doc %>%
      xml_find_all('.//b:Investor', ns = xml_ns(doc)))

  (InvBal_rows <- tibble(
    row = seq_along(InvBal_data),
    Entity_nodeset = InvBal_data %>% map(~ xml_find_all(.x, './b:EntityID', ns = xml_ns(doc))),
    InvestorBalances_nodeset = InvBal_data %>% map(~ xml_find_all(.x, './b:InvestorBalances/b:InvestorBalance', ns = xml_ns(doc))) %>% map(~ xml_children(.x))  )
  )

  (Entity_df <- InvBal_rows %>%
      mutate(EntityID = Entity_nodeset %>% map(~ xml_text(.x))
      ) %>%
      unnest(cols = c(EntityID)) %>%
      type_convert() %>% select(row, EntityID)
  )

  (InvBal_df <- InvBal_rows %>%
      mutate(cols = InvestorBalances_nodeset %>% map(~xml_name(.x)),
             vals = InvestorBalances_nodeset %>% map(~xml_text(.x))
      ) %>%
      select(row, cols, vals) %>%
      unnest(cols = c(cols, vals)) %>%
      pivot_wider(names_from = cols, values_from = vals, id_cols = c(row)) %>%
      unnest(cols = c(InvestorAssociatedID, InvestorAssociatedIDType, InvestorBookAccount,
                      InvestorDisparityAccount, InvestorEquityAccountName, InvestorEquityAccountNumber,
                      InvestorNav, InvestorOwnershipPercent, InvestorTaxAccount,
                      InvestorUnits, ManagementFeesAccrued, ManagementFeesCharged,
                      PerformanceFeesAccrued, PerformanceFeesCharged)) %>%
      type_convert() %>%
      left_join(Entity_df)
  )



  return(InvBal_df)

}
