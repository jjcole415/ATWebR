def GetPortfolios_py(username, password, enterpriseID):
  # Get Portfolios Call
  
  
  import pandas as pd
  import requests
  import re
  import datetime
  import xml.etree.ElementTree as ET
  
  # Login Call
  auth_wsdl_url = "https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAuth.svc?singleWsdl"
  api_wsdl_url = "https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAPI.svc?singleWsdl"
  
  host_url = "uat.archwayplatform.seic.com"
  auth_url = "https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAuth.svc"
  api_url = "https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAPI.svc"
  
  
  payload = f"""<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
          <s:Header>
                  <a:Action s:mustUnderstand="1">http://tempuri.org/IATWebWSAuth/ManagerLogin</a:Action>
                  <a:ReplyTo>
                                  <a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
                  </a:ReplyTo>
                  <a:To s:mustUnderstand="1">https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAuth.svc</a:To>
          </s:Header>
          <s:Body>
                  <ManagerLogin xmlns="http://tempuri.org/">
                                  <username>{username}</username>
                                  <password>{password}</password>
                  </ManagerLogin>
          </s:Body>
  </s:Envelope>"""
  
  # print(payload)
  
  headers = {
      'Content-Type': 'application/soap+xml; charset=utf-8'
  }
  
  response = requests.post(url=auth_url, headers=headers, data=payload)
  
  if response:
      print("login successful")
  else:
      print("there was a problem logging in!")
  
  SessionID = re.search("(?<=<b:SessionID>).+(?=</b:SessionID>)", response.text).group()
  UserID = re.search("(?<=<b:UserID>).+(?=</b:UserID>)", response.text).group()
  UTC_time = datetime.datetime.now(datetime.timezone.utc)
  created = UTC_time.strftime(format="%Y-%m-%d")+"T"+UTC_time.strftime(format="%H:%M:%S") + ".000Z"
  sample = "2021-11-02T23:48:13.000Z"
  UTC_time2 = datetime.timedelta(minutes=5)+UTC_time
  ended = UTC_time2.strftime(format="%Y-%m-%d")+"T"+UTC_time2.strftime(format="%H:%M:%S") + ".000Z"
  
  # GetPortfolios Call
  
  
  api_payload = f"""<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing" xmlns:u="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
          	<s:Header>
          		<a:Action s:mustUnderstand="1">http://www.atweb.us/ATWebAPI/IATWebWSAPI/GetPortfolioList</a:Action>
          		<a:ReplyTo>
          			<a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
          		</a:ReplyTo>
          		<a:To s:mustUnderstand="1">https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAPI.svc</a:To>
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
                  <GetPortfolioList xmlns="http://www.atweb.us/ATWebAPI">
          			<enterpriseID>{enterpriseID}</enterpriseID>
          			<entityIDs>
          			    <b>NULL</b>
          			</entityIDs>
          		</GetPortfolioList>
          	</s:Body>
          </s:Envelope>"""
  
  api_response = requests.post(url=api_url, headers=headers, data=api_payload)
  
  text = api_response.text
  content = api_response.content
  namespace = {'b':'http://schemas.datacontract.org/2004/07/Archway.WebServices'}
  
  EntityID = []
  DefaultPortfolio = []
  ExcludeFromDisplay= []
  ExcludeFromRebalance = []
  IncomeOffsetNotional = []
  PortfolioCurrency = []
  PortfolioGLCashAccount = []
  PortfolioGLDueToDueFromAccount = []
  PortfolioGLInvestmentAccount = []
  PortfolioGLNestedEntityCostAccount = []
  PortfolioGLNestedEntityDisparity = []
  PortfolioGLNestedEntityProfitLoss = []
  PortfolioID = []
  PortfolioInventoryMethod = []
  PortfolioName = []
  PrimaryAccountNumberList = []
  PrimaryDataProvider = []
  WashSales = []
  PortfolioNotes = []
  DateOpened = []
  PortfolioLegalName = []
  InternalIdentifier = []
  
  
  root = ET.fromstring(content)
  
  for x in root.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}GetPortfolioListEntity'):
    root1 = ET.Element('root')
    root1 = (x)
    for entity in root1.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}EntityID'):
        EntID = entity.text
        print(EntID)
    for p1 in root1.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}Portfolios'):
        root2 = ET.Element('root')
        root2 = (p1)
        for PortID in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioID'):
            EntityID.append(entity.text)
            PortfolioID.append(PortID.text)
        for PortName in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioName'):
            PortfolioName.append(PortName.text)
        for DefPort in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}DefaultPortfolio'):
            DefaultPortfolio.append(DefPort.text)
        for ExclFromDisp in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}ExcludeFromDisplay'):
            ExcludeFromDisplay.append(ExclFromDisp.text)
        for ExclFromRebal in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}ExcludeFromRebalance'):
            ExcludeFromRebalance.append(ExclFromRebal.text)
        for IncOffNot in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}IncomeOffsetNotional'):
            IncomeOffsetNotional.append(IncOffNot.text)
        for PortCurr in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioCurrency'):
            PortfolioCurrency.append(PortCurr.text)
        for PortGLCash in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLCashAccount'):
            PortfolioGLCashAccount.append(PortGLCash.text)
        for PortDueToDueFrom in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLDueToDueFromAccount'):
            PortfolioGLDueToDueFromAccount.append(PortDueToDueFrom.text)
        for PortGLInvest in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLInvestmentAccount'):
            PortfolioGLInvestmentAccount.append(PortGLInvest.text)
        for PortGLNestEntCost in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLNestedEntityCostAccount'):
            PortfolioGLNestedEntityCostAccount.append(PortGLNestEntCost.text)
        for PortGLNestEntDisp in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLNestedEntityDisparity'):
            PortfolioGLNestedEntityDisparity.append(PortGLNestEntDisp.text)
        for PortGLNestEntProf in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioGLNestedEntityProfitLoss'):
            PortfolioGLNestedEntityProfitLoss.append(PortGLNestEntProf.text)
        for PortInvMethod in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioInventoryMethod'):
            PortfolioInventoryMethod.append(PortInvMethod.text)
        for PrimAcct in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PrimaryAccountNumberList'):
            PrimaryAccountNumberList.append(PrimAcct.text)
        for PrimDataProv in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PrimaryDataProvider'):
            PrimaryDataProvider.append(PrimDataProv.text)
        for WashS in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}WashSales'):
            WashSales.append(WashS.text)
        for PortNotes in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioNotes'):
            PortfolioNotes.append(PortNotes.text)
        for DateOpen in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}DateOpened'):
            DateOpened.append(DateOpen.text)
        for PortLegalName in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}PortfolioLegalName'):
            PortfolioLegalName.append(PortLegalName.text)
        for InternalId in root2.iter('{http://schemas.datacontract.org/2004/07/Archway.WebServices}InternalIdentifier'):
            InternalIdentifier.append(InternalId.text)
  

  df = pd.DataFrame({
          'EntityID' : pd.to_numeric(EntityID),
          'PortfolioID' : pd.to_numeric(PortfolioID),
          'PortfolioName' : PortfolioName,
          'ExcludeFromDisplay' : ExcludeFromDisplay,
          'ExcludeFromRebalance' : ExcludeFromRebalance,
          'IncomeOffsetNotional' : pd.to_numeric(IncomeOffsetNotional),
          'PortfolioCurrency' : PortfolioCurrency,
          'PortfolioGLCashAccount' : pd.to_numeric(PortfolioGLCashAccount),
          'PortfolioGLDueToDueFromAccount' : pd.to_numeric(PortfolioGLDueToDueFromAccount),
          'PortfolioGLInvestmentAccount' : pd.to_numeric(PortfolioGLInvestmentAccount),
          'PortfolioGLNestedEntityCostAccount' : pd.to_numeric(PortfolioGLNestedEntityCostAccount),
          'PortfolioGLNestedEntityDisparity' : pd.to_numeric(PortfolioGLNestedEntityDisparity),
          'PortfolioGLNestedEntityProfitLoss' : pd.to_numeric(PortfolioGLNestedEntityProfitLoss),
          'PortfolioInventoryMethod' : PortfolioInventoryMethod,
          'PrimaryAccountNumberList' : PrimaryAccountNumberList,
          'PrimaryDataProvider' : PrimaryDataProvider,
          'WashSales' : WashSales,
          'PortfolioNotes' : PortfolioNotes,
          'DateOpened' : pd.to_datetime(DateOpened),
          'PortfolioLegalName' : PortfolioLegalName,
          'InternalIdentifier' : InternalIdentifier,
          'UploadDate' : datetime.datetime.now()
                     })  
  # print(df)
  
  
  # Logout Call
  logout_payload = f"""<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
          <s:Header>
                  <a:Action s:mustUnderstand="1">http://tempuri.org/IATWebWSAuth/Logout</a:Action>
                  <a:ReplyTo>
                                  <a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
                  </a:ReplyTo>
                  <a:To s:mustUnderstand="1">https://archwayplatform.seic.com/ATWebWSAPI/ATWebWSAuth.svc</a:To>
          </s:Header>
          <s:Body>
                  <Logout xmlns="http://tempuri.org/">
                                        <token>{SessionID}</token>
                  </Logout>
          </s:Body>
  </s:Envelope>"""
  
  logout_response = requests.post(url=auth_url, headers=headers, data=logout_payload)
  
  if logout_response:
      print("logout successful")
  else:
      print("there was a problem logging out!")
  
  return df
