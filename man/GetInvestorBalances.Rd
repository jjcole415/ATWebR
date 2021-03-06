% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetInvestorBalances.R
\name{GetInvestorBalances}
\alias{GetInvestorBalances}
\title{GetInvestorBalances API Call}
\usage{
GetInvestorBalances(username, password, enterpriseID, StartDate, EndDate)
}
\arguments{
\item{username}{Username for the API}

\item{password}{Password for the API}

\item{enterpriseID}{Enterprise ID}

\item{StartDate}{Date for beginning of period in yyyy-mm-dd format}

\item{EndDate}{Date for end of period in yyyy-mm-dd format}
}
\description{
This function queries the Archway API for equity account balances from the GL for a given period
}
