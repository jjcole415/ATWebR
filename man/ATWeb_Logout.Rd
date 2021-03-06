% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Auth_Requests.R
\name{ATWeb_Logout}
\alias{ATWeb_Logout}
\title{Logout Request}
\usage{
ATWeb_Logout(username, password, SessionID)
}
\arguments{
\item{username}{Username for the API}

\item{password}{Password for the API}
}
\description{
This function queries the Archway API to logout.
It is used at the end of other requests to the API
and is neccessary due to the set limits on connections
}
