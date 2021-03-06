% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peloton_api.R
\name{get_peloton_cookie}
\alias{get_peloton_cookie}
\title{Return a cookie string after authenticating with the Peloton API}
\usage{
get_peloton_cookie(username, password)
}
\arguments{
\item{username}{a string, your Peloton account username}

\item{password}{a string, your Peloton account password}
}
\value{
a string, the cookie id
}
\description{
Return a cookie string after authenticating with the Peloton API
}
\examples{

get_peloton_cookie('username','password')

}
