% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umls_api_search_functions.R
\name{umls_api_search}
\alias{umls_api_search}
\title{Search the UMLS Metathesaurus}
\usage{
umls_api_search(
  string,
  searchType = "normalizedString",
  inputType = "sourceConcept",
  includeObsolete = NULL,
  includeSuppressible = NULL,
  returnIdType = NULL,
  pageSize = 100,
  sleep_time = 5
)
}
\arguments{
\item{string}{A human readable term, such as ‘gestatational diabetes’, or a code from a source vocabulary, such as 11687002 from SNOMEDCT_US.}

\item{searchType}{(optional)  Type of search you wish to use.}

\item{inputType}{(optional)  Specifies the data type you are using as your search parameter.}

\item{includeObsolete}{(optional)  Return content that is a result of matches on obsolete terms.}

\item{includeSuppressible}{(optional)  Return content that is a result of matches on suppressible terms.}

\item{returnIdType}{(optional)  Specifies the type of identifier you wish to retrieve.}

\item{pageSize}{(optional)  Whole number that specifies the number of results to include per page.}

\item{sabs}{(optional)  Comma-separated list of source vocabularies to include in your search}

\item{pageNumber}{(optional)  Whole number that specifies which page of results to fetch.}
}
\description{
Search the UMLS Metathesaurus
}
\details{
'/search/current?string=fracture of carpal bone'path Retrieves CUIs for a search term and returns a JSON Object classType of searchResults
'/search/current?string=fracture of carpal bone&searchType=exact'path Uses ‘exact’ searching and returns a JSON Object classType of searchResults
'/search/current?string=fracture of carpal bone&sabs=SNOMEDCT_US&returnIdType=code'path Returns SNOMEDCT concepts associated with a search term and returns a JSON Object classType of searchResults
'/search/current?string=9468002&inputType=sourceUi&searchType=exact&sabs=SNOMEDCT_US'path Returns UMLS CUIs associated with a SNOMEDCT_US concept and returns a JSON Object classType of searchResults
}
\seealso{
\code{\link[httr]{GET}},\code{\link[httr]{content}}
\code{\link[jsonlite]{toJSON, fromJSON}}
\code{\link[tibble]{tibble}}
\code{\link[dplyr]{bind}}
}
