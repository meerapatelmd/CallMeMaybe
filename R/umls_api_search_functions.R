#' @title
#' Search
#'
#' @param string			 A human readable term, such as ‘gestatational diabetes’, or a code from a source vocabulary, such as 11687002 from SNOMEDCT_US.
#' @param inputType			 (optional)  Specifies the data type you are using as your search parameter.
#' @param includeObsolete		 (optional)  Return content that is a result of matches on obsolete terms.
#' @param includeSuppressible		 (optional)  Return content that is a result of matches on suppressible terms.
#' @param returnIdType			 (optional)  Specifies the type of identifier you wish to retrieve.
#' @param sabs			         (optional)  Comma-separated list of source vocabularies to include in your search
#' @param searchType			 (optional)  Type of search you wish to use.
#' @param pageNumber			 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			 (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/search/current?string=fracture of carpal bone'path Retrieves CUIs for a search term and returns a JSON Object classType of searchResults
#' '/search/current?string=fracture of carpal bone&searchType=exact'path Uses ‘exact’ searching and returns a JSON Object classType of searchResults
#' '/search/current?string=fracture of carpal bone&sabs=SNOMEDCT_US&returnIdType=code'path Returns SNOMEDCT concepts associated with a search term and returns a JSON Object classType of searchResults
#' '/search/current?string=9468002&inputType=sourceUi&searchType=exact&sabs=SNOMEDCT_US'path Returns UMLS CUIs associated with a SNOMEDCT_US concept and returns a JSON Object classType of searchResults
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}}
#' @rdname umls_api_search
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows

umls_api_search <-
        function(string,
                 searchType = "normalizedString",
                 inputType = "sourceConcept",
                 includeObsolete = NULL,
                 includeSuppressible = NULL,
                 returnIdType = NULL,
                 pageSize = 100,
                 sleep_time = 5) {

                baseURL <- "https://uts-ws.nlm.nih.gov/rest/search/current"

                pageNumber <- 1
                search_response <-
                        httr::GET(url = baseURL,
                                  query = list(pageSize = pageSize,
                                               pageNumber = pageNumber,
                                               string = string,
                                               searchType = searchType,
                                               inputType = inputType,
                                               includeObsolete = includeObsolete,
                                               includeSuppressible = includeSuppressible,
                                               returnIdType = returnIdType,
                                               ticket = get_service_ticket()))

                parsed_response <-
                        search_response %>%
                        httr::content(as = "text") %>%
                        jsonlite::fromJSON()

                output <-
                        tibble::tibble(
                                search_datetime = Sys.time(),
                                string = string,
                                searchType = searchType,
                                classType = parsed_response$result$classType,
                                pageSize = parsed_response$pageSize,
                                pageNumber = parsed_response$pageNumber
                        ) %>%
                        dplyr::bind_cols(
                                parsed_response$result$results %>%
                                        as.data.frame())

                while (!("NONE" %in% output$ui)) {


                        service_ticket <- get_service_ticket()

                        Sys.sleep(sleep_time)

                        pageNumber <- pageNumber+1

                        search_response <-
                                httr::GET(url = baseURL,
                                          query = list(pageSize = pageSize,
                                                       pageNumber = pageNumber,
                                                       string = string,
                                                       searchType = searchType,
                                                       inputType = inputType,
                                                       ticket = service_ticket))

                        parsed_response <-
                                search_response %>%
                                httr::content(as = "text", encoding = "UTF-8") %>%
                                jsonlite::fromJSON()

                        output <-
                                dplyr::bind_rows(output,
                                                tibble::tibble(
                                                        search_datetime = Sys.time(),
                                                        string = string,
                                                        searchType = searchType,
                                                        classType = parsed_response$result$classType,
                                                        pageSize = parsed_response$pageSize,
                                                        pageNumber = parsed_response$pageNumber
                                                ) %>%
                                                dplyr::bind_cols(
                                                        parsed_response$result$results %>%
                                                                as.data.frame()))

                }
                return(output)
        }
