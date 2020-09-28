#' @title
#' Get the TGT and Service Ticket for Authentication
#'
#' @description
#' This function retrieves the API Token from the `nih_api_key` variable stored in the Renv file and saves the authentication responses for a TGT and a Service Ticket to ".umls_api_tgt.txt" and ".umls_api_service_ticket.txt" files at the provided destination. TGT expires every 8 hours and the Service Ticket expires after it is used to make an API call or after 4 minutes have passed since it was generated. These time allotments are built into this function to automate the calls for authentication.
#'
#' @seealso
#'  \code{\link[httr]{POST}},\code{\link[httr]{content}},\code{\link[httr]{status_code}}
#'  \code{\link[rvest]{html_nodes}},\code{\link[rvest]{html_text}}
#' @rdname get_service_ticket
#' @noRd
#' @importFrom httr POST content status_code
#' @importFrom rvest html_nodes html_attr html_text


get_service_ticket <-
        function(store_creds_at = "~") {

                tgt_file_path <- file.path(store_creds_at, ".umls_api_tgt.txt")
                service_ticket_file_path <- file.path(store_creds_at, ".umls_api_service_ticket.txt")

                if (file.exists(tgt_file_path)) {

                        if (difftime(Sys.time(), file.info(tgt_file_path)$mtime, units = "hours")  > 8) {

                                proceed <- TRUE

                        } else {

                                proceed <- FALSE

                        }
                } else {
                        proceed <- TRUE
                }


                if (proceed) {

                        auth_response <-
                                httr::POST(
                                        url = "https://utslogin.nlm.nih.gov/cas/v1/api-key",
                                        body = list(apikey = Sys.getenv("nih_api_key")),
                                        encode = "form"
                                )


                        TGT <-
                                httr::content(auth_response, type = "text/html", encoding = "UTF-8") %>%
                                rvest::html_nodes("form") %>%
                                rvest::html_attr("action")

                        cat(TGT,
                            sep = "\n",
                            file = tgt_file_path)

                        tgt_response <-
                                httr::POST(
                                        url = TGT,
                                        body = list(service = "http://umlsks.nlm.nih.gov"),
                                        encode = "form")

                        if (httr::status_code(tgt_response) != 200) {
                                stop(
                                        sprintf(
                                                "API request for Service Ticket failed",
                                                httr::status_code(auth_response)
                                        ),
                                        call. = FALSE
                                )
                        }


                        service_ticket <-
                                tgt_response %>%
                                httr::content(type = "text/html", encoding = "UTF-8") %>%
                                rvest::html_text()

                        cat(service_ticket,
                            sep = "\n",
                            file = service_ticket_file_path)

                } else {

                        TGT <- readLines(tgt_file_path)


                        if (file.exists(service_ticket_file_path)) {
                                if (Sys.time() - file.info(service_ticket_file_path)$mtime >= 4) {
                                        proceed2 <- TRUE
                                } else {
                                        proceed2 <- FALSE
                                }
                        } else {
                                proceed2 <- TRUE
                        }

                        if (proceed2) {

                                tgt_response <-
                                        httr::POST(
                                                url = TGT,
                                                body = list(service = "http://umlsks.nlm.nih.gov"),
                                                encode = "form")

                                if (httr::status_code(tgt_response) != 200) {
                                        stop(
                                                sprintf(
                                                        "API request for Service Ticket failed",
                                                        httr::status_code(auth_response)
                                                ),
                                                call. = FALSE
                                        )
                                }

                                service_ticket <-
                                        tgt_response %>%
                                        httr::content(type = "text/html", encoding = "UTF-8") %>%
                                        rvest::html_text()

                                cat(service_ticket,
                                    sep = "\n",
                                    file = service_ticket_file_path)
                        } else {
                                service_ticket <- readLines(service_ticket_file_path)
                        }

                }

                file.remove(service_ticket_file_path)
                service_ticket
        }
