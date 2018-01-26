#' Create request to EcoCounter API
#'
#' @param EcoCounterId [character] or [integer] (**requiered**): Id of the EcoCounter installation
#' @param path [character]: Path to requested data from EcoCounter API. Use c("data", "periode") or
#' c("counter", "channels").
#' @param API_key [character]: API key to get access to EcoCounter API
#' @param query [list]: Querys to get data for specific date. Use with argument path = c("data", "periode").
#'
#' @return [data.frame] with parsed data from EcoCounter API
#'
#' @section Function version 0.0.1
#' @author Johannes Friedrich
#'
#' @md
#' @export

create_request <- function(
  EcoCounterId,
  path = c("data", "periode"),
  API_key = "cw6Xk4jW4X4R",
  query = NULL){

  ##=======================================##
  ## ERROR HANDLING
  ##=======================================##

  if (missing(EcoCounterId))
    stop("[create_request()] Argument 'EcoCounterId' is missing", call. = FALSE)

  if (!(class(unlist(EcoCounterId)) %in% c("character", "numeric")))
    stop("[create_request()] Argument 'EcoCounterId' has to be of class numeric or character", call. = FALSE)

  if (!is.null(query) && class(query) != "list")
    stop("[create_request()] Argument 'query' has to be a list", call. = FALSE)

  ##=======================================##
  ## END ERROR HANDLING
  ##=======================================##

  if (is.numeric(EcoCounterId)) EcoCounterId <- as.numeric(EcoCounterId)

  host <- "http://www.eco-public.com/"

  url <- httr::modify_url(host, path = c("api", API_key, path, EcoCounterId))

  res <- httr::GET(url, query = query)

  if (httr::http_error(res)) {
    content <-  httr::content(res, 'parsed', encoding = 'UTF-8')
    warning(
      if ('message' %in% names(content)) {
        content$message
      } else {
        paste0("Errorcode: ", httr::status_code(res), ". Problem with EcoCounterId: ", EcoCounterId,". EcoCounterId skipped.\n")
      }, call. = FALSE)

    return(NULL)
  } else {

    content <- httr::content(res, "text")

    parsed_request <- jsonlite::fromJSON(content)

    parsed_request$date <- lubridate::ymd_hms(parsed_request$date)

    ##remove timestamp, because coloumn date already inherits this information
    parsed_request$timestamp <- NULL

    return(parsed_request)
  }
}
