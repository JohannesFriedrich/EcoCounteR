#' Get location from one EcoCounter installation
#'
#' @param EcoCounterId [character] or [integer] or [vector] of [character] or [integer](**required**):
#' Id of the EcoCounter installation
#' @param from [character](**optional**): Start date (YYYYMMDD). If no date is submitted, the day before yesterday is
#' automatically set.
#' @param to [character](**optional**): End date (YYYYMMDD). If no date is submitted, yesterday is
#' automatically set. Note: When date is the date of today, usually no data are available.
#' @param step [integer](**optional**): Get data : 2 (15 min), 3 (hourly), 4 (daily), 5 (weekly), 6 (monthly), 7 (yearly). With default: 4.
#' @return [data.frame] with parsed data from EcoCounter API
#'
#' @section Function version: 0.0.1
#' @author Johannes Friedrich
#'
#' @examples
#'
#' EcoCounterId <- 100037011
#' get_EcoCounter_data(EcoCounterId)
#'
#' ## set specific date and request weekly data (step = 5)
#' get_EcoCounter_data(EcoCounterId, from = "20171101", to = "20171201", step = 5)
#'
#' @md
#' @export

get_EcoCounter_data <- function(
  EcoCounterId,
  from = NULL,
  to = NULL,
  step = 4){

  ##=======================================##
  ## ERROR HANDLING
  ##=======================================##

  if (missing(EcoCounterId))
    stop("[get_EcoCounter_data()] Argument 'EcoCounterId' is missing", call. = FALSE)

  if (!(class(unlist(EcoCounterId)) %in% c("character", "integer", "numeric")))
    stop("[get_EcoCounter_data()] Argument 'EcoCounterId' has to be of class numeric or character", call. = FALSE)

  if (!(step %in% seq(2,7)))
    stop("[get_EcoCounter_data()] Argument 'step' has to 2 (15 min), 3 (hourly), 4 (daily), 5 (weekly), 6 (monthly) or 7 (yearly)", call. = FALSE)

  if (class(step) != "numeric")
    stop("[get_EcoCounter_data()] Argument 'step' has to be of class numeric", call. = FALSE)

  if (!is.null(from) && class(from) != "character")
    stop("[get_EcoCounter_data()] Argument 'from' has to be of class character", call. = FALSE)

  if (!is.null(to) && class(to) != "character")
    stop("[get_EcoCounter_data()] Argument 'to' has to be of class character", call. = FALSE)

  if (!is.null(from) && length(unlist(strsplit(from, ""))) != 8)
    stop("[get_EcoCounter_data()] Argument 'from' has to be in form 'YYYYMMDD'", call. = FALSE)

  if (!is.null(to) && length(unlist(strsplit(to, ""))) != 8)
    stop("[get_EcoCounter_data()] Argument 'to' has to be in form 'YYYYMMDD'", call. = FALSE)

  ##=======================================##
  ## END ERROR HANDLING
  ##=======================================##

  if (is.null(from)) {
    from <- lubridate::today() - lubridate::days(2)
    from <- strftime(from, format = "%Y%m%d")
  }

  if (is.null(to)) {
    to <- lubridate::today() - lubridate::days(1)
    to <- strftime(to, format = "%Y%m%d")
  }

  if (class(EcoCounterId) == "data.frame") {
    EcoCounterId <- unname(unlist(EcoCounterId))
  }

  query <- list(
    begin = from,
    end = to,
    step = ifelse(is.character(step), step, as.character(step))
  )

  results <- lapply(EcoCounterId, function(x){

    data <- create_request(EcoCounterId = x,
                           query = query)

    if (!is.null(data)) {
      data$id <- as.factor(x)
      return(data)
    } else {
      return(NULL)
    }
  })

  results <- do.call(rbind, results)

  return(results)

}
