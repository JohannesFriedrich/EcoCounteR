#' Get location from an EcoCounter installation
#'
#' @param EcoCounterId [character] or [integer] (**requiered**): Id of the EcoCounter installation
#' @return [data.frame] with parsed data from EcoCounter API. Coloumns: longitude, lattitude, name. This can be used
#' as input in [leaflet::leaflet], see example.
#'
#' @section Function version 0.0.1
#' @author Johannes Friedrich
#'
#' @examples
#'
#' EcoCounterId <- 101020207
#' location <- get_EcoCounter_location(EcoCounterId)
#'
#' \dontrun{
#' library(leaflet)
#' leaflet(location) %>%
#'   addProviderTiles(providers$OpenStreetMap) %>%
#'   addTiles() %>%
#'   addMarkers(~long, ~lat, popup = ~htmltools::htmlEscape(name))
#' }
#'
#' @md
#' @export

get_EcoCounter_location <- function(
  EcoCounterId){

  ##=======================================##
  ## ERROR HANDLING
  ##=======================================##

  if (missing(EcoCounterId))
    stop("[get_EcoCounter_info()] Argument 'EcoCounterId' is missing", call. = FALSE)

  if (!(class(unlist(EcoCounterId)) %in% c("character", "numeric")))
    stop("[get_EcoCounter_info()] Argument 'EcoCounterId' has to be of class numeric or character", call. = FALSE)

  ##=======================================##
  ## END ERROR HANDLING
  ##=======================================##

  results <- lapply(EcoCounterId, function(x){

    data <- get_EcoCounter_info(EcoCounterId = x)

    longitude <- data$longitude
    latitude <- data$latitude
    name <- data$name

    return(data.frame(long = longitude, lat = latitude, name = name))


  })

  results <- do.call(rbind, results)

  return(results)

}
