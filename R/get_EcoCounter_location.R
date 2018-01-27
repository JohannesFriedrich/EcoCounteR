#' Get location from an EcoCounter installation
#'
#' @param EcoCounterId [character] or [integer] (**required**): Id of the EcoCounter installation
#' @return [data.frame] with parsed data from EcoCounter API. Coloumns: longitude, lattitude, name, id and idParent.
#' This can be used as input in [leaflet::leaflet], see example.
#'
#' @section Function version 0.0.1
#' @author Johannes Friedrich
#'
#' @examples
#'
#' EcoCounterId <- 100020207
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
    stop("[get_EcoCounter_location()] Argument 'EcoCounterId' is missing", call. = FALSE)

  if (!(class(EcoCounterId) %in% c("character", "numeric", "integer")))
    stop("[get_EcoCounter_location()] Argument 'EcoCounterId' has to be of class numeric or character", call. = FALSE)

  ##=======================================##
  ## END ERROR HANDLING
  ##=======================================##

  results <- lapply(EcoCounterId, function(x){

    data <- get_EcoCounter_info(EcoCounterId = x)

    longitude <- data$longitude
    latitude <- data$latitude
    name <- data$name
    id <- data$id
    idParent <- data$idParent

    return(data.frame(long = longitude,
                      lat = latitude,
                      name = name,
                      id = id,
                      idParent = idParent))


  })

  results <- do.call(rbind, results)

  return(results)

}
