#' Get specific information from one EcoCounter installation
#'
#' @param EcoCounterId [character] or [integer] (**requiered**): Id of the EcoCounter installation
#' @return [data.frame] with parsed data from EcoCounter API
#'
#' @section Function version 0.0.1
#' @author Johannes Friedrich
#'
#' @examples
#'
#' EcoCounterId <- 100020207
#' get_EcoCounter_info(EcoCounterId)
#'
#' @md
#' @export

get_EcoCounter_info <- function(
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

    create_request(EcoCounterId = x,
                   path = c("counter", "channels"),
                   API_key = "cw6Xk4jW4X4R")
  })

  results <- do.call(rbind, results)

  return(results)

}
