#' Delete a calendar
#'
#' Deletes a single calendar. This function operates on the Google
#' Calendar API \code{Calendars} resource. Use requires authorization.
#'
#' @param x A \code{googlecalendar} object representing the calendar to
#'   delete.
#' @template verbose
#'
#' @return Logical indicating whether the calendar was successfully
#'   deleted.
#'
#' @examples
#' \dontrun{
#' gc_summary("Commitments") %>%
#'   gc_delete()
#' }
#'
#' @export
gc_delete <- function(x, verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  url <-
    file.path(.cred$base_url_v3, "calendars", x$id) %>%
    httr::modify_url(query = list(
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::DELETE(url, gc_token()) %>%
    httr::stop_for_status()

  status <- httr::status_code(resp)

  if (status != 204) {
    if (verbose) {
      sprintf("Calendar \"%s\" was not deleted.", x$summary) %>%
        message()
    }
    return(invisible(FALSE))
  }

  if (verbose) {
    sprintf("Calendar \"%s\" was successfully deleted.", x$summary) %>%
      message()
  }

  invisible(TRUE)

}
