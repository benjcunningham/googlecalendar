#' Delete an event
#'
#' Deletes a single calendar event. This function operates on the Google
#' Calendar API \code{Events} resource. Use requires authorization.
#'
#' @param x An \code{event} object representing the calendar to
#'   delete.
#' @param sendNotifications Logical indicating whether to send
#'   notifications about the deletion of the event.
#' @template verbose
#'
#' @return Logical indicating whether the event was successfully
#'   deleted.
#'
#' @examples
#' \dontrun{
#' cal <- gc_summary("Commitments")
#' event <- gc_event_query(cal, q = "Lunch with Mitch")
#'
#' gc_event_delete(event)
#' }
#'
#' @export
gc_event_delete <- function(x, sendNotifications = FALSE,
                            verbose = TRUE) {

  stopifnot(methods::is(x, "event"))

  url <-
    file.path(.cred$base_url_v3, "calendars", x$cid, "events", x$id) %>%
    httr::modify_url(query = list(
      sendNotifications = sendNotifications,
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::DELETE(url, gc_token()) %>%
    httr::stop_for_status()

  status <- httr::status_code(resp)

  if (status != 204) {
    if (verbose) {
      sprintf("Event was not deleted: %s", x$id) %>%
        message()
    }
    return(invisible(FALSE))
  }

  if (verbose) {
    sprintf("Event was successfully deleted: %s", x$id) %>%
      message()
  }

  invisible(TRUE)

}
