#' Delete a calendar event
#'
#' Deletes a calendar event. This method operates on the Google Calendar
#' API Events resource.
#'
#' @param x \code{event} object representing the event to delete.
#' @template sendNotifications
#' @template verbose
#'
#' @return Logical indicating whether the event was successfully
#'   deleted.
#'
#' @examples
#' \dontrun{
#' gc_summary("Commitments") %>%
#'   gc_event_query(q = "Lunch with Mitch") %>%
#'   gc_event_delete()
#' }
#'
#' @export
gc_event_delete <- function(x, sendNotifications = FALSE,
                            verbose = TRUE) {

  stopifnot(methods::is(x, "event"))

  path <- file.path("calendars", x$cid, "events", x$id)
  status <- DELETE_resource(path, sendNotifications = sendNotifications)

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
