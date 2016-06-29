#' Edit a calendar event
#'
#' Makes arbitrary edits to the metadata of an event. This method
#' operates on the Google Calendar API Events resource.
#'
#' For more information on properties available for editing, see
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events/insert}{Events: Insert}.
#' in the Google Calendar API reference documentation.
#'
#' @param x \code{event} object representing the event to edit.
#' @param \dots Optional named properties and their new values to be
#'   passed as part of the HTTP request body.
#' @template verbose
#'
#' @return The edited resource as an \code{event} object.
#'
#' @examples
#' \dontrun{
#' gc_new("Bar Trivia") %>%
#'   gc_event_new(
#'     start = list(dateTime = "2016-09-21T20:00:00Z"),
#'     end = list(dateTime = "2016-09-21T23:30:00Z"),
#'     summary = "Dons"
#'   ) %>%
#'   gc_event_edit(summary = "Donnelly's Irish Pub")
#' }
#'
#' @export
gc_event_edit <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "event"))

  dots <- list(...)

  path <- file.path("calendars", x$cid, "events", x$id)
  PATCH_resource(path, body = dots)

  event <- gc_event_lookup(x$cid, x$id, verbose = FALSE)

  if (identical(event[names(dots)], dots)) {
    if (verbose) {
      sprintf("Successfully edited event: \"%s\"", x$summary) %>%
        message()
    }
  } else {
    if (verbose) {
      sprintf(paste0(
        "Could not confirm all edits to \"%s\" were made.\n",
        "The event will be returned, but some changes may have ",
        "failed."), x$summary) %>%
        message()
    }
  }

  invisible(event)

}
