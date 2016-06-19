#' Edit an event
#'
#' Enables arbitrary edits to the metadata of an event. This function
#' returns an updated \code{event} object made available through the
#' Google Calendar API \code{Events} resource. Use requires
#' authorization.
#'
#' @param x \code{event} object representing the event to edit.
#' @template verbose
#' @param \dots Optional named properties and their new values to be
#'   passed as part of the HTTP request body to the API. For more
#'   information on available properties, see the
#'   \href{Events: Patch}{https://developers.google.com/google-apps/calendar/v3/reference/events/path}
#'   method overview in the Google Calendar API reference documentation.
#'
#' @return The patched Events resource as an \code{event} object.
#'
#' @export
gc_event_edit <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "event"))

  dots <- list(...)

  path <- file.path("calendars", x$cid, "events", x$id)
  PATCH_resource(path, body = dots)

  cal <- gc_id(x$cid, verbose = FALSE)
  event <- gc_event_id(cal, x$id, verbose = FALSE)

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
