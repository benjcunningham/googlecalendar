#' Create a new calendar event
#'
#' Creates an event in a Google Calendar. This method operates on the
#' Google Calendar API Events resource.
#'
#' For more information on the structure of an \code{event} object, see
#' the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events}{Events Resource Overview}.
#'
#' @param x \code{googlecalendar} object representing the calendar in
#'   which to create the event.
#' @param start,end Named list representing the start or end time. May
#'   include the following properties: \code{date}, \code{dateTime}, and
#'   \code{timeZone}.
#' @template sendNotifications
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request body. More information on these optional values is
#'   available below.
#' @template verbose
#'
#' @return Event metadata as an invisible \code{event} object (a custom
#'   class wrapping a named list).
#'
#' @examples
#' \dontrun{
#' cal <- gc_summary("Commitments")
#'
#' gc_event_new(
#'   cal,
#'   start = list(dateTime = "2016-06-01T09:00:00Z"),
#'   end = list(dateTime = "2016-06-01T10:00:00Z"),
#'   summary = "Lunch with Alyssa"
#' )
#' }
#'
#' @export
gc_event_new <- function(x, start, end, ..., sendNotifications = FALSE,
                         verbose = TRUE) {

  list2env(purrr::map(list(start = start, end = end), function(.x) {
    if (is.list(.x)) {
      .x
    } else {
      nm <- if (inherits(.x, "POSIXct"))
        "dateTime"
      else if (inherits(.x, "Date"))
        "date"

      rlang::list2(!!rlang::sym(nm) := lubridate::format_ISO8601(.x, usetz = TRUE))
    }
  }), environment())
  stopifnot(methods::is(x, "googlecalendar"),
            is.list(start), is.list(end))

  path <- file.path("calendars", x$id, "events")
  body <- c(list(end = end, start = start), list(...))
  resp <- POST_resource(path, body = body,
                        sendNotifications = sendNotifications)

  event_out <- gc_event_id(x, json_content(resp)$id, verbose = FALSE)

  if (methods::is(event_out, "event")) {
    starting <- start$dateTime %||% start$date
    if (verbose) {
      sprintf("Successfully created event starting: %s",
              starting) %>%
        message()
    }
  } else {
    if (verbose) {
      sprintf(
        paste0("Could not confirm the event starting %s was created.\n",
               "The event will not be returned, but you can verify ",
               "the POST using\n`gc_event_query`, `gc_event_ls`, or ",
               "the Google Calendar browser UI."),
        starting) %>%
        message()
    }
    return(invisible(NULL))
  }

  invisible(event_out)

}
