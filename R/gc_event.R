#' Retrieve a calendar event
#'
#' Retrieves metadata for a Google Calendar event in the form of an
#' \code{event} object. This function requests information made
#' available through the Google Calendar API \code{Events} resource. Use
#' requires authorization.
#'
#' The \code{gc_event_query} method can accept an arbitrary number of
#' named arguments via the \dots parameter. These values are passed as
#' part of the HTTP request to the \code{Events} resource; queries may
#' include those parameters listed in the
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events/list#request}{Events: list}
#' reference documentation. Notable parameters include:
#'
#' \itemize{
#'   \item{q} Free text search terms to find in any field.
#'   \item{singleEvents} Logical indicating whether to expand recurring
#'     events into instances and only return single one-off events.
#'   \item{timeMax} Exclusive upper bound of the start time filter
#'     (formatted as an RFC3339 timestamp).
#'   \item{timeMin} Inclusive lower bound of the end time filter
#'     (formatted as an RFC3339 timestamp).
#' }
#'
#' For more information on the structure of the \code{Events} resource
#' (and therefore the expected contents of an \code{events} object), see
#' the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events}{Events
#' Overview}.
#'
#' @name events
#'
#' @param x \code{googlecalendar} object representing the calendar for
#'   which to list events.
#' @param id Character string representing the event ID.
#' @template verbose
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request to the API. More information on these named arguments are
#'   available below.
#'
#' @examples
#' \dontrun{
#' cal <- gc_summary("Commitments")
#'
#' gc_event_id(cal, "<ID>")
#' gc_event_query(cal, q = "Lunch with Will")
#' }
#'
#' @export
gc_event_query <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  ls <- gc_event_ls(x, ..., verbose = FALSE)

  if (is.null(ls) || nrow(ls) != 1) {
    if (verbose) {
      matches <- ifelse(!is.null(ls), nrow(ls), 0)
      sprintf(paste("Found %s events matching the query.",
                    "Must match exactly one."),
              matches) %>%
        message()
    }
    return(invisible(NULL))
  }

  gc_event_id(x, ls$id, verbose = verbose)

}

#' @rdname events
#' @export
gc_event_id <- function(x, id, verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  url <-
    file.path(.cred$base_url_v3, "calendars", x$id, "events", id) %>%
    httr::modify_url(query = list(
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::GET(url, gc_token()) %>%
    httr::stop_for_status()

  event <- json_content(resp, flatten = TRUE)
  event$cid <- x$id

  as.event(event)

}
