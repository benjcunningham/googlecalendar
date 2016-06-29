#' List events on a Google Calendar
#'
#' Lists events scheduled on a Google Calendar. This method returns
#' event information made available via the Google Calendar API Events
#' resource.
#'
#' An arbitrary number of named arguments may be supplied via the \dots
#' parameter. These values are passed as part of the HTTP request. A
#' list of optional query parameters can be found in
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events/list#parameters}{Events: List}.
#'
#' Notable parameters include:
#'
#' \itemize{
#'   \item{\code{q} -- Free text search terms to find in any field.}
#'   \item{\code{singleEvents} -- Logical indicating whether to expand
#'     recurring events into instances and only return single one-off
#'     events.}
#'   \item{\code{timeMax} -- Exclusive upper bound of the start time
#'     filter (formatted as an RFC3339 timestamp).}
#'   \item{\code{timeMin} -- Inclusive lower bound of the end time
#'     filter (formatted as an RFC3339 timestamp).}
#' }
#'
#' Note that \code{event_ls} objects only represent resource properties
#' that are atomic. To fetch all properties for an event, use
#' \code{gc_event}. For more information on fields returned by this
#' function, see the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events}{Events Resource Overview}.
#'
#' @param x A \code{googlecalendar} object representing the calendar for
#'   which to list events.
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request to the API. More information on these named arguments are
#'   available below.
#' @template verbose
#'
#' @return An \code{event_ls} object (a custom class wrapping a
#'   \code{\link[dplyr]{tbl_df}}) with one row for each event returned
#'   by the service.
#'
#' @examples
#' \dontrun{
#' gc_summary("Commitments") %>%
#'   gc_event_ls()
#' }
#'
#' @export
gc_event_ls <- function(x, ..., verbose = FALSE) {

  stopifnot(methods::is(x, "googlecalendar"))

  fields <- c("anyoneCanAddSelf", "attendeesOmitted", "colorId",
              "created", "creator", "description", "end",
              "endTimeUnspecified", "etag", "guestsCanInviteOthers",
              "guestsCanModify", "guestsCanSeeOtherGuests",
              "hangoutLink", "htmlLink", "iCalUID", "id", "location",
              "locked", "organizer", "originalStartTime", "privateCopy",
              "recurringEventId", "reminders/useDefault", "sequence",
              "source", "start", "status", "summary", "transparency",
              "updated", "visibility")

  path <- file.path("calendars", x$id, "events")
  resp <- GET_resource(path, fields = itemize_fields(fields))

  ls_raw <- json_content(resp, flatten = TRUE)$items

  if (length(ls_raw) == 0) {
    if (verbose) {
      message("No matching events found.")
    }
    return(invisible(NULL))
  }

  as.event_ls(ls_raw)

}
