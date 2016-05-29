#' List events on a Google Calendar
#'
#' Lists a user-defined subset of events scheduled on a Google Calendar.
#' This function returns event information available via the Google
#' Calendar API \code{Events} resource. Use requires authorization.
#'
#' Only atomic properties are returned in this listing. To fetch all
#' properties for an event, use \code{gc_event}. For more information on
#' fields returned by this function, see the
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events}{Events Resource Overview}
#' in the Google Calendar API reference documentation.
#'
#' @param x A \code{googlecalendar} object representing the calendar for
#'   which to list events.
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request to the API.
#' @template verbose
#'
#' @return An \code{event_ls} object (a custom class wrapping
#'   \code{\link[dplyr]{tbl_df}}) with one row for each event returned
#'   by the service.
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

  url <-
    file.path(.cred$base_url_v3, "calendars", x$id, "events") %>%
    httr::modify_url(query = c(list(...), list(
      fields = itemize_fields(fields),
      key = getOption("googlecalendar.client_key")
    )))

  resp <-
    httr::GET(url, gc_token()) %>%
    httr::stop_for_status()

  ls_raw <- json_content(resp, flatten = TRUE)$items

  if (length(ls_raw) == 0) {
    if (verbose) {
      message("No matching events found.")
    }
    return(invisible(NULL))
  }

  as.event_ls(ls_raw)

}
