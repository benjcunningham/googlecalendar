#' List events on a Google Calendar
#'
#' Lists a user-defined subset of events scheduled on a Google Calendar.
#' This function returns event information available via the Google
#' Calendar API \code{Events} resource. Use requires authorization.
#'
#' An \code{event_ls} object will, in cases where a field is
#' non-\code{NULL}, contain the following information:
#'
#' \itemize{
#'   \item{\code{anyoneCanAddSelf}} Logical indicating whether anyone
#'     can invite themselves to the event.
#'   \item{\code{attendeesOmitted}} Logical indicating whether attendees
#'     may have been omitted from the event's representation.
#'   \item{\code{colorId}} Character string representing the color of
#'     the event using an index-based ID.
#'   \item{\code{created}} Character string representing the creation
#'     time of the event (as an
#'     \href{https://tools.ietf.org/html/rfc3339}{RFC3339} timestamp).
#'   \item{\code{creator.displayName}} Character string representing the
#'     name of the creator.
#'   \item{\code{creator.email}} Character string representing the email
#'     of the creator.
#'   \item{\code{creator.id}} Character string representing the Google+
#'     API identifier of the creator.
#'   \item{\code{creator.self}} Logical indicating whether the creator
#'     corresponds to the calendar on which the copy of the event
#'     appears.
#'   \item{\code{description}} Character string representing the
#'     description of the event.
#'   \item{\code{end.date}} Character string representing the day of the
#'     event (formatted as \code{yyyy-mm-dd}), if it is an all-day
#'     activity.
#'   \item{\code{end.dateTime}} Character string representing the end
#'     time of the event (as an
#'     \href{https://tools.ietf.org/html/rfc3339}{RFC3339} timestamp).
#'   \item{\code{end.timeZone}} Character string representing the time
#'     zone of \code{end.dateTime} (formatted as an IANA Time Zone
#'     Database name).
#'   \item{\code{endTimeUnspecified}} Logical indicating whether the end
#'     time is unspecified.
#'   \item{\code{etag}} Character string representing the ETag of the
#'     event listing in the \code{Event} resource.
#'   \item{\code{guestsCanInviteOthers}} Logical indicating whether
#'     attendees other than the organizer can invite others to the
#'     event.
#'   \item{\code{guestsCanModify}} Logical indicating whether attendees
#'     other than the organizer can modify the event.
#'   \item{\code{guestsCanSeeOtherGuests}} Logical indicating whether
#'     attendees other than the organizer can see event attendees.
#'   \item{\code{hangoutLink}} Character string representing a link to
#'     the Google+ hangout associated with the event.
#'   \item{\code{htmlLink}} Character string representing a link to the
#'     event in the Google Calendar Web UI.
#'   \item{\code{iCalUID}} Character string representing a unique
#'     identifier across calendaring systems (as an
#'     \href{RFC5545}{https://tools.ietf.org/html/rfc5545} ID).
#'   \item{\code{id}} Character string representing the unique
#'     identifier of the event.
#'   \item{\code{location}} Character string representing the geographic
#'     location of the event.
#'   \item{\code{locked}} Logical indicating whether the event is locked
#'     such that no changes can be made to the main fields
#'     \code{summary}, \code{description}, \code{location}, \code{end},
#'     \code{start}, or \code{recurrence}.
#'   \item{\code{organizer.displayName}} Character string representing
#'     the organizer's name.
#'   \item{\code{organizer.email}} Character string representing the
#'     organizer's email.
#'   \item{\code{organizer.id}} Character string representing the
#'     Google+ API identifier of the organizer.
#'   \item{\code{organizer.self}} Logical indicating whether the
#'     organizer corresponds to the calendar on which this copy of the
#'     event appears.
#'   \item{\code{originalStartTime.date}} Character string representing
#'     the original day of the event (formatted as \code{yyyy-mm-dd}),
#'     if it is an all-day activity, for a recurring event.
#'   \item{\code{originalStartTime.dateTime}} Character string
#'     representing the original end time of the event (as an
#'     \href{https://tools.ietf.org/html/rfc3339}{RFC3339} timestamp),
#'     for a recurring event.
#'   \item{\code{originalStartTime.timeZone}} Character string
#'     representing the original time zone of an event (formatted as an
#'     IANA Time Zone Database name), for a recurring event.
#'   \item{\code{privateCopy}} Logical indicating whether the event is a
#'     private copy such that changes are not shared with other copies
#'     on other calendars.
#'   \item{\code{recurringEventId}} Character string representing the
#'     unique identifier of a recurring event.
#'   \item{\code{reminders.useDefault}} Logical indicating whether the
#'     default reminders of the calendar apply to the event.
#'   \item{\code{sequence}} Integer representing the iCalendar sequence
#'     number.
#'   \item{\code{source.title}} Character string representing the title
#'     of the source from which the event was created.
#'   \item{\code{source.url}} Character string representing the URL of
#'     the source from which the event was created.
#'   \item{\code{start.date}} Character string representing the day of
#'     the event (formatted as \code{yyyy-mm-dd}), if it is an all-day
#'     activity.
#'   \item{\code{start.dateTime}} Character string representing the
#'     start time of the event (as an
#'     \href{https://tools.ietf.org/html/rfc3339}{RFC3339} timestamp).
#'   \item{\code{start.timeZone}} Character string representing the time
#'     zone of \code{start.dateTime} (formatted as an IANA Time Zone
#'     Database name).
#'   \item{\code{status}} Character string representing the status of an
#'     event. Possible values are: \code{confirmed}, \code{tentative},
#'     and \code{cancelled}.
#'   \item{\code{summary}} Character string representing the title of
#'     the event.
#'   \item{\code{transparency}} Character string representing whether
#'     the event blocks time on the calendar. Possible values are:
#'     \code{opaque} and \code{transparent}.
#'   \item{\code{updated}} Character string representing the last
#'     modification time of the event (as an
#'     \href{https://tools.ietf.org/html/rfc3339}{RFC3339} timestamp).
#'   \item{\code{visibility}} Character string representing the
#'     visibility of the event. Possible values are: \code{default},
#'     \code{public}, \code{private}, \code{confidential}.
#' }
#'
#' For more information on fields returned by this function, see the
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

  stopifnot(nrow(x) == 1, methods::is(x, "googlecalendar"))

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

  events_raw <- json_content(resp, flatten = TRUE)$items

  if (length(events_raw) == 0) {
    if (verbose) {
      message("No matching events found.")
    }
    return(invisible(NULL))
  }

  events_out <-
    dplyr::as.tbl(events_raw) %>%
    dplyr::select_(~ summary,
                   ~ start.dateTime:end.dateTime,
                   ~ everything()) %>%
    structure(., class = c("event_ls", class(.)))

  events_out

}

#' @export
print.event_ls <- function(x, ...) {
  x %>%
    dplyr::mutate_each(dplyr::funs_(~ truncate_col(.))) %>%
    print(...)
}
