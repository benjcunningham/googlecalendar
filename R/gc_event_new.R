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

#' Event CSV Template
#' @return \code{data.frame}
#' @export
gc_event_csv_template <- function(full = FALSE) {
  if (!full)
    structure(
      list(
        summary = c("Midterm", "Final Exam"),
        start.dateTime = c("2016-09-20T15:00:00Z",
                           "2016-09-21T15:00:00Z"),
        end.dateTime = c("2016-09-20T16:00:00Z",
                         "2016-09-21T18:00:00Z"),
        description = c(
          "25 multiple choice questions",
          "50 multiple choice questions and two essay questions"
        ),
        location = c("S304 PBB",
                     "W10 PBB")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  else
    structure(list(cid = NA, etag = NA, id = NA, status = NA, htmlLink = NA,
                   created = NA, updated = NA, summary = NA, description = NA,
                   location = NA, colorId = NA, creator.id = NA, creator.email = NA,
                   creator.displayName = NA, creator.self = NA, organizer.id = NA,
                   organizer.email = NA, organizer.displayName = NA, organizer.self = NA,
                   start.date = NA, start.dateTime = NA, start.timeZone = NA,
                   end.date = NA, end.dateTime = NA, end.timeZone = NA, endTimeUnspecified = NA,
                   recurrence = NA, recurringEventId = NA, originalStartTime.date = NA,
                   originalStartTime.dateTime = NA, originalStartTime.timeZone = NA,
                   transparency = NA, visibility = NA, iCalUID = NA, sequence = NA,
                   attendees.id = NA, attendees.email = NA, attendees.displayName = NA,
                   attendees.organizer = NA, attendees.self = NA, attendees.resource = NA,
                   attendees.optional = NA, attendees.responseStatus = NA, attendees.comment = NA,
                   attendees.additionalGuests = NA, attendeesOmitted = NA, hangoutLink = NA,
                   gadget.type = NA, gadget.title = NA, gadget.link = NA, gadget.iconLink = NA,
                   gadget.width = NA, gadget.height = NA, gadget.display = NA,
                   anyoneCanAddSelf = NA, guestsCanInviteOthers = NA, guestsCanModify = NA,
                   guestsCanSeeOtherGuests = NA, privateCopy = NA, locked = NA,
                   reminders.useDefault = NA, reminders.overrides.methods = NA,
                   reminders.overrides.minutes = NA, source.url = NA, source.title = NA,
                   attachments.fileUrl = NA, attachments.title = NA, attachments.mimeType = NA,
                   attachments.iconLink = NA, attachments.fileId = NA), class = c("data.frame"), row.names = c(NA, -1L))

}