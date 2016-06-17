#' Create a new event
#'
#' Creates an event in a Google Calendar. If successful, this function
#' returns an \code{event} object made available through the Google
#' Calendar API \code{Events} resource. Use requires authorization.
#'
#' See \code{\link{events}} for a complete description of the
#' information made available in an \code{event} object.
#'
#' For more information on optional event creation properties or fields
#' returned by this function, see the
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events/insert}{Events: Insert}
#' method overview in the Google Calendar API reference documentation.
#'
#' @param x \code{googlecalendar} object representing the calendar in
#'   which to create the event.
#' @param start,end Named lists representing the start and end time
#'   properties to be specified on event creation. May include the
#'   following properties: \code{date}, \code{dateTime}, and
#'   \code{timeZone}.
#' @param sendNotifications Logical indicating whether to send
#'   notifications about the creation of the new event.
#' @param \dots Additional parameters to be passed as part of the HTTP request
#'   body to the API. May include the query parameters listed in the
#'   resource documentation. Key properties include: \code{summary},
#'   \code{attendees}, \code{description}, and \code{location}.
#' @template verbose
#'
#' @return An invisible \code{event} object (a custom class wrapping a
#'  list) representing metadata for the newly created event.
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

  url <-
    file.path(.cred$base_url_v3, "calendars", x$id, "events") %>%
    httr::modify_url(query = list(
      sendNotifications = sendNotifications,
      fields = "id",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::POST(url, gc_token(), encode = "json",
               body = c(list(end = end, start = start), list(...))) %>%
    httr::stop_for_status()

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
