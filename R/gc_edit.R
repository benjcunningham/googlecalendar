#' Edit a calendar
#'
#' Enables arbitrary edits to the metadata of a calendar. This function
#' returns an updated \code{googlecalendar} object made available
#' through the Google Calendar API \code{CalendarList} resource. Use
#' requires authorization.
#'
#' This method combines the functionalities of the \code{PUT} methods
#' for the both the \code{Calendars} and \code{CalendarList} resources.
#' For more information on properties available for editing, see the
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendars/update}{Calendars: Update}
#' and
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendarList/update}{CalendarList: Update}
#' methods overviews in the Google Calendar API reference documentation.
#'
#' @param x \code{googlecalendar} object representing the calendar to
#'   edit.
#' @param \dots Optional named properties and their new values to be
#'   passed as part of the HTTP request body to the API. May include the
#'   following properties: \code{description}, \code{location},
#'   \code{summary}, \code{timeZone}, \code{backgroundColor},
#'   \code{colorId}, \code{defaultReminders}, \code{foregroundColor},
#'   \code{hidden}, \code{notificationSettings}, \code{selected}, and
#'   \code{summaryOverride}.
#' @template verbose
#'
#' @examples
#' \dontrun{
#' cal <- gc_new("Work Schedule")
#'
#' gc_edit(cal, summary = "Work", location = "New York")
#' }
#'
#' @export
gc_edit <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  # The Calendars and CalendarList resource (annoyingly) compliment one
  # another in terms of what properties their PUT methods can edit.
  cal_fields <- c("description", "location", "summary", "timeZone")
  cls_fields <- c("backgroundColor", "colorId", "defaultReminders",
                  "foregroundColor", "hidden", "notificationSettings",
                  "selected", "summaryOverride")

  dots <- list(...)
  cal_dots <- dots[names(dots) %in% cal_fields]
  cls_dots <- dots[names(dots) %in% cls_fields]

  # Calendars
  if (length(cal_dots) > 0) {
    cal_resp <-
      file.path(.cred$base_url_v3, "calendars", x$id) %>%
      generalized_put(body = cal_dots)
  }

  # CalendarList
  if (length(cls_dots) > 0) {
    cls_resp <-
      file.path(.cred$base_url_v3, "users/me/calendarList", x$id) %>%
      generalized_put(cls_dots)
  }

  cal_out <- gc_id(x$id, verbose = FALSE)

  if (identical(cal_out[names(dots)], dots)) {
    if (verbose) {
      sprintf("Successfully edited calendar: \"%s\"",
              dots$summary %||% x$summary) %>%
        message()
    }
  } else {
    if (verbose) {
      sprintf(paste0(
        "Could not confirm all edits to \"%s\" were made.\n",
        "The calendar will be returned, but some changes may have ",
        "failed."), x$summary) %>%
        message()
    }
  }

  invisible(cal_out)

}


#' @keywords internal
generalized_put <- function(path, body) {

  url <-
    path %>%
    httr::modify_url(query = list(
      fields = "id",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::PUT(url, gc_token(), encode = "json",
              body = body) %>%
    httr::stop_for_status()

  resp

}
