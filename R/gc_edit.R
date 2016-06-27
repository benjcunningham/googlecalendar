#' Edit a calendar
#'
#' Enables arbitrary edits to the metadata of a calendar. This function
#' returns an updated \code{googlecalendar} object made available
#' through the Google Calendar API \code{CalendarList} resource. Use
#' requires authorization.
#'
#' This method combines the functionalities of the \code{PUT} methods
#' for both the \code{Calendars} and \code{CalendarList} resources.
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
#'   following: \code{description}, \code{location}, \code{summary},
#'   \code{timeZone}, \code{backgroundColor}, \code{colorId},
#'   \code{defaultReminders}, \code{foregroundColor}, \code{hidden},
#'   \code{notificationSettings}, \code{selected}, and
#'   \code{summaryOverride}.
#' @template verbose
#'
#' @examples
#' \dontrun{
#' gc_new("Clint Meetings") %>%
#'   gc_edit(summary = "Client Meetings", location = "New York")
#' }
#'
#' @export
gc_edit <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  # The Calendars and CalendarList resources (annoyingly) compliment one
  # another in terms of what properties their PATCH methods can edit.
  cal_fields <- c("description", "location", "summary", "timeZone")
  cls_fields <- c("backgroundColor", "colorId", "defaultReminders",
                  "foregroundColor", "hidden", "notificationSettings",
                  "selected", "summaryOverride")

  dots <- list(...)
  cal_dots <- dots[names(dots) %in% cal_fields]
  cls_dots <- dots[names(dots) %in% cls_fields]

  if (length(cal_dots) > 0) {
    path <- file.path("calendars", x$id)
    PATCH_resource(path, body = cal_dots)
  }

  if (length(cls_dots) > 0) {
    path <- file.path("users/me/calendarList", x$id)
    PATCH_resource(path, body = cls_dots)
  }

  cal_out <- gc_id(x$id, verbose = FALSE)

  if (identical(cal_out[names(dots)], dots)) {
    if (verbose) {
      sprintf("Successfully edited calendar: \"%s\"", x$summary) %>%
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
