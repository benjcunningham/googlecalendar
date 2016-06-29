#' Create a new calendar
#'
#' Creates a new secondary calendar. This method operates on the Google
#' Calendar API Calendars resource.
#'
#' For more information on the structure of a calendar object, see the
#' Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendarList}{CalendarList Resource Overview}.
#'
#' @param summary Character string representing the title of the
#'   calendar.
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request body. More information on these optional values is
#'   available below.
#' @template verbose
#'
#' @return Calendar metadata as an invisible \code{googlecalendar}
#'   object (a custom class wrapping a named list).
#'
#' @examples
#' \dontrun{
#' gc_new(
#'   "R User Group",
#'   description = "Local user group meetings",
#'   timeZone = "America/Chicago"
#' )
#' }
#'
#' @export
gc_new <- function(summary, ..., verbose = TRUE) {

  body <- c(list(summary = summary), list(...))
  resp <- POST_resource("calendars", body = body)

  cal_out <- gc_id(json_content(resp)$id, verbose = FALSE)

  if (methods::is(cal_out, "googlecalendar")) {
    if (verbose) {
      sprintf("Successfully created calendar: \"%s\"", summary) %>%
        message()
    }
  } else {
    if (verbose) {
      sprintf(paste0("Could not confirm that \"%s\" was created.\n",
                    "The calendar will not be returned, but you can ",
                    "verify the POST using `gc_summary`, `gc_ls`, or\n",
                    "the Google Calendar browser UI."),
              summary) %>%
        message()
    }
    return(invisible(NULL))
  }

  invisible(cal_out)

}
