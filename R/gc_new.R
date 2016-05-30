#' Create a new calendar
#'
#' Creates a new secondary calendar in the user's list of Google
#' Calendars. If successful, this function returns a
#' \code{googlecalendar} object made available through the Google
#' Calendar API \code{Calendars} resource. Use requires authorization.
#'
#' See \code{\link{googlecalendars}} for a complete description of the
#' information made available in a \code{googlecalendar} object.
#'
#' For more information on optional calendar creation properties or
#' fields returned by this function, see the
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendars/insert}{Calendars: Insert}
#' method overview in the Google Calendar API reference documentation.
#'
#' @param summary Character string representing the title of the
#'   calendar.
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request body to the API. May include the following properties:
#'   \code{description}, \code{etag}, \code{id}, \code{kind},
#'   \code{location}, and \code{timeZone}.
#' @template verbose
#'
#' @return A single-row \code{googlecalendar} object (a custom class
#'   wrapping \code{\link[dplyr]{tbl_df}}) representing metadata for the
#'   newly created calendar.
#'
#' @examples
#' \dontrun{
#' gc_new("R User Group")
#'
#' gc_new("R User Group",
#'        description = "Local user group meetings",
#'        timeZone = "America/Chicago")
#' }
#'
#' @export
gc_new <- function(summary, ..., verbose = TRUE) {

  url <-
    file.path(.cred$base_url_v3, "calendars") %>%
    httr::modify_url(query = list(
      fields = "id",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::POST(url, gc_token(), encode = "json",
               body = c(list(summary = summary), list(...))) %>%
    httr::stop_for_status()

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
