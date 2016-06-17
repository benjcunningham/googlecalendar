#' List calendars a user can view and modify
#'
#' Lists the collection of calendars in the user's calendar list. This
#' function returns the information available in the Google Calendar API
#' \code{CalendarList} resource. Use requires authorization.
#'
#' This listing gives a partial view of the calendars available to be
#' viewed or modified by the user. Calendars represented in this listing
#' are the subset of available calendars where \emph{Show in List} has
#' been selected in
#' \href{https://calendar.google.com/calendar/render#settings-calendars_9}{Calendar
#' Settings}.
#'
#' @template pattern
#' @param ... Optional arguments to be passed to \code{\link{grep}}.
#' @template verbose
#'
#' @return A \code{googlecalendar_ls} object (a custom class wrapping a
#'   \code{\link[dplyr]{tbl_df}}) where each row represents a calendar.
#'
#' @examples
#' \dontrun{
#' gc_ls()
#'
#' gc_ls("ts$")
#' gc_ls("commitments", ignore.case = TRUE)
#' gc_ls("Commitments", fixed = TRUE)
#' }
#'
#' @export
gc_ls <- function(pattern = NULL, ..., verbose = TRUE) {

  fields <- c("accessRole", "backgroundColor", "colorId", "deleted",
              "description", "etag", "foregroundColor", "hidden", "id",
              "location", "primary", "selected", "summary",
              "summaryOverride", "timeZone")

  resp <- GET_resource("users/me/calendarList",
                       fields = itemize_fields(fields))

  ls_raw <- json_content(resp)$items

  if (!is.null(pattern)) {

    i <- grep(pattern, ls_raw$summary, ...)

    if (length(i) == 0) {
      if (verbose) {
        message("No matching calendars found.")
      }
      return(invisible(NULL))
    }

    ls_raw <- ls_raw[i, ]

  }

  as.googlecalendar_ls(ls_raw)

}
