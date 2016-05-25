#' Retrieve a Google Calendar
#'
#' Retrieves metadata for a Google Calendar in the form of a
#' \code{googlecalendar} object. This function filters information made
#' available through the Google Calendar API \code{CalendarList}
#' resource. Use requires authorization.
#'
#' A \code{googlecalendar} object will, in cases where the the field is
#' non-\code{NULL}, contain the following information:
#'
#' \itemize{
#'   \item \code{accessRole} Character string representing the effective
#'     access role that the authenticated user has on the calendar.
#'     Possible values are: \code{freeBusyReader}, \code{reader},
#'     \code{writer}, and \code{owner}.
#'   \item \code{backgroundColor} Character string representing the main
#'     color of the calendar in the hexadecimal format \code{#0088aa}.
#'   \item \code{colorId} Character string representing the color of the
#'     calendar using an index-based ID.
#'   \item \code{deleted} Logical indicating whether the calendar has
#'     been deleted from the \code{CalendarList} resource.
#'   \item \code{description} Character string representing the
#'     user-specified description of the calendar.
#'   \item \code{etag} Character string representing the ETag of the
#'     calendar listing in the \code{CalendarList} resource.
#'   \item \code{foregroundColor} Character string representing the
#'     foreground color of the calendar in the hexadecimal format
#'     \code{#ffffff}.
#'   \item \code{hidden} Logical indicating whether the calendar has
#'     been hidden from the list.
#'   \item \code{id} Character string representing the unique identifier
#'     of the calendar.
#'   \item \code{location} Character string representing the geographic
#'     location of the calendar as free-form text.
#'   \item \code{primary} Logical indicating whether the calendar is the
#'     primary calendar of the authenticated user.
#'   \item \code{selected} Logical indicating whether the calendar
#'     content has been selected to show up in the browser-based
#'     calendar UI.
#'   \item \code{summary} Character string representing the title of the
#'     calendar.
#'   \item \code{summaryOverride} Character string representing the
#'     title that the authenticated user has set for the calendar.
#'   \item \code{timeZone} Character string representing the time zone
#'     of the calendar.
#' }
#'
#' @name googlecalendars
#'
#' @param x Calendar-identifying information. In the case of
#'   \code{gc_id}, a character string representing a calendar ID to
#'   which the user has read access. In the case of \code{gc_summary}, a
#'   character string containing a regular expression (or exact
#'   character string for \code{fixed = TRUE}) to be matched to the
#'   calendar title field, \code{summary}.
#' @param fixed Logical indicating if \code{x} should be matched
#'   literally.
#' @param ... Optional arguments to be passed to \code{\link{grep}}.
#' @template verbose
#'
#' @return A single-row \code{googlecalendar} object (a custom class
#'   wrapping \code{\link[dplyr]{tbl_df}}) representing metadata for a
#'   single calendar.
#'
#' @examples
#' \dontrun{
#' gc_id("<26-CHAR-ID>@group.calendar.google.com")
#'
#' gc_summary("ments$")
#' gc_summary("commitments", ignore.case = TRUE)
#' gc_summary("Commitments", fixed = TRUE)
#' }
#'
#' @export
gc_id <- function(x, verbose = TRUE) {

  stopifnot(length(x) == 1, is.character(x))
  gc_lookup(x, "id", fixed = TRUE, verbose = verbose)

}

#' @rdname googlecalendars
#' @export
gc_summary <- function(x, fixed = FALSE, ..., verbose = TRUE) {

  stopifnot(length(x) == 1, is.character(x))
  gc_lookup(x, "summary", fixed = fixed, ..., verbose = verbose)

}

#' @keywords internal
gc_lookup <- function(x, lvar, fixed = FALSE, ..., verbose = TRUE) {

  cals <- gc_ls()

  i <- grep(x, cals[[lvar]], fixed = fixed, ...)

  if (length(i) != 1) {
    sprintf(paste("Found %s calendars where `%s` matches",
                  "\"%s\".\nMust match exactly one calendar."),
            length(i), lvar, x) %>%
      stop(call. = FALSE)
  }

  if (verbose) {
    sprintf("Successfully identified: \"%s\"", cals$summary[i]) %>%
      message()
  }

  look_out <-
    cals[i, ] %>%
    structure(., class = c("googlecalendar", class(.)))

  look_out

}

#' @export
print.googlecalendar <- function(x, ...) {
  x %>%
    dplyr::mutate_each(dplyr::funs_(~ truncate_col(.))) %>%
    print()
}
