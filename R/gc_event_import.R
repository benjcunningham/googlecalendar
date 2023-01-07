#' Import a table of calendar events
#'
#' Imports a delimited file into Google Calendar, creating one event for
#' every row in a table. This method operates on the Google Calendar API
#' Events resource.
#'
#' In order for \code{gc_event_import} to operate, it must be supplied
#' with a source that complies with the following naming convention for
#' column headers:
#'
#' \itemize{
#'   \item{Each column should represent a single property, with headers
#'     sharing the same name as their \code{event} object counterpart.
#'     For example, the title header would be \code{summary} and the
#'     location header would be \code{location}.}
#'   \item{The path of a nested property is delimited with a \code{.}
#'   (period). For example, the start time header would be
#'   \code{start.dateTime} and the end time zone header would be
#'   \code{end.timeZone}.}
#' }
#'
#' When reading event import tables R, be careful that your input method
#' preserves the header names and does not improperly coerce column
#' types. For example, with \code{read.csv}, you may need to set
#' \code{check.names = FALSE} and with \code{readr}'s \code{read_csv},
#' you may need to set \code{col_types =} \code{cols(.default = "c")}.
#'
#' Note that it may not be possible to represent all properties of an
#' event within the scalar confines of a single CSV-type row (the
#' \code{attendees} sub-structure is an example of one such group of
#' arbitrary-length properties). However, careful preprocessing of an
#' input table may enable you to overcome this restriction. See the
#' project
#' \href{https://github.com/benjcunningham/googlecalendar}{README} on
#' GitHub for some examples of this.
#'
#' @param x \code{googlecalendar} object representing the calendar in
#'   which to create the event.
#' @param events \code{data.frame}-like object or file path to the table
#'   to be imported.
#' @template sendNotifications
#' @template verbose
#'
#' @return Character vector of the newly created event IDs.
#'
#' @examples
#' \dontrun{
#' tbl <- read.csv("sunsets.csv", stringsAsFactors = FALSE)
#'
#' gc_summary("Sunsets") %>%
#'   gc_event_import(tbl)
#' }
#'
#' @export
gc_event_import <- function(x, events, sendNotifications = FALSE,
                            verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"),
            methods::is(events, "data.frame"))

  path <- file.path("calendars", x$id, "events")

  out <-
    purrr::pmap(events, function(...) {
      e <- list(...)
      body <- as.body(e)
      resp <- POST_resource(path, body = body, sendNotifications =
                            unclass(e)[["sendNotifications"]] %||%
                              sendNotifications)
      id <- json_content(resp)$id

      if (methods::is(id, "character")) {
        if (verbose) {
          sprintf("Successfully created event starting: %s",
                  body$start$dateTime %||% body$start$date) %>%
            message()
        }
      } else {
        if (verbose) {
          sprintf("Something went wrong creating event: %s",
                  body$start$dateTime %||% body$start$date %||% "???") %>%
            message()
        }
        return(NA_character_)
      }

      id

    }, .to = "id")

  invisible(unlist(out$id))

}
