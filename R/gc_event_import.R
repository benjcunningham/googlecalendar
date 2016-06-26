#' Create new events in bulk
#'
#' Imports a table of new events, adding them to a Google Calendar. If
#' successful, this function returns a vector of event IDs. Use requires
#' authorization.
#'
#' @param x \code{googlecalendar} object representing the calendar in
#'   which to create the event.
#' @param events \code{data.frame} where each row represents an event to
#'   be created.
#' @template sendNotifications
#' @template verbose
#'
#' @return Character vector of the newly created event IDs.
#'
#' @export
gc_event_import <- function(x, events, sendNotifications = FALSE,
                            verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"),
            methods::is(events, "data.frame"))

  path <- file.path("calendars", x$id, "events")

  out <-
    purrr::by_row(events, function(e) {

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
