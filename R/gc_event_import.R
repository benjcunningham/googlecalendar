gc_event_import <- function(x, events, sendNotifications = FALSE,
                            verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"),
            methods::is(events, "data.frame"))

  path <- file.path("calendars", x$id, "events")

  out <-
    purrr::by_row(events, function(e) {
      body <- # COERCE tbl_df ROW TO list
        resp <- POST_resource(path, body = body,
                              sendNotifications = sendNotifications)
      if (methods::is(resp, "character")) {
        sprintf("Successfully created event starting: %s",
                body$start$dateTime %||% body$start$date) %>%
          message()
      } else {
        sprintf("Something went wrong creating event: %s",
                body$start$dateTime %||% body$start$date %||% "???") %>%
          message()
        return(NULL)
      }
      resp
    }, .to = "id") %>%
    dplyr::select(id)

  invisible(out)

}
