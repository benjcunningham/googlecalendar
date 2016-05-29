gc_event_query <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  ls <- gc_event_ls(x, ..., verbose = FALSE)

  if (is.null(ls) || nrow(ls) != 1) {
    if (verbose) {
      matches <- ifelse(!is.null(ls), nrow(ls), 0)
      sprintf(paste("Found %s events matching the query.",
                    "Must match exactly one."),
              matches) %>%
        message()
    }
    return(invisible(NULL))
  }

  gc_event_id(x, ls$id, verbose = verbose)

}

gc_event_id <- function(x, id, verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  url <-
    file.path(.cred$base_url_v3, "calendars", x$id, "events", id) %>%
    httr::modify_url(query = list(
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::GET(url, gc_token()) %>%
    httr::stop_for_status()

  event <- json_content(resp, flatten = TRUE)

  as.event(event)

}
