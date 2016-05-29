gc_event_lookup <- function(x, lvar, fixed = FALSE, ..., verbose = TRUE) {

  url <-
    file.path(.cred$base_url_v3, "users", "me", "calendarList") %>%
    httr::modify_url(query = list(
      fields = "items",
      key = getOption("googlecalendar.client_key")
    ))

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
