# Calendars    handles:
#   description, location, summary, timeZone
# CalendarList handles:
#   backgroundColor, colorId, defaultReminders, foregroundColor, hidden,
#   notificationSettings, selected, summaryOverride
gc_edit <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  cal_fields <- c("description", "location", "summary", "timeZone")
  cls_fields <- c("backgroundColor", "colorId", "defaultReminders",
                  "foregroundColor", "hidden", "notificationSettings",
                  "selected", "summaryOverride")

  dots <- list(...)
  cal_dots <- dots[names(dots) %in% cal_fields]
  cls_dots <- dots[names(dots) %in% cls_fields]

  # Calendars
  if (length(cal_dots) > 0) {
    cal_resp <-
      file.path(.cred$base_url_v3, "calendars", x$id) %>%
      generalized_put(body = cal_dots)
  }

  # # CalendarList
  if (length(cls_dots) > 0) {
    cls_resp <-
      file.path(.cred$base_url_v3, "users/me/calendarList", x$id) %>%
      generalized_put(cls_dots)
  }

  cal_out <- gc_id(x$id, verbose = FALSE)

  if (identical(cal_out[names(dots)], dots)) {
    if (verbose) {
      sprintf("Successfully edited calendar: \"%s\"",
              dots$summary %||% x$summary) %>%
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

  return(invisible(cal_out))

}


#' @keywords internal
generalized_put <- function(path, body) {

  url <-
    path %>%
    httr::modify_url(query = list(
      fields = "id",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::PUT(url, gc_token(), encode = "json",
              body = body) %>%
    httr::stop_for_status()

  resp

}
