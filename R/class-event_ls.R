as.event_ls <- function(x) UseMethod("as.event_ls", x)

as.event_ls.data.frame <- function(x) {

  structure(dplyr::tbl_df(data.frame(
    etag = x$etag,
    id = x$id,
    status = x$status %||% NA_character_,
    htmlLink = x$htmlLink,
    created = x$created, # RFC3339 Timestamp
    updated = x$updated, # RFC3339 Timestamp
    summary = x$summary,
    description = x$description %||% NA_character_,
    location = x$location %||% NA_character_,
    colorId = x$colorId %||% NA_character_,
    creator.id = x$creator.id %||% NA_character_,
    creator.email = x$creator.email %||% NA_character_,
    creator.displayName = x$creator.displayName %||% NA_character_,
    creator.self = x$creator.self %||% FALSE %NA% FALSE,
    organizer.id = x$organizer.id %||% NA_character_,
    organizer.email = x$organizer.email %||% NA_character_,
    organizer.displayName = x$organizer.displayName %||% NA_character_,
    organizer.self = x$organizer.self %||% FALSE %NA% FALSE,
    start.date = x$start.date %||% NA_character_,
    start.dateTime = x$start.dateTime,
    start.timeZone = x$start.timeZone %||% NA_character_,
    end.date = x$end.date %||% NA_character_,
    end.dateTime = x$end.dateTime,
    end.timeZone = x$end.timeZone %||% NA_character_,
    endTimeUnspecified = x$endTimeUnspecified %||% FALSE %NA% FALSE,
    recurringEventId = x$recurringEventId %||% NA_character_,
    originalStartTime.date =
      x$originalStartTime.date %||% NA_character_,
    originalStartTime.dateTime =
      x$originalStartTime.dateTime %||% NA_character_,
    originalStartTime.timeZone =
      x$originalStartTime.timeZone %||% NA_character_,
    transparency = x$transparency %||% "opaque" %NA% "opaque",
    visibility = x$visibility %||% "default" %NA% "default",
    iCalUID = x$iCalUID,
    sequence = x$sequence,
    attendeesOmitted = x$attendeesOmitted %||% FALSE %NA% FALSE,
    hangoutLink = x$hangoutLink %||% NA_character_,
    anyoneCanAddSelf = x$anyoneCanAddSelf %||% FALSE %NA% FALSE,
    guestsCanInviteOthers = x$guestsCanInviteOthers %||% TRUE %NA% TRUE,
    guestsCanModify = x$guestsCanModify %||% FALSE %NA% FALSE,
    guestsCanSeeOtherGuests =
      x$guestsCanSeeOtherGuests %||% TRUE %NA% TRUE,
    privateCopy = x$privateCopy %||% FALSE %NA% FALSE,
    locked = x$locked %||% FALSE %NA% FALSE,
    reminders.useDefault = x$reminders.useDefault %||% TRUE %NA% TRUE,
    source.url = x$source.url %||% NA_character_,
    source.title = x$source.title %||% NA_character_,
    stringsAsFactors = FALSE
  )),
  class = c("event_ls", "tbl_df", "tbl", "data.frame"))

}

#' @export
print.event_ls <- function(x, ...) {
  x %>%
    dplyr::mutate_each(dplyr::funs_(~ truncate_col(.))) %>%
    print(...)
}
