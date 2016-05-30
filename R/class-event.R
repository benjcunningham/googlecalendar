event <- function() {

  structure(list(
    kind = "calendar#event",
    cid = character(),
    etag = character(),
    id = character(),
    status = character(),
    htmlLink = character(),
    created = character(), # datetime
    updated = character(), # datetime
    summary = character(),
    description = character(),
    location = character(),
    colorId = character(),
    creator = list(
      id = character(),
      email = character(),
      displayName = character(),
      self = logical()
    ),
    organizer = list(
      id = character(),
      email = character(),
      displayName = character(),
      self = logical()
    ),
    start = list(
      date = character(), # date
      dateTime = character(), # datetime
      timeZone = character()
    ),
    end = list(
      date = character(), # date
      dateTime = character(), # datetime
      timeZone = character()
    ),
    endTimeUnspecified = logical(),
    recurrence = character(),
    recurringEventId = character(),
    originalStartTime = list(
      date = character(), # date
      dateTime = character(), # datetime
      timeZone = character()
    ),
    transparency = character(),
    visibility = character(),
    iCalUID = character(),
    sequence = integer(),
    attendees = list(
      id = character(),
      email = character(),
      displayName = character(),
      organizer = logical(),
      self = logical(),
      resource = logical(),
      optional = logical(),
      responseStatus = character(),
      comment = character(),
      additionalGuests = integer()
    ),
    attendeesOmitted = logical(),
    hangoutLink = character(),
    gadget = list(
      type = character(),
      title = character(),
      link = character(),
      iconLink = character(),
      width = integer(),
      height = integer(),
      display = character()
    ),
    anyoneCanAddSelf = logical(),
    guestsCanInviteOthers = logical(),
    guestsCanModify = logical(),
    guestsCanSeeOtherGuests = logical(),
    privateCopy = logical(),
    locked = logical(),
    reminders = list(
      useDefault = logical(),
      overrides = list(
        methods = character(),
        minutes = integer()
      )
    ),
    source = list(
      url = character(),
      title = character()
    ),
    attachments = list(
      fileUrl = character(),
      title = character(),
      mimeType = character(),
      iconLink = character(),
      fileId = character()
    )
  ),
  class = c("event", "list"))

}

as.event <- function(x) UseMethod("as.event", x)

as.event.list <- function(x) {

  ev <- event()

  ev$cid <- x$cid
  ev$etag <- x$etag
  ev$id <- x$id
  ev$status <- x$status %||% "confirmed"
  ev$htmlLink <- x$htmlLink
  ev$created <- x$created # RFC3339
  ev$updated <- x$updated # RFC3339
  ev$summary <- x$summary
  ev$description <- x$description %||% NA_character_
  ev$location <- x$location %||% NA_character_
  ev$colorId <- x$colorId %||% NA_character_
  ev$creator$id <- x$creator$id %||% NA_character_
  ev$creator$email <- x$creator$email %||% NA_character_
  ev$creator$displayName <- x$creator$displayName %||% NA_character_
  ev$creator$self <- x$creator$self %||% FALSE
  ev$organizer$id <- x$organizer$id %||% NA_character_
  ev$organizer$email <- x$organizer$email %||% NA_character_
  ev$organizer$displayName <- x$organizer$displayName %||% NA_character_
  ev$organizer$self <- x$organizer$self %||% FALSE
  ev$start$date <- x$start$date %||% NA_character_ # yyyy-mm-dd
  ev$start$dateTime <- x$start$dateTime %||% NA_character_ # RFC3339
  ev$start$timeZone <- x$start$timeZone %||% NA_character_
  ev$end$date <- x$end$date %||% NA_character_ # yyyy-mm-dd
  ev$end$dateTime <- x$end$dateTime %||% NA_character_ # RFC3339
  ev$end$timeZone <- x$end$timeZone %||% NA_character_
  ev$endTimeUnspecified <- x$endTimeUnspecified %||% FALSE
  ev$recurrence <- x$recurrence %||% NA_character_
  ev$recurringEventId <- x$recurringEventId %||% NA_character_
  ev$originalStartTime$date <-
    x$originalStartTime$date %||% NA_character_ # yyyy-mm-dd
  ev$originalStartTime$dateTime <-
    x$originalStartTime$dateTime %||% NA_character_ # RFC3339
  ev$originalStartTime$timeZone <-
    x$originalStartTime$timeZone %||% NA_character_
  ev$transparency <- x$transparency %||% "opaque"
  ev$visibility <- x$visibility %||% "default"
  ev$iCalUID <- x$iCalUID
  ev$sequence <- x$sequence
  ev$attendees$id <- x$attendees$id %||% NA_character_
  ev$attendees$email <- x$attendees$email %||% NA_character_
  ev$attendees$displayName <- x$attendees$displayName %||% NA_character_
  ev$attendees$organizer <- x$attendees$organizer %||% FALSE
  ev$attendees$self <- x$attendees$self %||% FALSE
  ev$attendees$resource <- x$attendees$resource %||% FALSE
  ev$attendees$optional <- x$attendees$optional %||% FALSE
  ev$attendees$responseStatus <-
    x$attendees$responseStatus %||% NA_character_
  ev$attendees$comment <- x$attendees$comment %||% NA_character_
  ev$attendees$additionalGuests <- x$attendees$additionalGuests %||% 0
  ev$attendeesOmitted <- x$attendeesOmitted %||% FALSE
  ev$hangoutLink <- x$hangoutLink %||% NA_character_
  ev$gadget$type <- x$gadget$type %||% NA_character_
  ev$gadget$title <- x$gadget$title %||% NA_character_
  ev$gadget$link <- x$gadget$link %||% NA_character_
  ev$gadget$iconLink <- x$gadget$iconLink %||% NA_character_
  ev$gadget$width <- x$gadget$width %||% NA_character_
  ev$gadget$height <- x$gadget$height %||% NA_character_
  ev$gadget$display <- x$gadget$display %||% NA_character_
  ev$anyoneCanAddSelf <- x$anyoneCanAddSelf %||% FALSE
  ev$guestsCanInviteOthers <- x$guestsCanInviteOthers %||% TRUE
  ev$guestsCanModify <- x$guestsCanModify %||% FALSE
  ev$guestsCanSeeOtherGuests <- x$guestsCanSeeOtherGuests %||% TRUE
  ev$privateCopy <- x$privateCopy %||% FALSE
  ev$locked <- x$locked %||% FALSE
  ev$reminders$useDefault <- x$reminders$useDefault %||% TRUE
  ev$reminders$overrides$methods <-
    x$reminders$overrides$methods %||% NA_character_
  ev$reminders$overrides$minutes <-
    x$reminders$overrides$minutes %||% NA_integer_
  ev$source$url <- x$source$url %||% NA_character_
  ev$source$title <- x$source$title %||% NA_character_
  ev$attachments$fileUrl <- x$attachments$fileUrl %||% NA_character_
  ev$attachments$title <- x$attachments$title %||% NA_character_
  ev$attachments$mimeType <- x$attachments$mimeType %||% NA_character_
  ev$attachments$iconLink <- x$attachments$iconLink %||% NA_character_
  ev$attachments$fileId <- x$attachments$fileId %||% NA_character_

  ev

}

#' @export
print.event <- function(x, ...) {

  start <-
    x$start$dateTime %>%
    ifelse(!is.na(.), ., x$start$date)

  end <-
    x$end$dateTime %>%
    ifelse(!is.na(.), ., x$end$date)

  paste(
    "   Event ID: %s",
    "",
    "      Title: %s",
    "Description: %s",
    "   Location: %s",
    "      Start: %s",
    "        End: %s",
    "     Status: %s",
    "       ETag: %s",
    sep = "\n") %>%
    sprintf(x$id, x$summary, x$description, x$location,
            start, end, x$status, x$etag) %>%
    cat()

}
