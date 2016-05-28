googlecalendar <- function() {

  structure(list(
    kind = "calendar#calendarListEntry",
    etag = character(),
    id = character(),
    summary = character(),
    description = character(),
    location = character(),
    timeZone = character(),
    summaryOverride = character(),
    colorId = character(),
    backgroundColor = character(),
    foregroundColor = character(),
    hidden = logical(),
    selected = logical(),
    accessRole = character(),
    defaultReminders = list(
      method = character(),
      minutes = integer()
    ),
    notificationSettings = list(
      notifications = list(
        type = character(),
        method = character()
      )
    ),
    primary = logical(),
    deleted = logical()
  ),
  class = c("googlecalendar", "list"))

}

as.googlecalendar <- function(x) UseMethod("as.googlecalendar", x)

as.googlecalendar.data.frame <- function(x) {

  cal <- googlecalendar()

  cal$etag <- x$etag
  cal$id <- x$id
  cal$summary <- x$summary
  cal$description <- x$description %||% NA_character_
  cal$location <- x$location %||% NA_character_
  cal$timeZone <- x$timeZone %||% NA_character_
  cal$summaryOverride <- x$summaryOverride %||% NA_character_
  cal$colorId <- x$colorId %||% NA_character_
  cal$backgroundColor <- x$backgroundColor %||% NA_character_
  cal$foregroundColor <- x$foregroundColor %||% NA_character_
  cal$hidden <- x$hidden %||% FALSE
  cal$selected <- x$selected %||% FALSE
  cal$accessRole <- x$accessRole
  cal$defaultReminders$method <-
    x$defaultReminders[[1]]$method %||% NA_character_
  cal$defaultReminders$minutes <-
    x$defaultReminders[[1]]$minutes %||% NA_integer_
  cal$notificationSettings$notifications$type <-
    x$notificationSettings.notifications[[1]]$type %||% NA_character_
  cal$notificationSettings$notifications$method <-
    x$notificationSettings.notifications[[1]]$method %||% NA_character_
  cal$primary <- x$primary %||% FALSE
  cal$deleted <- x$deleted %||% FALSE

  cal

}

#' @export
print.googlecalendar <- function(x, ...) {

  paste(
    "Calendar ID: %s",
    "",
    "      Title: %s",
    "Description: %s",
    "   Location: %s",
    "  Time Zone: %s",
    "Permissions: %s",
    "       ETag: %s",
    sep = "\n") %>%
    sprintf(x$id, x$summary, x$description, x$location, x$timeZone,
            x$accessRole, x$etag) %>%
    cat()

}
