googlecalendar <- function(...) {

  dots <- list(...)

  structure(list(
    kind = "calendar#calendarListEntry",
    etag = dots[["etag"]] %||% character(),
    id = dots[["id"]] %||% character(),
    summary = dots[["summary"]] %||% character(),
    description = dots[["description"]] %||% character(),
    location = dots[["location"]] %||% character(),
    timeZone = dots[["timeZone"]] %||% character(),
    summaryOverride = dots[["summaryOverride"]] %||% character(),
    colorId = dots[["colorId"]] %||% character(),
    backgroundColor = dots[["backgroundColor"]] %||% character(),
    foregroundColor = dots[["foregroundColor"]] %||% character(),
    hidden = dots[["hidden"]] %||% logical(),
    selected = dots[["selected"]] %||% logical(),
    accessRole = dots[["accessRole"]] %||% character(),
    defaultReminders = list(
      method = dots[["defaultReminders"]][["method"]] %||% character(),
      minutes = dots[["defaultReminders"]][["minutes"]] %||% integer()
    ),
    notificationSettings = list(
      notifications = list(
        type = dots[["notificationSettings"]][["notifications"]][["type"]] %||% character(),
        method = dots[["notificationSettings"]][["notifications"]][["method"]] %||% character()
      )
    ),
    primary = dots[["primary"]] %||% logical(),
    deleted = dots[["deleted"]] %||% logical()
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
    sprintf(
      x$id %0L% NA,
      x$summary %0L% NA,
      x$description %0L% NA,
      x$location %0L% NA,
      x$timeZone %0L% NA,
      x$accessRole %0L% NA,
      x$etag %0L% NA
    ) %>%
    cat()

}
