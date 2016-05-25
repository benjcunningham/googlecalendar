.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googlecalendar <- list(
    googlecalendar.client_key = getOption("googlecalendar.client_key"),
    googlecalendar.client_secret = getOption("googlecalendar.client_secret"),
    googlecalendar.oauth_cache = getOption("googlecalendar.oauth_cache")
  )

  unset <- !(names(op.googlecalendar) %in% names(op))
  if (any(unset)) {
    options(op.googlecalendar[unset])
  }

  invisible(NULL)

}

#' Environment for storing credentials
#'
#' @noRd
.cred <- new.env(parent = emptyenv())
.cred$base_url <- "https://www.googleapis.com/calendar"
.cred$base_url_v3 <- file.path(.cred$base_url, "v3")
