#' Authorize requests to the Google Calendar API
#'
#' Authorizes this package to interact with your Google Calendars. You
#' will be automatically directed to a web browser, asked to sign in to
#' your Google account, and prompted to grant permission for the package
#' to interact with the Google Calendar service.
#'
#' Typically, users will not need to call this function explicitly, as
#' the authorization process will be triggered by the first action that
#' requires a user login.
#'
#' While the client credentials \code{key} and \code{secret} are handled
#' by default when \code{googlecalendar} is loaded, the arguments must
#' be either interactively-specified or predefined in an
#' \code{.Rprofile} file using:
#'
#' \preformatted{
#' options(
#'   googlecalendar.client_key = "<KEY>",
#'   googlecalendar.client_secret = "<SECRET>"
#' )
#' }
#'
#' For more information on obtaining your own client credentials
#' required for authentication, see the official
#' \href{https://github.com/benjcunningham/googlecalendar}{README} on
#' GitHub.
#'
#' @param new_user Logical indicating whether to reauthorize using a new
#'   account and remove the current cached credentials.
#' @param key,secret The client key and secret for the application. See
#'   below for more information on overriding the default arguments.
#' @param cache Logical indicating whether to cache credentials in a
#'   \code{.httr-oauth} file in the working directory.
#' @template verbose
#'
#' @return An OAuth token object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load or refresh credentials.
#' # Note that a web browser authentication process may be triggered.
#' gs_auth()
#' }
gc_auth <- function(new_user = FALSE,
                    key = getOption("googlecalendar.client_key"),
                    secret = getOption("googlecalendar.client_secret"),
                    cache = getOption("googlecalendar.oauth_cache"),
                    verbose = FALSE) {

  if (new_user) {
    gc_deauth(clear_cache = TRUE, verbose = verbose)
  }

  scope <- "https://www.googleapis.com/auth/calendar"
  gc_app <- httr::oauth_app("google", key = key, secret = secret)

  gc_token <-
    httr::oauth_endpoints("google") %>%
    httr::oauth2.0_token(gc_app, scope, cache = cache)

  .cred$token <- gc_token

  invisible(.cred$token)

}

#' Deauthorize requests to Google Calendar API
#'
#' Deauthorizes this package to interact with your Google Calendars.
#'
#' @param clear_cache Logical indicating whether to remove the
#'   \code{.httr-oauth} file in the working directory.
#' @template verbose
#'
#' @return Logical indicating whether a token was removed from the
#'   internal credentialing environment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gc_deauth()
#' }
gc_deauth <- function(clear_cache = TRUE, verbose = TRUE) {

  # Take care of removing the actual .httr-oauth file. Not entirely sure
  # if deletion is ideal, but I think the functionality is as would be
  # expected.
  if (clear_cache) {
    if (file.exists(".httr-oauth")) {
      file.remove(".httr-oauth")
      if (verbose) {
        message("Removed OAuth cache.")
      }
    } else {
      if (verbose) {
        message("No OAuth cache to remove.")
      }
    }
  }

  if (token_available()) {
    rm("token", envir = .cred)
    if (verbose) {
      message("Removed the Google Calendar API token.")
    }
  } else {
    if (verbose) {
      message("No Google Calendar API token to remove.")
    }
    return(invisible(FALSE))
  }

  invisible(TRUE)

}

#' Generate a Google token
#'
#' Produces a token for interaction with the Google Calendar API by
#' either reauthorizing the application or loading a cached token.
#'
#' @return A \code{httr} request object.
#'
#' @keywords internal
gc_token <- function() {

  if(!token_available()) {
    gc_auth()
  }

  httr::config(token = .cred$token)

}

#' Check if a Google token exists
#'
#' Determines whether a token exists in the internal credential
#' environment.
#'
#' @return Logical indicating whether a token exists in the internal
#' credential environment.
#'
#' @keywords internal
token_available <- function() {

  if (is.null(.cred$token)) {
    return(FALSE)
  }

  TRUE

}
