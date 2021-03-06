#' Authorize requests to the Google Calendar API
#'
#' Authorizes this package to interact with your Google Calendars. You
#' will be automatically directed to a web browser, asked to sign in to
#' your Google account, and prompted to grant permission for the package
#' to interact with the Google Calendar service.
#'
#' Typically, users will not need to call this function explicitly, as
#' the authorization process will be triggered by the first action that
#' requires a user login. However, before using \code{googlecalendar},
#' the client credentials \code{key} and \code{secret} should be
#' predefined in an \code{.Rprofile} file using:
#'
#' \preformatted{
#' options(
#'   googlecalendar.client_key = "<KEY>",
#'   googlecalendar.client_secret = "<SECRET>",
#'   googlecalendar.oauth_cache = TRUE
#' )
#' }
#'
#' Note that the default value of \code{cache} may also be specified in
#' the \code{.Rprofile} file using \code{googlecalendar.oauth_cache}.
#' The recommended value is \code{TRUE}.
#'
#' For more information on obtaining these credentials, see the project
#' \href{https://github.com/benjcunningham/googlecalendar}{README} on
#' GitHub or visit the
#' \href{https://console.developers.google.com/}{Google API Console} to
#' get started registering an application.
#'
#' @param new_user Logical indicating whether to reauthorize using a new
#'   account and remove the current cached credentials. Currently, only
#'   one account can be authorized or cached in a directory at a time.
#' @param key,secret Client key and secret for the application. See
#'   below for more information on setting these in an \code{.Rprofile}
#'   file.
#' @param cache Logical indicating whether to cache credentials in a
#'   \code{.httr-oauth} file in the working directory.
#' @param token Token object or the name of a token object \code{.rds}
#'   file to be set in the credentialing environment. This is most
#'   useful for authentication in a non-interactive setting.
#' @template verbose
#'
#' @return An OAuth token object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gs_auth()
#' gs_auth(token = readRDS("saved_token.rds"))
#' gs_auth(token = "saved_token.rds")
#' }
gc_auth <- function(new_user = FALSE,
                    key = getOption("googlecalendar.client_key"),
                    secret = getOption("googlecalendar.client_secret"),
                    cache = getOption("googlecalendar.oauth_cache"),
                    token = NULL,
                    verbose = FALSE) {

  if (new_user) {
    gc_deauth(clear_cache = TRUE, verbose = verbose)
  }

  if (!is.null(token)) {

    if(methods::is(token, "character")) {
      token <- readRDS(token)
    }

    .cred$token <- token

  } else {

    scope <- "https://www.googleapis.com/auth/calendar"
    gc_app <- httr::oauth_app("google", key = key, secret = secret)

    gc_token <-
      httr::oauth_endpoints("google") %>%
      httr::oauth2.0_token(gc_app, scope, cache = cache)

    .cred$token <- gc_token

  }

  invisible(.cred$token)

}

#' Deauthorize requests to the Google Calendar API
#'
#' Deauthorizes this package from interacting with your Google
#' Calendars.
#'
#' @param clear_cache Logical indicating whether to remove any
#'   \code{.httr-oauth} file that exists in the working directory.
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
