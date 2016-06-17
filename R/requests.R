#' @keywords internal
update_resource <- function(path, dots) {

  url <-
    file.path(.cred$base_url_v3, path) %>%
    httr::modify_url(query = list(
      fields = "id",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::PATCH(url, gc_token(), encode = "json",
                body = dots) %>%
    httr::stop_for_status()

  return(invisible(NULL))

}
