#' @keywords internal
update_resource <- function(path, dots) {

  url <- build_url(path, fields = "id")

  resp <-
    httr::PATCH(url, gc_token(), encode = "json",
                body = dots) %>%
    httr::stop_for_status()

  return(invisible(NULL))

}
