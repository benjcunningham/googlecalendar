build_url <- function(path, fields = NULL, ..., base = .cred$base_url_v3) {

  file.path(base, path) %>%
    httr::modify_url(query = c(list(...), list(
      fields = fields,
      key = getOption("googlecalendar.client_key")
    )))

}

itemize_fields <- function(x) {
  paste0("items(", paste(x, collapse = ","), ")")
}

json_content <- function(x, flatten = FALSE) {
  httr::content(x, "text", "application/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = flatten)
}

truncate_col <- function(x, n = 20) {
  ifelse(stringr::str_length(x) > n,
         paste0(stringr::str_sub(x, end = n - 1), "\u2026"),
         stringr::str_sub(x, end = n))
}

# Does this already exist somewhere?
# Am I just forgetting the better way to do this?
`%NA%` <- function(x, y) {
  x[is.na(x)] <- y
  x
}

`%0L%` <- function(x, y) {
  if (length(x) == 0) {
    return(y)
  }
  x
}
