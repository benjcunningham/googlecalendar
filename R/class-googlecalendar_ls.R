as.googlecalendar_ls <- function(x) UseMethod("as.googlecalendar_ls", x)

as.googlecalendar_ls.data.frame <- function(x) {

  structure(dplyr::tbl_df(data.frame(
    etag = x$etag,
    id = x$id,
    summary = x$summary,
    description = x$description %||% NA_character_,
    location = x$location %||% NA_character_,
    timeZone = x$timeZone %||% NA_character_,
    summaryOverride = x$summaryOverride %||% NA_character_,
    colorId = x$colorId %||% NA_character_,
    backgroundColor = x$backgroundColor %||% NA_character_,
    foregroundColor = x$foregroundColor %||% NA_character_,
    hidden = x$hidden %||% FALSE %NA% FALSE,
    selected = x$selected %||% FALSE %NA% FALSE,
    accessRole = x$accessRole,
    primary = x$primary %||% FALSE %NA% FALSE,
    deleted = x$deleted %||% FALSE %NA% FALSE,
    stringsAsFactors = FALSE
  )),
  class = c("googlecalendar_ls", "tbl_df", "tbl", "data.frame"))

}

#' @export
print.googlecalendar_ls <- function(x, ...) {
  x %>%
    dplyr::mutate_each(dplyr::funs_(~ truncate_col(.))) %>%
    print()
}
