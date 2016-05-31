#' Run this whenever Travis-CI needs a new token.
#' Hopefully that shouldn't be often, but as per this old StackOverflow
#' answer (http://stackoverflow.com/a/26136111), there is a 25 token
#' max stack for a single user's valid token pool.
#'
#' Also remember to re-encode the in Ruby with:
#'
#' travis login
#' travis encrypt-file tests/testthat/googlecalendar_token.rds

gc_deauth(clear_cache = TRUE)
new_travis_token <- gc_auth()

saveRDS(new_travis_token, file = "tests/testthat/googlecalendar_token.rds")
