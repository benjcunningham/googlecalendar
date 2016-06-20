context("Authentication")

test_that("No token is registered", {
  expect_null(.cred$token)
  expect_false(token_available())
})

# Get a token in place to simulate what happens when we run gc_auth()
.cred$token <- readRDS("googlecalendar_token.rds")

test_that("The token is legitimate", {
  expect_is(.cred$token, "Token2.0")
})

test_that("The token has been deregistered", {
  expect_true(gc_deauth())
  expect_null(.cred$token)
  expect_false(token_available())
})
