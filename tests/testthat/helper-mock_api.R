library(httr)
library(stringr)
with_mock_api <- function(response_body, expr) {
  code <- substitute(expr)
  testthat::with_mocked_bindings(
    eval(code, parent.frame()),
    GET = function(url, ...) list(url = url),
    content = function(response, type = NULL, encoding = "UTF-8", ...) response_body,
    .package = "httr"
  )
}
