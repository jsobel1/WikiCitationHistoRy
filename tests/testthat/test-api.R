test_that("get_article_info_table parses mocked response", {
  json <- '{"batchcomplete":"","query":{"pages":{"123":{"pageid":123,"title":"Foo","length":456}}}}'
  result <- with_mock_api(json, get_article_info_table("Foo"))
  expect_equal(as.numeric(result[["pageid"]]), 123)
  expect_equal(result[["title"]], "Foo")
  expect_equal(as.numeric(result[["length"]]), 456)
})
