test_that("parse_cite_type extracts type", {
  expect_equal(parse_cite_type("{{cite journal|title=Example}}"), "journal")
  expect_equal(parse_cite_type("{{Cite book|title=Book}}"), "book")
})

test_that("extract_citations finds citations", {
  text <- "Intro {{Cite journal|title=Foo}} middle {{cite web|url=http://example.com}} end"
  cites <- extract_citations(text)
  expect_equal(cites, c("{{Cite journal|title=Foo}}", "{{cite web|url=http://example.com}}"))
})

test_that("Get_sci_score2 counts DOI over refs", {
  text <- "<ref>{{Cite journal|doi=10.1234/abcd}}</ref><ref>{{Cite book|title=Book}}</ref>"
  expect_equal(Get_sci_score2(text), 0.5)
})

test_that("Get_source_type_counts counts citation types", {
  text <- "{{cite journal|title=Foo}} {{cite book|title=Bar}} {{cite journal|title=Baz}}"
  counts <- Get_source_type_counts(text)
  journal <- counts$Freq[counts$cite_type == "journal"]
  book <- counts$Freq[counts$cite_type == "book"]
  expect_equal(journal, 2)
  expect_equal(book, 1)
})
