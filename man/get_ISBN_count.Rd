% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WikiHistoRyfunctions.r
\name{get_ISBN_count}
\alias{get_ISBN_count}
\title{Get ISBN Count from wikipedia article content}
\usage{
get_ISBN_count(art_text)
}
\arguments{
\item{art_text}{text content of wikipedia article}
}
\value{
ISBN_count
}
\description{
This function get a wikipedia article content
and return ISBN count
}
\examples{
art_test=get_article_most_recent_table("Zeitgeber")
get_ISBN_count(art_test[9])

}
