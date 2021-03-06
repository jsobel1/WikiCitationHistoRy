% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WikiHistoRyfunctions.r
\name{get_refCount}
\alias{get_refCount}
\title{Get refCount from wikipedia article content}
\usage{
get_refCount(art_text)
}
\arguments{
\item{art_text}{text content of wikipedia article}
}
\value{
refCount
}
\description{
This function get a wikipedia article content
and return refCount
}
\examples{
art_test=get_article_most_recent_table("Zeitgeber")
get_refCount(art_test[9])

}
