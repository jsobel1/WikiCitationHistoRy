% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WikiHistoRyfunctions.r
\name{extract_citations}
\alias{extract_citations}
\title{Extract citations}
\usage{
extract_citations(art_text)
}
\arguments{
\item{art_text}{text content of wikipedia article}
}
\value{
citations
}
\description{
This function get a wikipedia article content
and return extracted citations using "\\{\\{[c|C]ite.*?\\}\\}" as regular expression
}
\examples{
art_test=get_article_most_recent_table("Zeitgeber")
citation_type_extracted=as.character(sapply(extract_citations(art_test[9]),replace_wikihypelinks))


}
