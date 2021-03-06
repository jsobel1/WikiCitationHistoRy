% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WikiHistoRyfunctions.r
\name{write_wiki_history_to_xlsx}
\alias{write_wiki_history_to_xlsx}
\title{Write Article Table to an xlsx}
\usage{
write_wiki_history_to_xlsx(wiki_hist, file_name)
}
\arguments{
\item{wiki_hist}{wiki history table}

\item{file_name}{output file name prefix}
}
\value{
A table with the last revision of the wikipedia article
}
\description{
This function get a wikipedia article table as input and the name of the target xls file
}
\examples{

tmpwikitable=get_article_initial_table("Zeitgeber")
write_wiki_history_to_xlsx(tmpwikitable,"Zeitgeber")
}
