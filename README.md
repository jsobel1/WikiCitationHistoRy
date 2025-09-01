# WikiCitationHistoRy R package

[![Codecov test coverage](https://codecov.io/gh/jsobel1/WikiCitationHistoRy/branch/master/graph/badge.svg)](https://codecov.io/gh/jsobel1/WikiCitationHistoRy?branch=master)

WikiCitationHistoRy was developed in order to retrieve the history and its content of any wikipedia article as timestamps, revision ID , users, article size,and article text. This package allows retrieval of all information in structured tables and provides several visualisations of the data. In addition, the package offers several options to extract citations and references, and annotate them.

<img src="https://github.com/jsobel1/WikiCitationHistoRy/blob/master/img/WikiHistory_logo_V3_test.png" width="250">


# Package installation

first, install the devtools package:

 	install.packages("devtools")

Then, install the **WikipediaHistoRy** package from Github:

	library(devtools)
	install_github("jsobel1/WikiCitationHistoRy")

# First steps and use cases with **WikiCitationHistoRy**

First you need to load few packages that are required:

	library("WikiCitationHistoRy")
	library("httr")
	library("openxlsx")

## Content of **WikiCitationHistoRy** package
- Retrieve most recent content or full history of one or more Wikipedia pages and pages informations.
- Extract citations, references, hyperlinks, DOIs, ISBN, PMID, and others.
- Annotate DOIs with Altmetrics, Europmc, CrossRef; Annotate ISBN with google book, altmetrics, openlib
- Exports in xlsx tables.
- Compute SciScore for a page and latency for each DOI annotated citation.
- Make various visualisations such as timeline of wikipedia article creation in a category, timeline of edits and views per page,
network of co-citations, and more  

## Get wikipedia pages content and information

To retrieve the text of a given page and the revision history:

	Zeitgeber_history=get_article_full_history_table("Zeitgeber")

Here all the revisions of the Zeitgeber page are retrived and stored in a table with the following structure

	colnames(Zeitgeber_history)

	"art"       "revid"     "parentid"  "user"      "userid"    "timestamp" "size"     "comment"   "*"      

art is the name of the page; revid is the revision ID number; parentid is the revision id of the previous version used for the current revision;  user is the name of the user responsible for the update; the userid is the id of the user; timestamp is the logged timestamp of the current revision; the size is the length in bits of the article; comments are annotations made by user on the given revision; * is the raw text of the page of the given revision.

To get the initial (first) version of an article in the same format, you can use:

	get_article_initial_table("Zeitgeber")

or for the most recent version:

	get_article_most_recent_table("Zeitgeber")

To get only informations on the current version of the article:

	get_article_info_table("Zeitgeber")

To export the history (or a wikipedia table) of one or more pages in an xlsx format:

	write_wiki_history_to_xlsx(Zeitgeber_history,"Zeitgeber")

here the argument "Zeitgeber" is the name of the exported xlsx.

To get multiple articles history at once you can use:

	Category_articles_history=get_category_articles_history(c("Zeitgeber","Advanced sleep phase disorder"))

*Note that it can take a long time if there are many articles or if the articles have many revisions.*

To get multiple articles most recent content in the same format at once you can use:

	Category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))

To get a list of pages related to a single wikipedia category you can use the following function based on *WikipediR* R package:

	library(WikipediR)
	get_pagename_in_cat("Circadian rhythm")

## Extraction and parsing of citations, urls, hyperlinks and more

Working with citations in wikipedia can be a bit tricky. However, there is a citation template called [**Citation Style 1**](https://en.wikipedia.org/wiki/Help:Citation_Style_1), that is quite used. Unfortunately, it is possible to cite a reference without the use of the template. Thus, in this package, we developped a parser working on the **Citation Style 1** (CS1) and multiple *regular expressions* allowing the extraction of DOI, ISBN, PMID, URL, hyperlinks or page protection information (locked page).  

For this purpose we developped a generic function which takes in arguments 
the table of revisions of one or more wikipedia page and a regular expression:

	get_regex_citations_in_wiki_table(article_wiki_table,citation_regexp) # generic syntaxe (not run)

Here is the list of regular expressions in the package

**Citation Style 1**:

	tweet_regexp='\\{\\{cite tweet.*?\\}\\}'
	news_regexp='\\{\\{cite news.*?\\}\\}'
	journal_regexp='\\{\\{cite journal.*?\\}\\}'
	web_regexp='\\{\\{cite web.*?\\}\\}'
	article_regexp='\\{\\{cite article.*?\\}\\}'
	report_regexp='\\{\\{cite report.*?\\}\\}'
	court_regexp='\\{\\{cite court.*?\\}\\}'
	press_release_regexp='\\{\\{cite press release.*?\\}\\}'
	book_regexp='\\{\\{cite book .*?\\}\\}'
	cite_regexp='\\{\\{[c|C]ite.*?\\}\\}' # All citations using the template

Others *regexp*:

	doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+" 
	isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]' 
	url_regexp = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
	pmid_regexp="(?<=(pmid|PMID)\\s?[=:]\\s?)\\d{5,9}"
	ref_in_text_regexp='<ref>\\{\\{.*?\\}\\}</ref>' # in-text refs
	ref_regexp='<ref.*?</ref>' # All refs of a page
	wikihyperlink_regexp='\\[\\[.*?\\]\\]' # hyperlink between wikipedia pages
	template_regexp='\\{\\{pp.*?\\}\\}' # protection informations

You can use this function with your own regular expression to extract anything on a wikipedia table (multiple pages, and/or revisions)

a working example will be as follow:
	
	category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))

 	extracted_citation_table=get_regex_citations_in_wiki_table(category_most_recent, "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+") # doi_regexp

Here *extracted_citation_table* is a dataframe with 3 columns including the name of the page, the revision ID and the extracted infromation thanks to the regular expression. In this example, we extracted every DOI from the most recent pages "Zeitgeber","Advanced sleep phase disorder", and "Sleep deprivation". 

To extract and export in multiple xlsx every predefined regular expression matches:

	export_extracted_citations_xlsx(category_most_recent,"Category_most_recent")

Here the second argument of the function is a file prefix name. An xlsx file with matches for each regular expression will be created for each pages or revisions in the provided wikipeia table (here *Category_most_recent*).


Next to parse citations information from the CS1 template:

	paresd_citations=get_paresd_citations(category_most_recent)

This function gets a wikipedia page table as input and return the the parsed citations from the CS1 template with every field.
the output is  6 column dataframe containing page name, revisionID, type of citation,
an integer value for each citation extracted in a given page, citation variable names (i.e publisher, date, authors)
and values.

## DOI annotations

*Note that quality of retrieved informations and number of successfully annotated elements may varies depending on the source. Thus we implemented several solutaions and we will add more sources in the future*

As shown above we can extract DOI with the following code:

	category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
	extracted_citation_table=get_regex_citations_in_wiki_table(category_most_recent, "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+") # doi_regexp

next to annotate DOI with EuropePMC R package you can simply use:

	library(europepmc)
	DOI_annotated_epmc=annotate_doi_list_europmc(as.character(extracted_citation_table$citation_fetched))

*DOI_annotated_epmc* is a table of DOI with many informations such as authors, journal, publication date, Open-Access information, and citation count in the scientific literature.

We can get similar informations from the R CrossRef package with the following code:

	library(rcrossref)
	DOI_annotated_rcrossref=annotate_doi_list_cross_ref(as.character(extracted_citation_table$citation_fetched))

*Note that Rcrossref can be quite slow for large list.*

We can get similar informations and impact of the publication from the R Altmetric package with the following code:

	library(rAltmetric)
	library(purrr)
	DOI_annotated_altmetrics=annotate_doi_list_altmetrics(list(as.character(extracted_citation_table$citation_fetched)))

*Note that dates from Altmetrics are in unix timestamp. for more information on each field look at the rAltmetric website*


### To Do: 
- ISBN annotations
- SciScore & latency examples
- Exports 
- Visualisation examples

*Note that this package is still under development. Please contact me (jsobel83@gmail.com) if you want to contribute.*










