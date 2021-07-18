# WikiCitationHistoRy
WikiCitationHistoRy was developed in order to retrieve the history of any wikipedia article and its content as timestamps, revision ID , users, article size, and citations counts,and article text. This package allows retrieval of all information in structured tables and provides several visualisations for the data.

# Package installation

first, install the devtools package:

 	install.packages("devtools")

Then, install the WikipediaHistoRy package from Github:

	library(devtools)
	install_github("jsobel1/WikiCitationHistoRy")

# First steps and use cases with WikiCitationHistoRy



First you need to load few packages that are required:

		library("WikiCitationHistoRy")
		library("httr")
		library("openxlsx")

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

To export the history of a page in an xlsx format:

		write_wiki_history_to_xlsx(Zeitgeber_history,"Zeitgeber")

here the argument "Zeitgeber" is the name of the exported xlsx.

To get multiple articles history at once you can use:

		Category_articles_history=get_category_articles_history(c("Zeitgeber","Advanced sleep phase disorder"))

Note that it can take a long time if there are many articles or if the articles have many revisions.

## Count, extract and parse citations

Next several functions will be useful to count references, hyperlink, url and more








