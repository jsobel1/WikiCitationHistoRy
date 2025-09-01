# extractions and counts of various objects

pkg.env <- new.env()

pkg.env$doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+" #Good enough

pkg.env$isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)[-0-9X ]{13,20}'#'(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]' # to test

pkg.env$url_regexp = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

pkg.env$tweet_regexp='\\{\\{cite tweet.*?\\}\\}'

pkg.env$news_regexp='\\{\\{cite news.*?\\}\\}'

pkg.env$journal_regexp='\\{\\{cite journal.*?\\}\\}'

pkg.env$web_regexp='\\{\\{cite web.*?\\}\\}'

pkg.env$article_regexp='\\{\\{cite article.*?\\}\\}'

pkg.env$report_regexp='\\{\\{cite report.*?\\}\\}'

pkg.env$court_regexp='\\{\\{cite court.*?\\}\\}'

pkg.env$press_release_regexp='\\{\\{cite press release.*?\\}\\}'

pkg.env$book_regexp='\\{\\{cite book .*?\\}\\}'

pkg.env$pmid_regexp="(?<=(pmid|PMID)\\s?[=:]\\s?)\\d{5,9}"

pkg.env$ref_in_text_regexp='<ref>\\{\\{.*?\\}\\}</ref>' # in-text refs!

pkg.env$ref_regexp='<ref.*?</ref>' # All refs of a page

pkg.env$cite_regexp='\\{\\{[c|C]ite.*?\\}\\}' # All citations using the template

pkg.env$wikihyperlink_regexp='\\[\\[.*?\\]\\]'

pkg.env$template_regexp='\\{\\{pp.*?\\}\\}'

pkg.env$regexp_list=c(
  doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+",

  isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)[-0-9X ]{13,17}',#'(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]',

  url_regexp = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",

  wikihyperlink_regexp='\\[\\[.*?\\]\\]',

  tweet_regexp='\\{\\{cite tweet.*?\\}\\}',

  news_regexp='\\{\\{cite news.*?\\}\\}',

  journal_regexp='\\{\\{cite journal.*?\\}\\}',

  web_regexp='\\{\\{cite web.*?\\}\\}',

  article_regexp='\\{\\{cite article.*?\\}\\}',

  report_regexp='\\{\\{cite report.*?\\}\\}',

  court_regexp='\\{\\{cite court.*?\\}\\}',

  press_release_regexp='\\{\\{cite press release.*?\\}\\}',

  book_regexp='\\{\\{cite book .*?\\}\\}',

  pmid_regexp="(?<=(pmid|PMID)\\s?[=:]\\s?)\\d{5,9}",

  ref_in_text_regexp='<ref>\\{\\{.*?\\}\\}</ref>', # in-text refs!

  ref_regexp='<ref.*?</ref>', # All refs of a page

  cite_regexp='\\{\\{[c|C]ite.*?\\}\\}', # All citations using the template

  template_regexp='\\{\\{pp.*?\\}\\}'
)

#' Get Regex citations in wiki table
#'
#' This function get a regex of citations type and a wikipedia article table as input
#' and th creation dates e table with each matched ciation in each revision/article from a wiki table
#'
#' accessible regular expression
#'
#' doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"
#'
#' isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)[-0-9X ]{13,17}'# '(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]'
#'
#' url_regexp = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
#'
#' tweet_regexp='\\{\\{cite tweet.*?\\}\\}'
#'
#' news_regexp='\\{\\{cite news.*?\\}\\}'
#'
#' journal_regexp='\\{\\{cite journal.*?\\}\\}'
#'
#' web_regexp='\\{\\{cite web.*?\\}\\}'
#'
#' article_regexp='\\{\\{cite article.*?\\}\\}'
#'
#' report_regexp='\\{\\{cite report.*?\\}\\}'
#'
#' court_regexp='\\{\\{cite court.*?\\}\\}'
#'
#' press_release_regexp='\\{\\{cite press release.*?\\}\\}'
#'
#' book_regexp='\\{\\{cite book .*?\\}\\}'
#'
#' pmid_regexp="(?<=(pmid|PMID)\\s?[=:]\\s?)\\d{5,9}"
#'
#' ref_in_text_regexp='<ref>\\{\\{.*?\\}\\}</ref>' # in-text refs!
#'
#' ref_regexp='<ref.*?</ref>'
#'
#' cite_regexp='\\{\\{[c|C]ite.*?\\}\\}'
#'
#' @param article_wiki_table A data frame of Wikipedia revisions.
#' @param citation_regexp A regular expression used to locate citations.
#'
#'
#' @return A data frame mapping each revision to the citations matching
#'   `citation_regexp`.
#' @export
#'
#' @examples
#' Zeitgeber_history=get_article_full_history_table("Zeitgeber")
#' citations_in_wiki_table=get_regex_citations_in_wiki_table(Zeitgeber_history,doi_regexp)
#'

get_regex_citations_in_wiki_table=function(article_wiki_table,citation_regexp){
  citation_fetched=str_match_all(article_wiki_table$`*`, citation_regexp)

df_citation=data.frame(revid=rep(article_wiki_table$revid,unlist(lapply(citation_fetched,length))),citation_fetched=unlist(citation_fetched))

df_citation_revid_art=dplyr::select(article_wiki_table,art,revid)%>%dplyr::right_join(df_citation,by="revid")

return(df_citation_revid_art)
}

#' Parse Citation Type
#'
#' This function get a citation as input
#' and return the type of a citation such as book, website,newspaper, journal
#'
#' @param citation extracted citaion from wiki article content
#' @return citation type
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' citation_type_extracted=as.character(sapply(extract_citations(art_test[9]),replace_wikihypelinks))
#'
#'

parse_cite_type=function(citation){
  get_cite=gsub("\\{\\{[c|C]ite","",as.character(citation))
  get_cite=gsub("\\{\\{[c|C]ite","",get_cite)
  get_cite_type=unlist(strsplit(get_cite,"\\|"))[1]
  get_cite_type=gsub("\\s", "", get_cite_type)
  get_cite_type=tolower(get_cite_type)
  return(get_cite_type)
}

#' Extract citations
#'
#' This function get a wikipedia article content
#' and return extracted citations using "\\{\\{[c|C]ite.*?\\}\\}" as regular expression
#'
#' @param art_text text content of wikipedia article
#' @return citations
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' citation_type_extracted=as.character(sapply(extract_citations(art_test[9]),replace_wikihypelinks))
#'
#'

extract_citations=function(art_text){
  cite_regexp='\\{\\{[c|C]ite.*?\\}\\}'

  #cite
  cite_fetched=str_match_all(art_text, cite_regexp)

  cite=as.character(unlist(cite_fetched))

  return(cite)#df_cite_revid_art=dplyr::select(article_most_recent_table,art,revid)%>%dplyr::right_join(df_cite,by="revid")

}

#' Extract wikihypelinks
#'
#' This function get a wikipedia article content
#' and return extracted wikihypelinks using "\\[\\[.*?\\]\\]" as regular expression
#'
#' @param art_text text content of wikipedia article
#' @return wikihypelinks
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' citation_type_extracted=as.character(sapply(extract_citations(art_test[9]),replace_wikihypelinks))
#'
#'

extract_wikihypelinks=function(art_text){
  wiki_regexp='\\[\\[.*?\\]\\]'

  #cite
  wikihypelinks_fetched=str_match_all(art_text, wiki_regexp)

  wikihypelinks=as.character(unlist(wikihypelinks_fetched))

  return(wikihypelinks)#df_cite_revid_art=dplyr::select(article_most_recent_table,art,revid)%>%dplyr::right_join(df_cite,by="revid")

}

#' Replace wikihypelinks
#'
#' This function get a wikipedia article content
#' and replace wikihypelinks to clean wikipedia article content
#'
#' @param art_text text content of wikipedia article
#' @return art_text
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' citation_type_extracted=as.character(sapply(extract_citations(art_test[9]),replace_wikihypelinks))
#'
#'

replace_wikihypelinks=function(art_text){
  whl=extract_wikihypelinks(art_text)
  whl_cleaned=gsub("\\[\\[","",whl)
  whl_cleaned=gsub("\\]\\]","",whl_cleaned)

  whl_cleaned=sapply(whl_cleaned,function(x) as.character(unlist(strsplit(x,"\\|")))[1])

  art_text=mgsub(art_text,whl,whl_cleaned)
  return(art_text)

}


#' Parse wikipedia article content for ALL citations
#'
#' This function get a wikipedia article content
#' and return every citations including, news, web ,journal, etc.
#'
#' @param art_text text content of wikipedia article
#' @return citations
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' citation_extracted=parse_article_ALL_citations(art_test[9]))
#'

parse_article_ALL_citations=function(art_text){
#art_text=art_test
  get_cite=as.character(sapply(extract_citations(art_text),replace_wikihypelinks))
  cite_types=sapply(get_cite,parse_cite_type)
  get_cite=gsub("\\{\\{[c|C]ite","",as.character(get_cite))
  get_cite=gsub("\\{\\{[c|C]ite","",get_cite)
  get_cite=gsub("\\{\\{","",get_cite)
  get_cite=gsub("\\}\\}","",get_cite)

  get_cite_subfield=sapply(get_cite, function(x) unlist(strsplit(x,"\\|"))[2:length(unlist(strsplit(x,"\\|")))])

  df_out=data.frame(type=rep(as.character(unlist(cite_types)),lapply(get_cite_subfield,length)),id_cite=rep(1:length(get_cite),lapply(get_cite_subfield,length)),
                    reshape2::colsplit(string=unlist(get_cite_subfield), pattern="=", names=c("variable", "value")))

  df_out$variable=gsub(" ","",df_out$variable)

  #dcast(df_out,id_cite~Part1,value.var="Part2")
  return(df_out)
}

#' Get refCount from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return refCount
#'
#' @param art_text text content of wikipedia article
#' @return refCount
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_refCount(art_test[9])
#'

get_refCount=function(art_text){
  ref_regexp='<ref.*?</ref>'
  ref_fetched=str_match_all(art_text, ref_regexp)
  ref_count=length(as.character(unlist(ref_fetched)))
  return(as.numeric(as.character(ref_count)))
}

#' Get urlCount from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return urlCount
#'
#' @param art_text text content of wikipedia article
#' @return urlCount
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_urlCount(art_test[9])
#'

get_urlCount=function(art_text){
  ref_regexp=url_regexp
  ref_fetched=str_match_all(art_text, ref_regexp)
  ref_count=length(as.character(unlist(ref_fetched)))
  return(as.numeric(as.character(ref_count)))
}

#' Get hyperlinkCount from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return hyperlinkCount
#'
#' @param art_text text content of wikipedia article
#' @return hyperlinkCount
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_hyperlinkCount(art_test[9])
#'

get_hyperlinkCount=function(art_text){
  ref_regexp='\\[\\[.*?\\]\\]'
  ref_fetched=str_match_all(art_text, ref_regexp)
  ref_count=length(as.character(unlist(ref_fetched)))
  return(as.numeric(as.character(ref_count)))
}


#' Get DOI Count from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return doi count
#'
#' @param art_text text content of wikipedia article
#' @return doi_count
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_doi_count(art_test[9])
#'

get_doi_count=function(art_text){
  doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"
  doi_fetched=str_match_all(art_text, doi_regexp)
  doi_count=length(as.character(unlist(doi_fetched)))
  return(as.numeric(as.character(doi_count)))
}

#' Get ISBN Count from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return ISBN count
#'
#' @param art_text text content of wikipedia article
#' @return ISBN_count
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_ISBN_count(art_test[9])
#'

get_ISBN_count=function(art_text){
  ISBN_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)[-0-9X ]{13,17}'#'(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]'
  ISBN_fetched=str_match_all(art_text, ISBN_regexp)
  ISBN_count=length(as.character(unlist(ISBN_fetched)))
  return(as.numeric(as.character(ISBN_count)))
}

#' Get any regex Count from wikipedia article content
#'
#' This function get a wikipedia article content and a regular expression as arguments
#' and return regex count in the text.
#'
#' @param art_text text content of wikipedia article
#' @param regexp regular expression
#' @return ref_count
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' get_anyCount(art_test[9],'<ref.*?</ref>')
#'

 get_anyCount=function(art_text,regexp){
   ref_regexp=regexp
   ref_fetched=str_match_all(art_text, ref_regexp)
   ref_count=length(as.character(unlist(ref_fetched)))
   return(as.numeric(as.character(ref_count)))
 }



#' Get SciScore from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return SciScore from citation template journal citation over all citations
#'
#' @param art_text text content of wikipedia article
#' @return SciScore
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' Get_sci_score(art_test[9])
#'


Get_sci_score=function(art_text){
  extracted_cite=tryCatch(extract_citations(art_text),error = function(e) 0)
  cite_type=sapply(extracted_cite,parse_cite_type)
  all_cite_sum= tryCatch(sum(table(cite_type)),error = function(e) 0)
  journal_cite= tryCatch(table(cite_type)[which(names(table(cite_type))=="journal")],error = function(e) NA)
  if(length(journal_cite)==0){return(0)}
  return(as.numeric(as.character(journal_cite/all_cite_sum)))
}



#' Get SciScore2 from wikipedia article content
#'
#' This function get a wikipedia article content
#' and return SciScore2 doi over refs
#'
#' @param art_text text content of wikipedia article
#' @return SciScore2
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' Get_sci_score2(art_test[9])
#'


Get_sci_score2=function(art_text){

  ref_regexp='<ref.*?</ref>' # in-text refs!
  doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"

  ref_fetched=str_match_all(art_text, ref_regexp)
  ref_count=length(as.character(unlist(ref_fetched)))

  doi_fetched=str_match_all(art_text, doi_regexp)
  doi_count=length(as.character(unlist(doi_fetched)))

  return(as.numeric(as.character(doi_count/ref_count)))
}

#' Get Source Type count
#'
#' This function get a wikipedia article content as input
#' and return the the count of a citation from the CS1 template such as book, website, newspaper, journal
#'
#' @param art_text wiki article content
#' @return citation_type_count
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' Get_source_type_counts(art_test[9])
#'
#'


Get_source_type_counts=function(art_text){
  extracted_cite=tryCatch(extract_citations(art_text),error = function(e) 0)
  cite_type=sapply(extracted_cite,parse_cite_type)

  cite_source_count= tryCatch(table(cite_type),error = function(e) NA)
  if(length(cite_source_count)==0){return(NA)}
  return(as.data.frame(cite_source_count))

}


#' Get parsed citations
#'
#' This function get a wikipedia page table as input
#' and return the the parsed citations from the CS1 template with every field.
#' the output is  6 column dataframe containing the page name, revisionID, type of citation,
#' an integer value for each citation extracted, the citation variable name (i.e publisher, date, authors)
#' and the variable value.
#'
#' @param article_most_recent_table wikipedia pages table
#' @return df_cite_parsed_revid_art
#' @export
#'
#' @examples
#' category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#' paresd_citations=get_paresd_citations(category_most_recent)
#'
#'

get_paresd_citations=function(article_most_recent_table){

  df_cite_clean=c()

  for(i in 1:length(article_most_recent_table$revid)){

    print(article_most_recent_table$art[i])

    dfctmp=try(parse_article_ALL_citations(article_most_recent_table$`*`[i]))
    try({
      if(dim(dfctmp)[1]>=1){
        dfctmp$revid=rep(article_most_recent_table$revid[i],dim(dfctmp)[1])
        df_cite_clean=rbind(df_cite_clean,dfctmp)
      }
    })
  }

  df_cite_parsed_revid_art=dplyr::select(article_most_recent_table,art,revid)%>%dplyr::right_join(df_cite_clean,by="revid")

  #write.table(df_cite_parsed_revid_art,"df_cite_parsed_revid_art.csv",quote = F,row.names = F,sep=";")

  return(df_cite_parsed_revid_art)
}


# Exports of history and category tables

#' Write Article Table to an xlsx
#'
#' This function get a wikipedia article table as input and the name of the target xls file.
#' an xlsx with the table is written in the working directory.
#'
#' @param wiki_hist wiki history table
#' @param file_name output file name prefix
#' @return nothing
#' @export
#'
#' @examples
#'
#' tmpwikitable=get_article_initial_table("Zeitgeber")
#' write_wiki_history_to_xlsx(tmpwikitable,"Zeitgeber")

#' Get citation type counts
#'
#' This function summarises citation types for each article in a table of
#' most recent revisions.
#'
#' @param article_most_recent_table Wikipedia revisions table.
#' @return A data frame of citation type counts per article.
#' @export
#'
#' @examples
#' art <- get_article_most_recent_table("Zeitgeber")
#' get_citation_type(art)

get_citation_type=function(article_most_recent_table){

  df_cite_type_clean=c()

  for(i in 1:length(article_most_recent_table$revid)){

    print(article_most_recent_table$art[i])

    dfctmp=try(Get_source_type_counts(article_most_recent_table$`*`[i]))
    try({
      if(dim(dfctmp)[1]>1){
        dfctmp$revid=rep(article_most_recent_table$revid[i],dim(dfctmp)[1])
        df_cite_type_clean=rbind(df_cite_type_clean,dfctmp)
      }
    })
  }
  df_cite_count_revid_art=dplyr::select(article_most_recent_table,art,revid)%>%dplyr::right_join(df_cite_type_clean,by="revid")

  return(df_cite_count_revid_art)
}

#' Plot top citation sources
#'
#' Generates bar plots of the top 20 values for each citation source type
#' present in a parsed citation table.
#'
#' @param df_cite_parsed_revid_art Parsed citation data frame.
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' df <- get_paresd_citations(get_category_articles_most_recent("Zeitgeber"))
#' get_pdfs_top20source(df)

get_pdfs_top20source=function(df_cite_parsed_revid_art){
  #pdf("top20source.pdf")
  for(i in 1:length(source_types_list)){
    plot_top_source(df_cite_parsed_revid_art,as.character(source_types_list[i]))
  }
  #dev.off()
}

#' Get top cited Wikipedia papers
#'
#' Identifies the most frequently cited DOIs and annotates them with
#' bibliographic information.
#'
#' @param df_doi_revid_art Data frame linking DOIs to Wikipedia articles.
#' @return A data frame of the top cited DOIs with annotations.
#' @export
#'
#' @examples
#' df <- get_regex_citations_in_wiki_table(get_article_most_recent_table("Zeitgeber"),
#'                                         "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")
#' get_top_cited_wiki_papers(df)

get_top_cited_wiki_papers=function(df_doi_revid_art){

  top_20_wiki_cited_doi=names(tail(sort(table(unique(df_doi_revid_art)$citation_fetched)),40))
  wikicount=data.frame(tail(sort(table(unique(df_doi_revid_art)$citation_fetched)),40))
  colnames(wikicount)=c("citation","wiki_count")

  top_20_wiki_cited_doi_annotated=annotate_doi_list_europmc(top_20_wiki_cited_doi)

  top_20_wiki_cited_doi_annotated=top_20_wiki_cited_doi_annotated%>%dplyr::inner_join(wikicount,by=c("doi"="citation"))

  citation_countdf=cr_citation_count(doi = top_20_wiki_cited_doi_annotated$doi)

  top_20_wiki_cited_doi_annotated=top_20_wiki_cited_doi_annotated%>%dplyr::left_join(citation_countdf,by=c("doi"))

  top20_cited_in_wiki_art=df_doi_revid_art%>% dplyr::filter(citation_fetched %in% top_20_wiki_cited_doi_annotated$doi)%>%unique()%>%dplyr::select(citation_fetched,art)%>%
    group_by(citation_fetched)%>% summarise(cited_in_wiki_art = paste(art, collapse = ", "))

  #top_20_wiki_cited_doi_annotated= top_20_wiki_cited_doi_annotated%>%dplyr::left_join(top20_cited_in_wiki_art,by=c("citation_fetched"="citation"))

  #write.table(top_20_wiki_cited_doi_annotated,"top_20_wiki_cited_doi_annotated_europmc.csv",sep=";",row.names = F)

  return(top_20_wiki_cited_doi_annotated)
}

# get_top_cited_wiki_papers(df_doi_revid_art)


#' Retrieve article tables for multiple titles
#'
#' Fetches initial, most recent, info and full history tables for each
#' article in a vector of titles.
#' Plot top values for a citation variable
#'
#' Displays the top 20 occurrences of a given citation variable.
#'
#' @param df_cite_parsed_revid_art Parsed citation data frame.
#' @param source_type Citation variable to summarise.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' df <- get_paresd_citations(get_category_articles_most_recent("Zeitgeber"))
#' plot_top_source(df, "publisher")

  plot_top_source=function(df_cite_parsed_revid_art,source_type){

    #publisher
    P1=df_cite_parsed_revid_art%>%dplyr::filter(variable==source_type)%>%dplyr::mutate(value=gsub(" ","",value))%>%dplyr::filter(value!="")%>%
      dplyr::group_by(value)%>%
      summarise(count=n())%>%arrange(-count)%>%
      top_n(20)%>%ggplot(aes(reorder(value,count),count))+
      geom_bar(stat="identity")+coord_flip()+ggtitle(paste("Top 20",source_type))
    print(P1)
  }

#' Plot distribution of citation source types
#'
#' Creates a boxplot showing counts of selected citation types.
#'
#' @param df_cite_count_revid_art Data frame of citation type counts.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' df <- get_citation_type(get_article_most_recent_table("Zeitgeber"))
#' plot_distribution_source_type(df)

  plot_distribution_source_type=function(df_cite_count_revid_art){
    P1=df_cite_count_revid_art%>%dplyr::filter(cite_type %in% c("journal","news","web","book"))%>%dplyr::group_by(revid,cite_type,Freq)%>%
      ggplot(aes(cite_type,Freq))+ geom_boxplot(width=0.6)+coord_flip() #geom_violin(trim = F)+
    print(P1)
  }


#' Retrieve subcategories
#'
#' Returns a table of subcategories for a given Wikipedia category.
#'
#' @param catname Category name.
#' @param replecement Character replacement for spaces.
#' @return Data frame of subcategories.
#' Extract citations using multiple patterns
#'
#' Applies each regular expression stored in \code{pkg.env$regexp_list} to a
#' table of article revisions.
#'
#' @param article_most_recent_table Table of article revisions.
#' @return A named list of data frames of matched citations.
#' @export
#'
#' @examples
#' art <- get_article_most_recent_table("Zeitgeber")
#' extract_citations_regexp(art)

  extract_citations_regexp=function(article_most_recent_table){
    extracted_citation_list=list()

    for(i in 1:length(pkg.env$regexp_list)){
      tmp_table=get_regex_citations_in_wiki_table(article_most_recent_table,as.character(pkg.env$regexp_list[i]))
      #tmp_table=tmp_table%>%dplyr::filter(citation!="pmid",citation!="isbn")
      extracted_citation_list[[i]]=tmp_table
    }
    names(extracted_citation_list)=names(pkg.env$regexp_list)
    return(extracted_citation_list)
  }





