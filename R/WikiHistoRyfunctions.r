#' Get Article History table
#'
#' This function get an english wikipedia article title as input
#' and retrieve a table containing ids, timestamp, comment,
#' user, userid,size,content for every revisions of the given wikipedia article
#'
#'
#' @param article_name Path to the input file
#' @param date_an input date to select most recent version
#' @return A table with all revisions of the wikipedia article
#' @export
#'
#' @examples
#' Zeitgeber_history=get_article_full_history_table("Zeitgeber")


get_article_full_history_table=function(article_name,date_an="2020-05-01T00:00:00Z"){
  what="ids|timestamp|comment|user|userid|size|content" #|parsedcomment|tags|flags

  article_name_c=gsub(" ","%20",article_name)
  output_table=c()
  cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&rvend=",date_an,"&format=json&rvlimit=max",sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
  name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
  tmp_tab=eval(parse(text=tt))
  output_table=cbind(art=name_tab,tmp_tab[,c("revid","parentid","user","userid","timestamp","size","comment","*")])


  while(length(parsed$continue$rvcontinue)==1){
    output_table_load=c()
    print(parsed$continue$rvcontinue)
    rvc=parsed$continue$rvcontinue
    cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&rvend=",date_an,"&format=json&rvlimit=max&rvcontinue=",rvc,sep="")
    resp=GET(cmd)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
    tt2=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
    name_tab2=rep(article_name,dim(eval(parse(text=tt2)))[1])

    tmp_tab2=eval(parse(text=tt2))


    if(length(name_tab2)<1) break
    output_table_load=cbind(art=name_tab2,tmp_tab2[,c("revid","parentid","user","userid","timestamp","size","comment","*")])


    output_table=try(rbind(output_table,output_table_load),silent=T)
  }
  return(output_table)
}

#' Get Article Initial Table
#'
#' This function get an english wikipedia article title as input
#' and retrieve a table containing ids, timestamp, comment,
#' user, userid,size,content of the first revision of the given wikipedia article
#'
#'
#' @param article_name Path to the input file
#' @return A table with the initial revision of the wikipedia article
#' @export
#'
#' @examples
#' get_article_initial_table("Zeitgeber")

get_article_initial_table=function(article_name){
  what="ids|timestamp|comment|user|userid|size|content" #|parsedcomment|tags|flags

  article_name_c=gsub(" ","%20",article_name)
  output_table=c()
  cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&format=json&rvlimit=1",sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
  name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
  tmp_tab=eval(parse(text=tt))
  output_table=cbind(art=name_tab,tmp_tab[,c("revid","parentid","user","userid","timestamp","size","comment","*")])

  return(output_table)
}



#' Get Article Informations Table
#'
#' This function get an english wikipedia article title as input
#' and retrieve a table containing pageids, title and length
#' of the given wikipedia article
#'
#'
#' @param article_name Path to the input file
#' @param date_an input date to select most recent version
#' @return A table with the initial revision of the wikipedia article
#' @export
#'
#' @examples
#' get_article_info_table("Zeitgeber")

get_article_info_table=function(article_name,date_an="2020-05-01T00:00:00Z"){
  #article_name="Zeitgeber"
  what="pageid|title|length" #|parsedcomment|tags|flags
  #api.php?action=query&titles=Albert%20Einstein&prop=info&inprop=url|talkid
  article_name_c=gsub(" ","%20",article_name)
  output_table=c()
  cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=info&inprop=",what,"&rvstart=",date_an,"&rvdir=older&format=json",sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'",sep="")
  #name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
  tmp_tab=unlist(eval(parse(text=tt)))
  #output_table=cbind(art=name_tab,tmp_tab[,c("revid","parentid","user","userid","timestamp","size","comment","*")])

  return(tmp_tab)
}


#' Get Article Most Recent Table
#'
#' This function get an english wikipedia article title as input
#' and retrieve a table containing ids, timestamp, comment,
#' user, userid,size,content of the last revision of the given wikipedia article
#'
#'
#' @param article_name Path to the input file
#' @param date_an input date to select most recent version
#' @return A table with the last revision of the wikipedia article
#' @export
#'
#' @examples
#' get_article_most_recent_table("Zeitgeber")


get_article_most_recent_table=function(article_name,date_an="2020-05-01T00:00:00Z"){
  what="ids|timestamp|comment|user|userid|size|content" #|parsedcomment|tags|flags

  article_name_c=gsub(" ","%20",article_name)
  output_table=c()
  cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=",date_an,"&rvdir=older&format=json&rvlimit=1",sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
  name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
  tmp_tab=eval(parse(text=tt))
  output_table=cbind(art=name_tab,tmp_tab[,c("revid","parentid","user","userid","timestamp","size","comment","*")])

  return(output_table)
}


#' Write Article Table to an xlsx
#'
#' This function get a wikipedia article table as input and the name of the target xls file
#'
#'
#' @param wiki_hist wiki history table
#' @param file_name output file name prefix
#' @return A table with the last revision of the wikipedia article
#' @export
#'
#' @examples
#'
#' tmpwikitable=get_article_initial_table("Zeitgeber")
#' write_wiki_history_to_xlsx(tmpwikitable,"Zeitgeber")


write_wiki_history_to_xlsx=function(wiki_hist,file_name){
  wiki_hist[is.na(wiki_hist)]="-"
  wiki_hist[is.null(wiki_hist)]="-"
  df <- data.frame(art=wiki_hist$art,revid=wiki_hist$revid,parentid=wiki_hist$parentid,user=wiki_hist$user,userid=wiki_hist$userid,
                   timestamp=wiki_hist$timestamp,size=wiki_hist$size,comment=wiki_hist$comment,content=wiki_hist$`*`,stringsAsFactors=FALSE)

  write.xlsx(df,paste(file_name,"wiki_table.xlsx",sep="_"), sheetName="Sheet1",  col.names=TRUE, row.names=F, append=FALSE, showNA=T)
}



#' Get Category Articles history
#'
#' This function get a list of wikipedia article titles as input
#' and create a wipipedia history table
#'
#' @param list_art list of wikipedia articles

#' @return A table with the all revisions of each wikipedia articles in the input
#' @export
#'
#' @examples
#'
#' Category_articles_history=get_category_articles_history(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#'


get_category_articles_history=function(list_art){
  dfn_art=c()
  for(art in 1:length(list_art)){
    dfn_load=c()
    print(list_art[art])
    dfn_load=try(get_article_full_history_table(list_art[art]))
    #dfn_load$user=rep(user_of_int[art],dim(dfn_load)[1])
    if(length(dfn_load)>1){
      dfn_art=rbind(dfn_art,dfn_load)
    }

  }
  return(dfn_art)
}


#' Get Category Articles creation
#'
#' This function get a list of wikipedia article titles as input
#' and create a wipipedia creation dates table
#'
#' @param list_art list of wikipedia articles
#' @return A table with the creations instance of each wikipedia articles in the input
#' @export
#'
#' @examples
#'
#' get_category_articles_history=get_category_articles_creation(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#'

get_category_articles_creation=function(list_art){
  dfn_art=c()
  for(art in 1:length(list_art)){
    dfn_load=c()
    print(list_art[art])
    dfn_load=try(get_article_initial_table(list_art[art]))
    #dfn_load$user=rep(user_of_int[art],dim(dfn_load)[1])
    if(length(dfn_load)>1){
      dfn_art=rbind(dfn_art,dfn_load)
    }

  }
  return(dfn_art)
}

#' Get Pages Names in Category
#'
#' This function get a list of wikipedia article titles as input
#' and create a wipipedia creation dates table
#'
#' @param category names of wikipedia category
#' @return list of wikipedia pages in the input category
#' @export
#'
#' @examples
#'
#' get_pagename_in_cat("Circadian rhythm")
#' # For multiple Categories
#' unique(unlist(sapply(category_list,get_pagename_in_cat)))
#'

get_pagename_in_cat=function(category){try({
  cats2=pages_in_category("en", "wikipedia", categories =category,limit = 500) # "Circadian rhythm"

  art_of_int=c()

  for(i in 1:length(cats2$query$categorymembers)){
    if(length(grep("User",cats2$query$categorymembers[[i]]$title))>0){  next}
    else if(length(grep("Category",cats2$query$categorymembers[[i]]$title))>0){next}
    else{
      #print(cats2$query$categorymembers[[i]]$title)
      art_of_int=c(art_of_int,cats2$query$categorymembers[[i]]$title)
    }
  }

  return(unlist(art_of_int))
})
}


doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+" #Good enough

isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]' # good enough

url_regexp = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

tweet_regexp='\\{\\{cite tweet.*?\\}\\}'

news_regexp='\\{\\{cite news.*?\\}\\}'

journal_regexp='\\{\\{cite journal.*?\\}\\}'

web_regexp='\\{\\{cite web.*?\\}\\}'

article_regexp='\\{\\{cite article.*?\\}\\}'

report_regexp='\\{\\{cite report.*?\\}\\}'

court_regexp='\\{\\{cite court.*?\\}\\}'

press_release_regexp='\\{\\{cite press release.*?\\}\\}'

book_regexp='\\{\\{cite book .*?\\}\\}'

pmid_regexp="(?<=(pmid|PMID)\\s?[=:]\\s?)\\d{5,9}"

ref_regexp='<ref>\\{\\{.*?\\}\\}</ref>' # in-text refs!

cite_regexp='\\{\\{[c|C]ite.*?\\}\\}'

wikihyperlink_regexp='\\[\\[.*?\\]\\]'

template_regexp='\\{\\{pp.*?\\}\\}'


regexp_list=c(
  doi_regexp= "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+", #Good enough

  isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]', # good enough

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

  ref_regexp='<ref>\\{\\{.*?\\}\\}</ref>', # in-text refs!

  cite_regexp='\\{\\{[c|C]ite.*?\\}\\}',

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
#' isbn_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]'
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
#' ref_regexp='<ref>\\{\\{.*?\\}\\}</ref>' # in-text refs!
#'
#' cite_regexp='\\{\\{[c|C]ite.*?\\}\\}'
#'
#' @param rticle_wiki_table a wiki table
#' @param citation_regexp citation regular expression
#'
#'
#' @return list of wikipedia pages in the input category
#' @export
#'
#' @examples
#' Zeitgeber_history=get_article_full_history_table("Zeitgeber")
#' citations_in_wiki_table=get_regex_citations_in_wiki_table(Zeitgeber_history,doi_regexp)
#'
#'
get_regex_citations_in_wiki_table=function(article_wiki_table,citation_regexp){
  citation_fetched=str_match_all(article_wiki_table$`*`, citation_regexp)

df_citation=data.frame(revid=rep(article_wiki_table$revid,unlist(lapply(citation_fetched,length))),citation_fetched=unlist(citation_fetched))

df_citation_revid_art=dplyr::select(article_wiki_table,art,revid)%>%dplyr::right_join(df_citation,by="revid")

return(df_citation_revid_art)
}


#' Annotate DOI List with Eurompmc
#'
#' This function get a list of DOIs as input
#' and create dataframe of annotated DOIs with Eurompmc
#'
#' @param doi_list names of wikipedia category
#' @return dataframe of annotated DOIs with Eurompmc
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' dois_fetched=unique(unlist(str_match_all(art_test$`*`, doi_regexp)))
#' annotate_doi_list_europmc(dois_fetched)

annotate_doi_list_europmc=function(doi_list){
  annotated_doi_df=c()
  for(i in 1:length(doi_list)){ #
    print(i)
    print(doi_list[i])
    annotated_dois_df_load=tryCatch(epmc_search(paste("DOI:",doi_list[i],sep="")),error = function(e) NULL)
    if(is.null(annotated_dois_df_load)){annotated_dois_df_load=tryCatch(epmc_search(doi_list[i]),error = function(e) NULL)}
    if(is.null(annotated_dois_df_load)){next}
    if(dim(annotated_dois_df_load)[1]==1){
      annotated_dois_df_load=dplyr::mutate(annotated_dois_df_load, id = if (exists('id', where = annotated_dois_df_load)) id else NA,
                                           source = if (exists('source', where = annotated_dois_df_load)) source else NA,
                                           pmid = if (exists('pmid', where = annotated_dois_df_load)) pmid else NA,
                                           pmcid = if (exists('pmcid', where = annotated_dois_df_load)) pmcid else NA,
                                           doi = if (exists('doi', where = annotated_dois_df_load)) doi else NA,
                                           title = if (exists('title', where = annotated_dois_df_load)) title else NA,
                                           authorString = if (exists('authorString', where = annotated_dois_df_load)) authorString else NA,
                                           journalTitle = if (exists('journalTitle', where = annotated_dois_df_load)) journalTitle else NA,
                                           pubYear = if (exists('pubYear', where = annotated_dois_df_load)) pubYear else NA,
                                           pubType = if (exists('pubType', where = annotated_dois_df_load)) pubType else NA,
                                           isOpenAccess = if (exists('isOpenAccess', where = annotated_dois_df_load)) isOpenAccess else NA,
                                           citedByCount = if (exists('citedByCount', where = annotated_dois_df_load)) citedByCount else NA,
                                           firstPublicationDate = if (exists('firstPublicationDate', where = annotated_dois_df_load)) firstPublicationDate else NA)
      annotated_dois_df_load=tryCatch(dplyr::select(annotated_dois_df_load,id,source,pmid,pmcid,doi,title,
                                                    authorString,journalTitle,pubYear,pubType,isOpenAccess,citedByCount,
                                                    firstPublicationDate),error = function(e) NULL)
      if(is.null(annotated_dois_df_load)){next}
      annotated_doi_df=rbind(annotated_doi_df,annotated_dois_df_load)
    }
  }
  return(data.frame(annotated_doi_df))

}


#' Annotate DOI List with CrossRef
#'
#' This function get a list of DOIs as input
#' and create dataframe of annotated DOIs with CrossRef
#'
#' @param doi_list names of wikipedia category
#' @return dataframe of annotated DOIs with CrossRef
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' dois_fetched=unique(unlist(str_match_all(art_test$`*`, doi_regexp)))
#' annotate_doi_list_cross_ref(dois_fetched)
#'

annotate_doi_list_cross_ref=function(doi_list){
  doi_bib=cr_cn(dois = doi_list,"bibentry",.progress = "text")

  doi_bib_df=dcast(melt(doi_bib[-(which(lapply(doi_bib,length)==0))]), L1 ~ L2)

  citation_countdf=cr_citation_count(doi = doi_bib_df$doi)

  doi_bib_df=doi_bib_df%>%dplyr::left_join(citation_countdf,by=c("doi"))

  return(doi_bib_df)
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
  ISBN_regexp='(?<=(isbn|ISBN)\\s?[=:]?\\s?)\\d{1,5}-\\d{1,7}-\\d{1,5}-[\\dX]'
  ISBN_fetched=str_match_all(art_text, ISBN_regexp)
  ISBN_count=length(as.character(unlist(ISBN_fetched)))
  return(as.numeric(as.character(ISBN_count)))
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

#' Get Sorce Type count
#'
#' This function get a wikipedia article content as input
#' and return the the count of a citation such as book, website,newspaper, journal
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

  write.table(df_cite_parsed_revid_art,"df_cite_parsed_revid_art.csv",quote = F,row.names = F,sep=";")

  return(df_cite_parsed_revid_art)
}

plot_article_creation_per_year=function(article_initial_table,name_title,Cumsum=T){

  data_edit_pattern= article_initial_table  #dplyr::select(article_initial_table,art,user,timestamp,size)%>%dplyr::filter(art %in% art_sci_of_int)


  data_edit_pattern$tsc=matrix(unlist(strsplit(as.character(data_edit_pattern$timestamp),"T")),byrow=T,ncol=2)[,1]
  data_edit_pattern$tsc=as.Date(data_edit_pattern$tsc)

  #ggplot(data_edit_pattern, aes(x =tsc)) +scale_x_date(date_breaks = "1 year",date_labels = "%Y")+ geom_density(adjust = 1/100)+ggtitle("articles edits in HMC category")+theme_classic()

  dfcr=data_edit_pattern %>%
    group_by(art) %>%
    dplyr::mutate(
      first = dplyr::first(tsc)
    ) %>% data.frame()%>%dplyr::select(art,tsc)%>% unique()

  #data_edit_pattern$tsc=matrix(unlist(strsplit(as.character(data_edit_pattern$ts),"T")),byrow=T,ncol=2)[,1]
  data_edit_pattern$tsc=as.Date(data_edit_pattern$tsc)

  dfcr_bin=data.frame(count=as.numeric(table(cut( dfcr$tsc, breaks="1 year"))),date=as.Date(names(table(cut(  dfcr$tsc, breaks="1 year")))))
if(Cumsum==T){
  ggplot(dfcr_bin, aes(x = date,y=cumsum(count))) +scale_x_date()+ geom_point()+ geom_line()+ggtitle(name_title)+theme_classic()
}else{
  ggplot(dfcr_bin, aes(x = date,y=count)) +scale_x_date()+ geom_point()+ geom_line()+ggtitle(name_title)+theme_classic()
}
  }


plot_static_timeline=function(article_initial_table_sel){

  article_initial_table_sel$tsc=matrix(unlist(strsplit(as.character(article_initial_table_sel$timestamp),"T")),byrow=T,ncol=2)[,1]
  article_initial_table_sel$tsc=as.Date(article_initial_table_sel$tsc)

  dfcr=article_initial_table_sel %>%
    group_by(art) %>%
    dplyr::mutate(
      first = dplyr::first(tsc)
    ) %>% data.frame()%>%dplyr::select(art,tsc)%>% unique()

  sel_tmp=article_initial_table_sel[,c("revid","art","user","size","timestamp")]

  dfcr=dplyr::inner_join(dfcr,sel_tmp,by=c("art"))

  P1= ggplot(dfcr,aes(x=tsc,y=0))+
    geom_point()+
    geom_label_repel(aes(label =art),nudge_y= 1,
                     direction = "y",
                     angle        = 0,
                     vjust        = 0,segment.alpha =0.2,
                     size=3,segment.size = .5)+
    scale_x_date()+theme_minimal()+
    ylim(0,1)+scale_colour_brewer("type", palette="Dark2")+
    scale_fill_brewer("type", palette="Dark2")+
    theme(legend.position = "bottom")#+facet_wrap(~type,ncol=1)

  print(P1)
}

plot_navi_timeline=function(article_initial_table_sel,article_info_table){

  article_initial_table_sel$tsc=matrix(unlist(strsplit(as.character(article_initial_table_sel$timestamp),"T")),byrow=T,ncol=2)[,1]
  article_initial_table_sel$tsc=as.Date(article_initial_table_sel$tsc)

  dfcr=article_initial_table_sel %>%
    group_by(art) %>%
    dplyr::mutate(
      first = dplyr::first(tsc)
    ) %>% data.frame()%>%dplyr::select(art,tsc)%>% unique()

  #sel_tmp=article_initial_table[,c("revid","art","user","size","timestamp")]

  # dfcr=dplyr::inner_join(dfcr,sel_tmp,by=c("art"))
  dfcr=dplyr::inner_join(dplyr::select(article_info_table,title,pageid),dfcr,by=c("title"="art"))
  dfcr=unique(dfcr)
  dfcr$wiki=paste("http://en.wikipedia.org/?curid=",dfcr$pageid,sep="")
  dfcr$label=paste('<a href="',dfcr$wiki,'">',dfcr$title,'</a>',sep="")


  dfcr$wiki=paste("http://en.wikipedia.org/?curid=",dfcr$pageid,sep="")

  dfcr$label=paste('<a href="',dfcr$wiki,'">',dfcr$title,'</a>',sep="")


  color_pal=c("#fbb4ae",
              "#b3cde3",
              "#ccebc5",
              "#decbe4",
              "#fed9a6",
              "#ffffcc")

  data <- data.frame(
    id      = 1:dim(dfcr)[1],
    content = dfcr$label,
    start   = dfcr$tsc,
    end     =rep(NA,dim(dfcr)[1]))

  ui <- fluidPage(
    timevisOutput("timeline")
  )

  server <- function(input, output, session) {
    output$timeline <- renderTimevis({
      timevis(data,zoomFactor=0.1)%>%setWindow("2004-04-01","2006-01-01")
    })
  }

  shinyApp(ui = ui, server = server)

}


export_extracted_citations_xlsx=function(article_most_recent_table,name_file_prefix){

  for(i in 1:length(regexp_list)){
    tmp_table=get_regex_citations_in_wiki_table(article_most_recent_table,as.character(regexp_list[i]))
    #tmp_table=tmp_table%>%dplyr::filter(citation!="pmid",citation!="isbn")

    #if(i ==1){
    try(write.xlsx(tmp_table, file=paste(name_file_prefix,as.character(names(regexp_list)[i]),"exctracted_citations.xlsx",sep="_"),
                   sheetName=as.character(names(regexp_list)[i]), append=FALSE))
    #}else{
    # try(write.xlsx(tmp_table, file="Exctracted_citations.xlsx",
    #            sheetName=as.character(names(regexp_list)[i]), append=TRUE))
    #}
  }
}

extract_citations_regexp=function(article_most_recent_table){
  extracted_citation_list=list()

  for(i in 1:length(regexp_list)){
    tmp_table=get_regex_citations_in_wiki_table(article_most_recent_table,as.character(regexp_list[i]))
    #tmp_table=tmp_table%>%dplyr::filter(citation!="pmid",citation!="isbn")
    extracted_citation_list[[i]]=tmp_table
  }
  names(extracted_citation_list)=names(regexp_list)
  return(extracted_citation_list)
}

annotate_isbn_google=function(isbn_nb){
  isbn_nb=gsub("-","",isbn_nb)
  cmd=paste("https://www.googleapis.com/books/v1/volumes?q=isbn:",isbn_nb,sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tryCatch({
    if(parsed$totalItems!=0){
      output_df=parsed$items$volumeInfo[,c("title","publisher","publishedDate","description")]
      output_df$categories=paste(unlist(parsed$items$volumeInfo$categories),sep=", ",collapse=", ")
      output_df$authors=paste(unlist(parsed$items$volumeInfo$authors),sep=", ",collapse=", ")
      return(output_df)}
  }, error = function(err) {return(NULL)})
}


annotate_doi_list_altmetrics=function(doi_list){
  alm <- function(x)  tryCatch(altmetrics(doi = x) %>% altmetric_data(), error=function(e) NULL)
  results <- pmap_df(doi_list, alm)
  results=dplyr::select(results,title,doi,pmid,altmetric_jid,issns,journal,authors1,type,altmetric_id,is_oa,cited_by_fbwalls_count,cited_by_posts_count,cited_by_tweeters_count,cited_by_videos_count,cited_by_feeds_count,cited_by_accounts_count,score ,published_on,added_on,url)

  return(results)
}
##anno_dois_altmetrics=annotate_doi_list_altmetrics(list(unique(as.character(doi_30K[1:200,1]))))

annotate_isbn_list_altmetrics=function(isbn_list){
  alm <- function(x)  tryCatch(altmetrics(isbn = x) %>% altmetric_data(), error=function(e) NULL)
  results <- pmap_df(isbn_list, alm)
  return(results)
}

annotate_doi_to_bibtex_cross_ref=function(doi_list){
  doi_bib=cr_cn(dois = doi_list,"bibtex",.progress = "text")
  return(doi_bib)
}

export_doi_to_bib=function(doi_list,file_name="doi_file_top_high_100520.bib"){
  dfa=annotate_doi_to_bibtex_cross_ref(doi_list)
  lapply(dfa, function(x) write.table( x, file_name  , append= T, sep='\n\n' ,quote = F,col.names = F,row.names = F))
}


annotate_isbn_openlib=function(isbn_nb){
  isbn_nb=gsub("-","",isbn_nb)
  cmd=paste("https://openlibrary.org/api/books?bibkeys=ISBN",isbn_nb,"&format=json",sep="")
  resp=GET(cmd)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tryCatch({
    return(as.data.frame(parsed))
  }, error = function(err) {return(NULL)})
}

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

  get_pdfs_top20source=function(df_cite_parsed_revid_art){
    #pdf("top20source.pdf")
    for(i in 1:length(source_types_list)){
      plot_top_source(df_cite_parsed_revid_art,as.character(source_types_list[i]))
    }
    #dev.off()
  }

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


  get_tables_initial_most_recent_full_info=function(all_art){
    #all_art=covid_imp_art

    article_initial_table=c()
    article_most_recent_table=c()
    article_info_table=c()
    article_full_history_table=c()

    for(i in 1:length(all_art)){
      print(all_art[i])
      try({
        article_initial_table=rbind(article_initial_table, get_article_initial_table(all_art[i]))
        article_most_recent_table=rbind(article_most_recent_table,get_article_most_recent_table(all_art[i]))
        article_info_table=rbind(article_info_table,get_article_info_table(all_art[i]))
        article_full_history_table=rbind(article_full_history_table,get_article_full_history_table(all_art[i]))
      })
    }
    return(list(article_initial_table=article_initial_table,article_most_recent_table=article_most_recent_table,article_info_table=article_info_table,article_full_history_table=article_full_history_table))
  }



  plot_venn_from_two_list=function(listA,listB,colors_2=c("red", "blue")){

    grid.newpage()
    venn.plot <- draw.pairwise.venn(
      length(unlist(listA)),length(unlist(listB)),length(intersect(unlist(listA),unlist(listB))),
      euler.d = TRUE,
      sep.dist = 0.03,col=c(NA,NA),
      rotation.degree = 0,category   = c(names(listA),names(listB)),fill=colors_2,
      lty             = 1, alpha     = 0.2,cex = rep(3, 3),cat.cex=c(3,3)
    )

    grid.draw(venn.plot)
  }


  page_view_plot=function(article_name,ymax=NA,start="2020010100",end="2020050100"){
    page_view=data.frame(article_pageviews(project = "en.wikipedia", article = article_name,start=start,end=end))

    page_view$date=ymd(page_view$date)

    Pl=ggplot(page_view,aes(date,views))+ geom_area(fill="darkgreen")+theme_classic()+ggtitle(paste(article_name, "daily views"))+
      scale_y_continuous(limits=c(0,ymax),expand=c(0,0))+scale_x_date(limits=c(as.Date(as.POSIXlt("2020010100",format="%Y%m%d%H",ts="GMT")),as.Date(as.POSIXlt("2020050100",format="%Y%m%d%H",ts="GMT"))))
    print(Pl)
  }

  page_edit_plot=function(article_name,ymax=NA,start="2020010100",end="2020050100"){
    history=get_article_full_history_table(article_name)
    history$ts=as.Date(sapply(history$timestamp,function(x){return(unlist(strsplit(x,"T"))[1])}))
    df_edits=dplyr::select(history,ts)%>%dplyr::filter(ts>as.Date(as.POSIXlt(start,format="%Y%m%d%H",ts="GMT"))&ts<as.Date(as.POSIXlt(end,format="%Y%m%d%H",ts="GMT"))) #to test
    df_edits_bin=data.frame(count=as.numeric(table(cut( df_edits$ts, breaks="1 week"))),date=as.Date(names(table(cut( df_edits$ts, breaks="1 week")))))
    Pl=ggplot(df_edits_bin,aes(date,count))+
      geom_area(fill="darkred")+theme_classic()+
      ggtitle(paste(article_name, "weekly edits"))+
      scale_y_continuous(limits=c(0,ymax),expand=c(0,0))+
      scale_x_date(limits=c(as.Date(as.POSIXlt(start,format="%Y%m%d%H",ts="GMT")),as.Date(as.POSIXlt(end,format="%Y%m%d%H",ts="GMT"))))
    print(Pl)
  }

  padNA <- function (mydata, rowsneeded, first = TRUE)
  {
    temp1 = colnames(mydata)
    rowsneeded = rowsneeded - nrow(mydata)
    temp2 = setNames(
      data.frame(matrix(rep(NA, length(temp1) * rowsneeded),
                        ncol = length(temp1))), temp1)
    if (isTRUE(first)) rbind(mydata, temp2)
    else rbind(temp2, mydata)
  }

  dotnames <- function(...) {
    vnames <- as.list(substitute(list(...)))[-1L]
    vnames <- unlist(lapply(vnames,deparse), FALSE, FALSE)
    vnames
  }

  Cbind <- function(..., first = TRUE) {
    Names <- dotnames(...)
    datalist <- setNames(list(...), Names)
    nrows <- max(sapply(datalist, function(x)
      ifelse(is.null(dim(x)), length(x), nrow(x))))
    datalist <- lapply(seq_along(datalist), function(x) {
      z <- datalist[[x]]
      if (is.null(dim(z))) {
        z <- setNames(data.frame(z), Names[x])
      } else {
        if (is.null(colnames(z))) {
          colnames(z) <- paste(Names[x], sequence(ncol(z)), sep = "_")
        } else {
          colnames(z) <- paste(Names[x], colnames(z), sep = "_")
        }
      }
      padNA(z, rowsneeded = nrows, first = first)
    })
    do.call(cbind, datalist)
  }

  # get_anyCount=function(art_text,regexp){
  #   ref_regexp=regexp
  #   ref_fetched=str_match_all(art_text, ref_regexp)
  #   ref_count=length(as.character(unlist(ref_fetched)))
  #   return(as.numeric(as.character(ref_count)))
  # }
  #
  # regexp_list

  plot_top_source=function(df_cite_parsed_revid_art,source_type){

    #publisher
    P1=df_cite_parsed_revid_art%>%dplyr::filter(variable==source_type)%>%dplyr::mutate(value=gsub(" ","",value))%>%dplyr::filter(value!="")%>%
      dplyr::group_by(value)%>%
      summarise(count=n())%>%arrange(-count)%>%
      top_n(20)%>%ggplot(aes(reorder(value,count),count))+
      geom_bar(stat="identity")+coord_flip()+ggtitle(paste("Top 20",source_type))
    print(P1)
  }

  plot_distribution_source_type=function(df_cite_count_revid_art){
    P1=df_cite_count_revid_art%>%dplyr::filter(cite_type %in% c("journal","news","web","book"))%>%dplyr::group_by(revid,cite_type,Freq)%>%
      ggplot(aes(cite_type,Freq))+ geom_boxplot(width=0.6)+coord_flip() #geom_violin(trim = F)+
    print(P1)
  }


  get_subcat_table=function(catname,replecement="_"){#catname,depth_cat
    cat_table=c()
    catname=gsub("Category:","",catname)
    catname=gsub(" ",replecement,catname)
    #api.php?action=query&list=categorymembers&cmtitle=Category:2019-20%20coronavirus%20pandemic&cmsort=timestamp&cmd_r=desc #&cmprop=ids|title|type|timestamp #&cmtype=subcat
    cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",catname,"&cmlimit=5&cmprop=ids|title|type|timestamp&format=json&cmtype=subcat",sep="")
    resp=GET(cmd)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
    cat_table=rbind(cat_table,parsed$query$categorymembers)
    try({
      while(length(parsed$continue$cmcontinue)==1){ #&cmtype=subcat
        rvc=parsed$continue$cmcontinue
        cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",catname,"&cmlimit=5&cmprop=ids|title|type|timestamp&format=json&cmtype=subcat&cmcontinue=",rvc,sep="")
        resp=GET(cmd)
        parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
        cat_table=rbind(cat_table,parsed$query$categorymembers)
      }
    })
    cat_table$parent_cat=rep(paste("Category:",catname,sep=""),dim(cat_table)[1])
    return(cat_table)
  }

  #test=get_subcat_table("Category:Impact of the COVID-19 pandemic on sports")

  get_pages_in_cat_table=function(catname,replecement="_"){#catname,depth_cat
    cat_table=c()
    catname=gsub("Category:","",catname)
    catname=gsub(" ",replecement,catname)
    #api.php?action=query&list=categorymembers&cmtitle=Category:2019-20%20coronavirus%20pandemic&cmsort=timestamp&cmd_r=desc #&cmprop=ids|title|type|timestamp #&cmtype=subcat
    cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",catname,"&cmlimit=5&cmprop=ids|title|type|timestamp&format=json&cmtype=page",sep="")
    resp=GET(cmd)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
    cat_table=rbind(cat_table,parsed$query$categorymembers)
    try({
      while(length(parsed$continue$cmcontinue)==1){ #&cmtype=subcat
        rvc=parsed$continue$cmcontinue
        cmd=paste("https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",catname,"&cmlimit=5&cmprop=ids|title|type|timestamp&format=json&cmtype=page&cmcontinue=",rvc,sep="")
        resp=GET(cmd)
        parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
        cat_table=rbind(cat_table,parsed$query$categorymembers)
      }
    })
    cat_table$parent_cat=rep(paste("Category:",catname,sep=""),dim(cat_table)[1])
    return(cat_table)
  }

  get_subcat_multiple=function(catlist,replecement="_"){
    cat_table_list=c()
    for(i in 1:length(catlist)){
      try({
        cat_table_list=rbind(cat_table_list,get_subcat_table(catlist[i],replecement))
      }, silent = TRUE)
    }
    return(cat_table_list)
  }

 # get_subcat_multiple(test$title)

  get_page_in_cat_multiple=function(catlist,replecement="_"){
    cat_table_list=c()
    for(i in 1:length(catlist)){
      try({
        cat_table_list=rbind(cat_table_list,get_pages_in_cat_table(catlist[i],replecement))
      }, silent = TRUE)
    }
    return(cat_table_list)
  }

  get_subcat_with_depth=function(catname,depth,replecement="_"){
    table_out=get_subcat_table(catname)
    while(depth>0){
      table_out=rbind(table_out,get_subcat_multiple(table_out$title,replecement))
      depth=depth-1
    }
    return(unique(table_out))
  }

