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

write_wiki_history_to_xlsx=function(wiki_hist,file_name){
  wiki_hist[is.na(wiki_hist)]="-"
  wiki_hist[is.null(wiki_hist)]="-"
  df <- data.frame(art=wiki_hist$art,revid=wiki_hist$revid,parentid=wiki_hist$parentid,user=wiki_hist$user,userid=wiki_hist$userid,
                   timestamp=wiki_hist$timestamp,size=wiki_hist$size,comment=wiki_hist$comment,content=wiki_hist$`*`,stringsAsFactors=FALSE)

  write.xlsx(df,paste(file_name,"wiki_table.xlsx",sep="_"), sheetName="Sheet1",  col.names=TRUE, row.names=F, append=FALSE, showNA=T)
}


#' annotate and export DOI list to bibtex file
#'
#' This function get a list of DOI as input, annotate it with Rcrossref and export a bibtexfile.
#' in the working directory.
#'
#' @param doi_list list of DOI
#' @param file_name output file name
#' @return nothing
#' @export
#'
#' @examples
#'
#' category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#' extracted_citation_table=get_regex_citations_in_wiki_table(category_most_recent, "10\\.\\d{4,9}/[-._;()/:a-z0-9A-Z]+") # doi_regexp
#' export_doi_to_bib(as.character(extracted_citation_table$citation_fetched)[1:5],"output.bib")
#'


export_doi_to_bib=function(doi_list,file_name="file.bib"){
  dfa=annotate_doi_to_bibtex_cross_ref(doi_list)
  lapply(dfa, function(x) write.table( x, file_name  , append= T, sep='\n\n' ,quote = F,col.names = F,row.names = F))
}


#' Export all regex from Article history Table to multiple xlsx with a
#'
#' This function get a wikipedia article table as input and the name of the target xls file.
#' an xlsx with the table is written in the working directory.
#'
#' @param article_most_recent_table wiki history or most recent table of multiple wikipedia pages
#' @param name_file_prefix output file name prefix
#' @return nothing
#' @export
#'
#' @examples
#'
#' category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#' export_extracted_citations_xlsx(category_most_recent, "example")

export_extracted_citations_xlsx=function(article_most_recent_table,name_file_prefix){

  for(i in 1:length(pkg.env$regexp_list)){
    tmp_table=get_regex_citations_in_wiki_table(article_most_recent_table,as.character(pkg.env$regexp_list[i]))
    #tmp_table=tmp_table%>%dplyr::filter(citation!="pmid",citation!="isbn")

    #if(i ==1){
    try(write.xlsx(tmp_table, file=paste(name_file_prefix,as.character(names(pkg.env$regexp_list)[i]),"exctracted_citations.xlsx",sep="_"),
                   sheetName=as.character(names(pkg.env$regexp_list)[i]), append=FALSE))
    #}else{
    # try(write.xlsx(tmp_table, file="Exctracted_citations.xlsx",
    #            sheetName=as.character(names(regexp_list)[i]), append=TRUE))
    #}
  }
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

#' Annotate DOI List with CrossRef
#'
#' This function get a list of DOIs as input
#' and create a bib of annotated DOIs with CrossRef
#'
#' @param doi_list names of wikipedia category
#' @return bib structure of annotated DOIs with CrossRef
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' dois_fetched=unique(unlist(str_match_all(art_test$`*`, doi_regexp)))
#' annotate_doi_to_bibtex_cross_ref(dois_fetched)
#'

annotate_doi_to_bibtex_cross_ref=function(doi_list){
  doi_bib=cr_cn(dois = doi_list,"bibtex",.progress = "text")
  return(doi_bib)
}


#' Annotate single isbn with google book API
#'
#' This function get an ISBN as input
#' and return a dataframe of annotation DOIs from google book API
#'
#' @param isbn_nb ISBN number
#' @return dataframe of annotation DOIs from google book API
#' @export
#'
#' @examples
#'
#'
#' annotate_isbn_google("978-0-15-603135-6")
#'

annotate_isbn_google=function(isbn_nb){
  isbn_nb=gsub("-","",isbn_nb)
  isbn_nb=gsub(" ","",isbn_nb)
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


#' Annotate single ISBN with Open Library
#'
#' This function retrieves metadata for a given ISBN using the Open
#' Library API.
#'
#' @param isbn_nb ISBN number
#' @return A data frame with the Open Library response or \code{NULL} if
#'   the ISBN is not found.
#' @export
#'
#' @examples
#' annotate_isbn_openlib("9780156031356")

annotate_isbn_openlib=function(isbn_nb){ # to improve
  isbn_nb=gsub("-","",isbn_nb)
  isbn_nb=gsub(" ","",isbn_nb)
  cmd=paste("https://openlibrary.org/api/books?bibkeys=ISBN",isbn_nb,"&format=json",sep="")
  resp=GET(cmd)
  #https://openlibrary.org/books/OL4749139M.json
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
  tryCatch({
    return(as.data.frame(parsed))
  }, error = function(err) {return(NULL)})
}


#' Annotate DOI List with altmetrics
#'
#' This function get a list of DOIs as input
#' and create a dataframe of annotated DOIs with altmetrics
#'
#' @param doi_list names of wikipedia category
#' @return dataframee of annotated DOIs with altmetrics
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' dois_fetched=unique(unlist(str_match_all(art_test$`*`, doi_regexp)))
#' annotate_doi_list_altmetrics(list(unique(as.character(dois_fetched))))
#'


annotate_doi_list_altmetrics=function(doi_list){
  alm <- function(x)  tryCatch(altmetrics(doi = x) %>% altmetric_data(), error=function(e) NULL)
  results <- pmap_df(doi_list, alm)
  results=dplyr::select(results,title,doi,pmid,altmetric_jid,issns,journal,authors1,type,altmetric_id,is_oa,cited_by_fbwalls_count,cited_by_posts_count,cited_by_tweeters_count,cited_by_videos_count,cited_by_feeds_count,cited_by_accounts_count,score ,published_on,added_on,url)

  return(results)
}
##anno_dois_altmetrics=annotate_doi_list_altmetrics(list(unique(as.character(doi_30K[1:200,1]))))

#' Annotate ISBN List with altmetrics
#'
#' This function get a list of ISBN as input
#' and create a dataframe of annotated ISBN with altmetrics
#'
#' @param doi_list names of wikipedia category
#' @return dataframee of annotated ISBN with altmetrics
#' @export
#'
#' @examples
#' art_test=get_article_most_recent_table("Zeitgeber")
#' isbn_fetched=unique(unlist(str_match_all(art_test$`*`, isbn_regexp)))
#' annotate_isbn_list_altmetrics(list(unique(as.character(isbn_fetched))))
#'

annotate_isbn_list_altmetrics=function(isbn_list){
  alm <- function(x)  tryCatch(altmetrics(isbn = x) %>% altmetric_data(), error=function(e) NULL)
  results <- pmap_df(isbn_list, alm)
  return(results)
}
