
# Retrieve article content, history/revisions, information of page, most recent version


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
#' @param date_an input date to select most recent version with the following format : 2020-05-01T00:00:00Z
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
#' category_articles_history=get_category_articles_creation(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
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

#' Get Category Articles most recent
#'
#' This function get a list of wikipedia article titles as input
#' and create a wipipedia table with most recent version of the pages
#'
#' @param list_art list of wikipedia articles
#' @return A table with the most recent instance of each wikipedia articles in the input
#' @export
#'
#' @examples
#'
#' category_most_recent=get_category_articles_most_recent(c("Zeitgeber","Advanced sleep phase disorder","Sleep deprivation"))
#'

get_category_articles_most_recent=function(list_art){
  dfn_art=c()
  for(art in 1:length(list_art)){
    dfn_load=c()
    print(list_art[art])
    dfn_load=try(get_article_most_recent_table(list_art[art]))

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
      art_of_int=c(art_of_int,cats2$query$categorymembers[[i]]$title)
    }
  }
  return(unlist(art_of_int))
})
}

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






#' Plot article creation over time
#'
#' Creates a cumulative or per-year plot of article creation dates.
#'
#' @param article_initial_table Table of initial revisions for articles.
#' @param name_title Plot title.
#' @param Cumsum Logical indicating whether to plot cumulative counts.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' initial <- get_article_initial_table("Zeitgeber")
#' plot_article_creation_per_year(initial, "Zeitgeber")

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

#' Plot static article timeline
#'
#' Displays article creation dates in a static ggplot timeline.
#'
#' @param article_initial_table_sel Subset of initial article table.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' initial <- get_article_initial_table("Zeitgeber")
#' plot_static_timeline(initial)

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

#' Plot interactive article timeline
#'
#' Creates an interactive timevis timeline linking to articles.
#'
#' @param article_initial_table_sel Subset of initial article table.
#' @param article_info_table Table with article page information.
#' @return No return value, launches a Shiny app.
#' @export
#'
#' @examples
#' initial <- get_article_initial_table("Zeitgeber")
#' info <- get_article_info_table("Zeitgeber")
#' plot_navi_timeline(initial, info)

plot_navi_timeline=function(article_initial_table_sel,article_info_table){
#' Plot page views
#'
#' Generates an area plot of Wikipedia page views over time for a given
#' article.
#'
#' @param article_name Wikipedia article title.
#' @param ymax Optional y-axis maximum.
#' @param start Start date in YYYYMMDDHH format.
#' @param end End date in YYYYMMDDHH format.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' page_view_plot("Zeitgeber")

  page_view_plot=function(article_name,ymax=NA,start="2020010100",end="2020050100"){
    page_view=data.frame(article_pageviews(project = "en.wikipedia", article = article_name,start=start,end=end))

    page_view$date=ymd(page_view$date)

    Pl=ggplot(page_view,aes(date,views))+ geom_area(fill="darkgreen")+theme_classic()+ggtitle(paste(article_name, "daily views"))+
      scale_y_continuous(limits=c(0,ymax),expand=c(0,0))+scale_x_date(limits=c(as.Date(as.POSIXlt("2020010100",format="%Y%m%d%H",ts="GMT")),as.Date(as.POSIXlt("2020050100",format="%Y%m%d%H",ts="GMT"))))
    print(Pl)
  }

#' Plot page edits
#'
#' Displays the number of edits per week for a given article.
#'
#' @param article_name Wikipedia article title.
#' @param ymax Optional y-axis maximum.
#' @param start Start date in YYYYMMDDHH format.
#' @param end End date in YYYYMMDDHH format.
#' @return No return value, generates a plot.
#' @export
#'
#' @examples
#' page_edit_plot("Zeitgeber")

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
#'
#' @examples
#' get_subcat_table("Category:Biology")

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

# test=get_subcat_table("Category:Impact of the COVID-19 pandemic on sports")

#' Retrieve pages in a category
#'
#' Returns a table of pages that belong to a specific Wikipedia category.
#'
#' @param catname Category name.
#' @param replecement Character replacement for spaces.
#' @return Data frame of pages in the category.
#' @export
#'
#' @examples
#' get_pages_in_cat_table("Category:Biology")

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

#' Retrieve subcategories for multiple categories
#'
#' @param catlist Vector of category names.
#' @param replecement Character replacement for spaces.
#' @return Combined data frame of subcategories.
#' @export
#'
#' @examples
#' get_subcat_multiple(c("Category:Biology"))

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

#' Retrieve pages for multiple categories
#'
#' @param catlist Vector of category names.
#' @param replecement Character replacement for spaces.
#' @return Combined data frame of pages.
#' @export
#'
#' @examples
#' get_page_in_cat_multiple(c("Category:Biology"))

  get_page_in_cat_multiple=function(catlist,replecement="_"){
    cat_table_list=c()
    for(i in 1:length(catlist)){
      try({
        cat_table_list=rbind(cat_table_list,get_pages_in_cat_table(catlist[i],replecement))
      }, silent = TRUE)
    }
    return(cat_table_list)
  }

#' Retrieve subcategories with depth
#'
#' Recursively fetches subcategories up to a specified depth.
#'
#' @param catname Category name.
#' @param depth Depth to traverse.
#' @param replecement Character replacement for spaces.
#' @return Data frame of subcategories up to the desired depth.
#' @export
#'
#' @examples
#' get_subcat_with_depth("Category:Biology", 1)

  get_subcat_with_depth=function(catname,depth,replecement="_"){
    table_out=get_subcat_table(catname)
    while(depth>0){
      table_out=rbind(table_out,get_subcat_multiple(table_out$title,replecement))
      depth=depth-1
    }
    return(unique(table_out))
  }
