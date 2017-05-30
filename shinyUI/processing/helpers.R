
libraries <- function(){
  library(pacman)
  p_load(stringi)
  p_load(httr)
  p_load(dplyr)
  p_load(data.table)
  p_load(plotly)
  p_load(shiny)
  p_load(shinythemes)
  p_load(foreach)
  
}


poster.pic.name <- function(vector){
  foreach(i =1:length(vector),.combine = c)%do%{
    name <- paste0(gsub(" ","", vector[i]),".png")
    name <- gsub(":","", name)
    name <- gsub("'","", name)
    name <- gsub("-","", name)
    name <- gsub("&","", name)
    return(name)
  } -> result ; rm(i)
  
  return(result)
}








Viz <- function(df){
  
  df <- data.table(df)
  df[, mapping_size:= critics_score * bo_score]
  plot <- df %>% plot_ly(x = bo_score,
                         y = critics_score,
                         type = "scatter",
                         mode = "markers",
                         size = mapping_size,
                         color = class,
                         colors = "RdYlGn",
                         opacity = mapping_size,
                         text = paste(toupper(title),"<br>",
                                      "Grade: ", toupper(class), "<br>",
                                      "Overall Critical Reception: ", round(critics_score,2),"%" ,"<br>",
                                      "Box Office Performance Index: ", round(bo_score,2),"<br>",
                                      "All-time Ranking: ", overall_rank),
                         hoverinfo = "text")
  
  
  
  
  
  plot <- layout(plot,
                 title = "Ranking Visulation (hover on bubble for movie info)",
                 titlefont = list(size = 15, color = "white"),
                 legend = list(font = list(size = 15, color = "white"),
                               x = 0.9,y = 0.1
                               ),
                 autosize = F, 
                 width = 1400, 
                 height = 650, 
                 paper_bgcolor='rgba(0,0,0,0)',
                 plot_bgcolor='rgba(0,0,0,0)',
                 # margin = m,
                 xaxis = list(title = "Overall Box Office Performance Index (higher is better)",
                              showgrid = F,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              tickfont = list(size = 15, color = "white"),
                              titlefont = list(size = 15, color = "white")
                              # showgrid = F,
                              # tickcolor = toRGB("white"),
                              # tickfont = list(color = toRGB("white"), size = 10),
                              # zeroline = FALSE,
                              # showline = FALSE,
                              # showticklabels = T
                              ),
                 yaxis = list(title = "Overall Critical Reception (in %)",
                              showgrid = F,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              tickfont = list(size = 15, color = "white"),
                              titlefont= list(size = 15, color = "white")
                              # showgrid = F,
                              # tickcolor = toRGB("white"),
                              # tickfont = list(color = toRGB("white"), size = 10),
                              # zeroline = FALSE,
                              # showline = FALSE,
                              # showticklabels = T
                              )
                 )
  
  
  
  #Get the list for the plot
  plot <- plotly_build(plot)

  foreach(i =1:length(plot$data)) %do% {

    # Pick up the hover text
    hvrtext <- plot$data[[i]]$text
    # Split by line break and wt
    hvrtext_fixed <- stri_split(hvrtext, fixed = '<br>mapping_size')
    # Get the first element of each split
    hvrtext_fixed <- lapply(hvrtext_fixed, function(x) x[1])
    # Convert back to vector
    hvrtext_fixed <- as.character(hvrtext_fixed)
    # Assign as hovertext in the plot
    plot$data[[i]]$text <- hvrtext_fixed


  }

  rm(list=c("hvrtext","hvrtext_fixed"))
  
  df <- df %>% select(which(names(df) %in% c("title","IP","studio","combined_score","class")))
  setnames(df, names(df), c("Studio","Title","Combined Critics/BoxOffice Score","IP","Grade"))
  
  
  return(list("viz"=plot, "table" = df))
  
}




rank.by <- function(vector, List){
  
  df <- data.table(List$processed.data)
  
  options <- c(
    "critics_score","bo_score",
    "RT_perc","RT_audience_perc",
    "RT_rating", "RT_audience_rating",
    "RT_score","RT_audience_score",
    "metascore","imdb_rating",
    "foreign_BO","domestic_BO","combined_BO",
    "avg","change","rank")
  
  if (vector %in% c("")){
    return(data.table("Empty" = "Yes", "title" ="N/A", "rank" = "N/A"))
  }else{
    
    bo.raw <- data.table(List$raw.data$BO)
    critics.raw <- data.table(List$raw.data$critics)
    
    
    if ( vector %in% names(bo.raw)){
      table <- bo.raw %>% select(which(names(bo.raw)%in% c("title",vector)))
      
      if ( vector == "foreign_BO") { table <- table %>%arrange(-table[[2]])}
      else if ( vector == "domestic_BO") { table <- table %>%arrange(-table[[2]])}
      else {table <- table %>%arrange(-table[[1]]) %>% select(2,1)}
      
      
      table$rank <- seq(1,dim(table)[1])
      table <- table %>%select(3,1,2)
      x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","poster")))
      table <- merge(table, x, by.y = "title", by.x= "title")
      table <- data.table(table) %>% select(c(2,1,3,6,5)) %>% arrange(rank)
      setnames(table, paste0(vector,".x"), vector)
      
      table$poster <-  poster.pic.name(table$title)
      
      return(table)
    }
    
    else if ( vector %in% names(critics.raw) ){
      table <- critics.raw %>% select(which(names(critics.raw)%in% c("title",vector)))
      table <- table %>% arrange(-table[[1]])%>% select(2,1)
      
      
      table$rank <- seq(1,dim(table)[1])
      table <- table %>%select(3,1,2)
      
      if (vector %in% c("imdb_rating","metascore")){
        
        x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","poster")))
        table <- merge(table, x, by.y = "title", by.x= "title")
        table <- data.table(table)%>% arrange(rank)%>% select(c(2,1,3,6,5))
        setnames(table, paste0(vector,".x"), vector)
        table <- table %>%select(2,3,4,5)
        
      } else{
        x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","poster")))
        table <- merge(table, x, by.y = "title", by.x= "title")
        table <- data.table(table)%>% arrange(rank)%>% select(c(1,3,5,4))
      }
      table$poster <- poster.pic.name(table$title)
      
      
      return(table)
    }
    
    else {
      table <- df %>% select(which(names(df)%in% c("title",vector)))
      table <- table %>% arrange(-table[[1]]) %>% select(2,1)
      
      
      table$Rank <- seq(1,dim(table)[1])
      table <- table %>%select(3,1,2)
      
      if (vector %in% c("avg","change","rank")){
        x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","theater_run_score","poster")))
        table <- merge(table, x, by.y = "title", by.x= "title")
        table[[3]] <-  table[[3]] * table$theater_run_score
        table <- data.table(table) %>% select(c(2,1,3,5,7,6))%>% arrange(desc(table[[3]]))
        setnames(table, paste0(vector,".x"), vector)
        # table <- table %>% arrange(-vector)
        # table[, Rank:= seq(1, dim(table)[1])]
        table <- table %>% select(c(2,3,5,6))
      } 
      
      else if (vector %in% c("bo_score")){
        x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","theater_run_score","poster")))
        table <- merge(table, x, by.y = "title", by.x= "title")
        table[[3]] <-  table[[3]] * table$theater_run_score
        table <- data.table(table) %>% select(c(2,1,3,5,7,6))%>% arrange(desc(table[[3]]))
        setnames(table, paste0(vector,".x"), vector)
        table <- table %>% select(c(2,3,5,6))
      }
      else {
        x <- df %>% select(which(names(df)%in% c("title",vector,"movie_report","poster")))
        table <- merge(table, x, by.y = "title", by.x= "title")
        table <- data.table(table) %>% select(c(2,1,3,6,5)) %>% arrange(Rank)%>%select(c(2,3,4,5))
        setnames(table, paste0(vector,".x"), vector)
      }
      
      # poster.pic.name <- function(vector){
      #   foreach(i =1:length(vector),.combine = c)%do%{
      #     name <- paste0(gsub(" ","", vector[i]),".png")
      #     name <- gsub(":","", name)
      #     return(name)
      #     
      #   } -> result ; rm(i)
      #   
      #   return(result)
      # }
      
      table$poster <- poster.pic.name(table$title)
      
      return(table)
    }
    
  }
}



report.text <- function(List){
  
  df <- List$processed.data
  
  df2.1 <- data.frame(List$raw.data$BO)
  df2.1 <- merge(df,df2.1, by = "title")
  df2.1 <- data.table(df2.1)%>% arrange(desc(combined_score))%>%select(which(names(df2.1) %in% c("runtime.y","rating.y")))
 
  df2.2 <- data.frame(List$raw.data$critics)
  df2.2 <- merge(df,df2.2, by = "title")
  df2.2 <- data.table(df2.2)%>% arrange(desc(combined_score))%>% select(which(names(df2.2) %in% c("RT_consensus","RT_perc","RT_rating","RT_audience_perc","RT_audience_rating")))
    # select(c(42,39))

  
  
  foreach(i = 1:dim(df)[1],.combine = c ) %do%
    {
      paste(
            paste0("<a>",toupper(df$title[i]),"</a>"),'<br/>',
            "<a>Grade: </a>", toupper(df$class[i]),'<br/>',
            "<a>All-time Ranking: </a>", df$overall_rank[i],'<br/>',
            "<a>Plot: </a>",df$plot[i], '<br/>',
            "<a>Release Date: </a> ", df$release_date[i],'<br/>',
            "<a>Director: </a> ", df$director[i],'<br/>',
            "<a>Runtime: </a> ", df2.1$runtime.y[i],"minutes",'<br/>',
            "<a>Rating: </a> ", df2.1$rating.y[i],'<br/>',
            "<a>Budget: </a> $",formatC(as.numeric(df$budget[i]), format="f", digits=2, big.mark=","),'<br/>',
            "<a>Opening Domestic BO: </a> $",formatC(as.numeric(df$ow_gross[i]), format="f", digits=2, big.mark=","),'<br/>',
            "<a>Domestic BO: </a> $",  formatC(as.numeric(df$domestic_BO[i]), format="f", digits=2, big.mark=","),'<br/>',
            "<a>Foreign BO: </a> $",formatC(as.numeric(df$foreign_BO[i]), format="f", digits=2, big.mark=","),'<br/>',
            "<a>World Wide BO: </a> $",formatC(as.numeric(df$domestic_BO[i] + df$foreign_BO[i]), format="f", digits=2, big.mark=","),'<br/>',
            "<a>Rotten Tomatoes Critics Tomatometer: </a>", df2.2$RT_perc[i],"%",'<br/>',
            "<a>Rotten Tomatoes Critics Rating: </a>", paste0(df2.2$RT_rating[i],"/10"),'<br/>',
            "<a>Rotten Tomatoes Audience Tomatometer: </a>", df2.2$RT_audience_perc[i],"%",'<br/>',
            "<a>Rotten Tomatoes Audience Rating: </a>", paste0(df2.2$RT_audience_rating[i],"/5"),'<br/>',
            "<a>Rotten Tomatoes Consensus: </a>",df2.2$RT_consensus[i], '<br/>',
            "<a>Metascore: </a>", df$metascore[i],"%",'<br/>',
            "<a>IMDB: </a>", df$imdb_rating[i],"%",'<br/>',
            "<a>Studio: </a>", df$studio[i]
            # '<br/>','<br/>','<br/>','<br/>','<br/>','<br/>'
      )

    }-> results; rm(i)

  df$movie_report <- results
  List$processed.data <- df

  
  # return(list(df2.1,df2.2))
  return(List)
}





filter.by <- function(filters,df,List){
  
 
  
  # IP, Studio(s), Year(s), Month(s), Runtime(s), Rating(s), 
  # imdb_rating, metascore,
  
  df$release_month[which(df$release_month == "Jan")] <- 1
  df$release_month[which(df$release_month == "Feb")] <- 2
  df$release_month[which(df$release_month == "Mar")] <- 3
  df$release_month[which(df$release_month == "Apr")] <- 4
  df$release_month[which(df$release_month == "May")] <- 5
  df$release_month[which(df$release_month == "Jun")] <- 6
  df$release_month[which(df$release_month == "Jul")] <- 7
  df$release_month[which(df$release_month == "Aug")] <- 8
  df$release_month[which(df$release_month == "Sep")] <- 9
  df$release_month[which(df$release_month == "Oct")] <- 10
  df$release_month[which(df$release_month == "Nov")] <- 11
  df$release_month[which(df$release_month == "Dec")] <- 12
  
  # IP filtering
  ip <- filters[[1]]
  if ("All" %in% ip| length(ip) == 0){
    data <- df
  }else{
    data <- df%>%filter(IP %in% ip)}
  
  # Studios filtering
  Studios <- filters[[2]]
  if ("All" %in% Studios | length(Studios) == 0){
    data <- data
  }else{
    data <- data%>%filter(studio %in% Studios)
  }

  # Years filtering
  Years <- filters[[3]]
  data <- data%>%filter(release_year >= Years[1] & release_year <= Years[2])

  # Months filtering
  Months <- filters[[4]]
  if ("All" %in% Months | length(Months) == 0){
    data <- data
  }else{data <- data%>%filter(release_month %in% Months)}
    
  # Runtime filtering
  Runtimes <- filters[[5]]
  if("All" %in% Runtimes | length(Runtimes) == 0){
    data <- data
  }else{data <- data%>%filter(runtime %in% Runtimes)}

  # Ratings filtering
  Ratings <- filters[[6]]
  if ("All" %in% Ratings| length(Ratings) == 0){
    data <- data
  }else{data <- data%>%filter(rating %in% Ratings)}

  #Metascore filtering
  Metascores <- filters[[7]]
  data <- data%>%filter(metascore >= Metascores[1] & metascore <= Metascores[2])

  #IMDB filtering
  Imdbs <- filters[[8]]
  Imdbs <- Imdbs*10
  data <- data%>%filter(imdb_rating >= Imdbs[1] & imdb_rating <= Imdbs[2])

  # RT_perc, RT_rating, RT_audience_perc, RT_audience_rating
  critics.raw <- data.table(rbind(List$Critics$dc$raw, List$Critics$marvel$raw,List$Critics$others$raw))
  critics.raw <- critics.raw%>%filter(title %in% data$title)

  #Rotten tomatoes critics percent filtering
  RT_percs <- filters[[9]]
  critics.raw <- critics.raw%>%filter(RT_perc >= RT_percs[1] & RT_perc <= RT_percs[2])

  #Rotten tomatoes critics rating filtering
  RT_ratings <- filters[[10]]
  critics.raw <- critics.raw%>%filter(RT_rating >= RT_ratings[1] & RT_rating <= RT_ratings[2])

  #Rotten tomatoes critics percent filtering
  RT_audience_percs <- filters[[11]]
  critics.raw <- critics.raw%>%filter(RT_audience_perc >= RT_audience_percs[1] & RT_audience_perc <= RT_audience_percs[2])

  #Rotten tomatoes audience rating filtering
  RT_audience_ratings <- filters[[12]]
  critics.raw <- critics.raw%>%filter(RT_audience_rating >= RT_audience_ratings[1] & RT_audience_rating <= RT_audience_ratings[2])

  ### finally
  bo.raw <- data.frame(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
  bo.raw <- bo.raw %>% filter(title %in% critics.raw$title)

  data <- data.table(data) %>% filter(title %in% critics.raw$title)
  
  raw.list <- list("critics"=critics.raw, "BO" = bo.raw)
  
  if (dim(data)[1] == 0){return("No movie fits the criteria given")}
  # return(list("processed.data" = data,"raw.data" = raw.list ))
  return(report.text(list("processed.data" = data,"raw.data" = raw.list )))
}




versus_meta <- function(name, df, List, type){
  
  data <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw, List$BO$others$raw)) %>% filter(title %in% name)
  data <- data %>% select(which(names(data) %in% c("title","director","genre","budget","rating","runtime","distributor")))
  dt <- data.table(df)  %>% select(which(names(df) %in% c("title","release_date","domestic_BO","foreign_BO"))) %>% filter(title %in% name)
  
  
  
  fix.director.names <- function(vector){
    x <- na.omit(vector)
    
    foreach(i = 1:length(x), .combine = c) %do% {
      strsplit(x[i],"'")[[1]][2]
    } -> fixed.names
    return(fixed.names)
  }
  
  if (is.na(data$director)){
    data$director <- "N/A"
  }else{
    data$director <- fix.director.names(data$director)
  }
  
  data <- merge(data,dt, by = "title")
  
  if (type %in% "BOP"){
    
    return(
      paste(
        '<br/>','<br/>','<br/>',
        "<a>Release Date: </a> ", data$release_date,'<br/>',
        "<a>Director: </a> ", data$director,'<br/>',
        "<a>Budget: </a> ", paste0("$",formatC(as.numeric(data$budget), format="f", digits=2, big.mark=","),'<br/>'),
        "<a>Rating: </a> ", data$rating,'<br/>',
        "<a>Runtime: </a> ", data$runtime," minutes",'<br/>',
        "<a>Studio: </a> ", data$distributor,'<br/>','<br/>','<br/>'
      )
    )
    
  }else{
    
    return(
      paste(
        '<br/>','<br/>','<br/>',
        "<a>Release Date: </a> ", data$release_date,'<br/>',
        "<a>Director: </a> ", data$director,'<br/>',
        "<a>Budget: </a> ", paste0("$",formatC(as.numeric(data$budget), format="f", digits=2, big.mark=","),'<br/>'),
        "<a>Domestic BO: </a> ", paste0("$",formatC(as.numeric(data$domestic_BO), format="f", digits=2, big.mark=","),'<br/>'),
        "<a>World Wide BO: </a> ", paste0("$",formatC(as.numeric(data$domestic_BO + data$foreign_BO), format="f", digits=2, big.mark=","),'<br/>'),
        "<a>Rating: </a> ", data$rating,'<br/>',
        "<a>Runtime: </a> ", data$runtime," minutes",'<br/>',
        "<a>Studio: </a> ", data$distributor,'<br/>','<br/>','<br/>'
      )
    )
    
  }
}





versus.weekly.avg <- function(titles, df, List){
  
  processed <- data.table(df) %>% filter(title %in% titles)
  processed <- processed %>% select(which(names(processed) %in% c("title","combined_score")))
  
  if(dim(processed)[1] == 0){
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw, List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("avg")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
    
    first <- plot_ly( x = c(1:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "scatter",
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(1:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "scatter",
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    
    chart <- layout(second,
                    title = "",
                    showlegend = F,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    
    return(chart)
    
  }else{
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw, List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("avg")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
  
    first <- plot_ly( x = c(1:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "markers"
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(1:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "markers"
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    
    chart <- layout(second,
                    title = "Weekly Average per Theater (hover on point for info)",
                    legend = list(font = list(size = 12, color = "white"),
                                  x = 0.2, 
                                  y = 0.8),
                    titlefont = list(size = 12, color = "#0ce3ac"),
                    # autosize = F,
                    # width = 800,
                    # height = 500,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Value (in $)",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    ),
                    xaxis = list(title = "Week #",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    )
    )
    
    return(chart)
    
  }
}






versus.weekly.perc <- function(titles, df, List){
  
  processed <- data.table(df) %>% filter(title %in% titles)
  processed <- processed %>% select(which(names(processed) %in% c("title","combined_score")))
  
  if(dim(processed)[1] == 0){
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw, List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("change")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
    
    
    first <- plot_ly( x = c(2:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "markers"
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(2:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "markers"
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    chart <- layout(second,
                    title = "",
                    showlegend = F,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    
    return(chart)
    
  }else{
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw, List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("change")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
    
    
    first <- plot_ly( x = c(2:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "markers"
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(2:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "markers"
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    chart <- layout(second,
                    title = "Weekly gross as percent of Opening Week BO (hover on point for info)",
                    # legend = list(font = list(size = 10, color = "white"),
                    #               x = 0.25, 
                    #               y = 0.75),
                    showlegend = F,
                    titlefont = list(size = 12, color = "#0ce3ac"),
                    # autosize = F,
                    # width = 800,
                    # height = 500,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Value (in %)",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    ),
                    xaxis = list(title = "Week #",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    )
    )
    
    return(chart)
    
  }
}






versus.weekly.rank <- function(titles, df, List){
  
  processed <- data.table(df) %>% filter(title %in% titles)
  processed <- processed %>% select(which(names(processed) %in% c("title","combined_score")))
  
  if( dim(processed)[1] == 0 ){
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("rank")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
    
    first <- plot_ly( x = c(1:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "markers"
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(1:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "markers"
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    chart <- layout(second,
                    title = "",
                    showlegend = F,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 autorange = "reversed",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    
    return(chart)
    
  }else{
    
    bo.raw <- data.table(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles) 
    
    table <- data.table(merge(processed, bo.raw, by = "title")) %>%
      select(contains("title"),
             contains("combined_score"),
             ends_with("rank")) %>% 
      arrange(desc(combined_score))
    
    table <- data.table(table)
    
    
    first <- plot_ly( x = c(1:15),
                      y = transpose(table[1] %>% select(3:dim(table)[2]))$V1,
                      # type = "markers"
                      # orientation = 'h',
                      marker = list(color = "#20B2AA"),
                      # colors = c("#66cccc"),
                      name = table$title[1]
    )
    
    second <- add_trace(first,
                        x = c(1:15),
                        y = transpose(table[2] %>% select(3:dim(table)[2]))$V1,
                        # type = "markers"
                        # orientation = 'h',
                        marker = list(color = "red"),
                        # colors = c("#66cccc"),
                        name = table$title[2]
    )
    
    
    
    chart <- layout(second,
                    title = "Weekly Rank (hover on point for info)",
                    showlegend = F,
                    # legend = list(font = list(size = 10, color = "white"),
                    #               x = 0.1, 
                    #               y = 0.1),
                    titlefont = list(size = 12, color = "#0ce3ac"),
                    # autosize = F,
                    # width = 800,
                    # height = 500,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Ranking #",
                                 autorange = "reversed",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    ),
                    xaxis = list(title = "Week #",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    )
    )
    
    return(chart)
    
  }
  
  
}






versus.BO.chart.1 <- function(titles, df){
  
  
  if((titles[1] %in% "" & titles[2] %in% "") | (length(titles[1]) == 0 & length(titles[2]) == 0)){
    plot.table <- data.table(df) %>% filter(title %in% titles)%>%
      select( which(names(df) %in% c("title",
                                     "foreign_BO",
                                     "domestic_BO",
                                     "ow_gross",
                                     # "combined_BO",
                                     "combined_score"
                                     )
                    )
              ) 
    
    plot.table$world_wide <- plot.table$domestic_BO + plot.table$foreign_BO
    plot.table <- plot.table %>% select(c(4,3,2,1,6,5))%>% arrange(desc(combined_score))
    
    
    
    chart_step.1 <- plot_ly( 
      x = c("Opening Week",
            "Domestic",
            "Foreign",
            "World Wide"
            # "D&F Weighted Avg"
      ),
      y = transpose(data.table(plot.table)[1] %>% select(2:5))$V1,
      type = "bar",
      marker = list(color = "#20B2AA"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c("Opening Week",
                                    "Domestic",
                                    "Foreign",
                                    "World Wide"
                                    # "D&F Weighted Avg"
                              ),
                              y = transpose(data.table(plot.table)[2] %>% select(2:5))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              name = plot.table$title[2]
    )
    
    
    chart <- layout(chart_step.2,
                    title = "",
                    showlegend = F,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    
    
    # return(plot.table)
    return(chart)
    
  }else{
    
    plot.table <- data.table(df) %>% filter(title %in% titles)%>%
      select( which(names(df) %in% c("title",
                                     "foreign_BO",
                                     "domestic_BO",
                                     "ow_gross",
                                     # "combined_BO",
                                     "combined_score"
                                     )
                    )
              ) 
    
    plot.table$world_wide <- plot.table$domestic_BO + plot.table$foreign_BO
    plot.table <- plot.table %>% select(c(4,3,2,1,6,5))%>% arrange(desc(combined_score))
    
    
    
    chart_step.1 <- plot_ly( 
      x = c("Opening Week",
            "Domestic",
            "Foreign",
            "World Wide"
            # "D&F Weighted Avg"
      ),
      y = transpose(data.table(plot.table)[1] %>% select(2:5))$V1,
      type = "bar",
      marker = list(color = "#20B2AA"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c("Opening Week",
                                    "Domestic",
                                    "Foreign",
                                    "World Wide"
                                    # "D&F Weighted Avg"
                              ),
                              y = transpose(data.table(plot.table)[2] %>% select(2:5))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              name = plot.table$title[2]
    )
    
    
    chart <- layout(chart_step.2,
                    title = "",
                    showlegend = F,
                    # legend = list(font = list(size = 10, color = "white"),
                    #               x = 0.05, 
                    #               y = 1
                    # ),
                    titlefont = list(size = 12, color = "#0ce3ac"),
                    # autosize = F,
                    # width = 800,
                    # height = 500,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Value (in $)",
                                 # autorange = "reversed",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    ),
                    xaxis = list(title = "Gross Sales (hover on bars for info)",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    )
    )
    
    
    # return(plot.table)
    return(chart)
    
  }
  
}





versus.BO.chart.2 <- function(titles, df){
  
  
  if((titles[1] %in% "" & titles[2] %in% "") | (length(titles[1]) == 0 & length(titles[2]) == 0)){
    plot.table <- data.table(df) %>% filter(title %in% titles) %>%
      select( which(names(df) %in% c("title",
                                     "avg",
                                     "change",
                                     "rank",
                                     "word_to_mouth",
                                     "ratio.ow_budget",
                                     # "ratio.www_budget",
                                     "ratio.domestic_budget",
                                     "ratio.foreign_budget",
                                     "ratio.worldwide_budget",
                                     "investment_index",
                                     "bo_score",
                                     "combined_score"
                                     )
                    )
              ) 
    
    plot.table <- plot.table%>% select(c(11,6,7,8,9,10,1,2,3,4,5,12)) %>% arrange(desc(combined_score))
   
    
    chart_step.1 <- plot_ly( 
      x = c(
        
        "Word to Mouth Index",
        "Opening Week/Budget",
        "Domestic/Budget",
        "Foreign/Budget",
        "World Wide/Budget",
        "Weekly Avg. Gross Index",
        "Weekly Drops Index",
        "Weekly Rank Index",
        "Investment Index",
        "Overall BO Index"
      ),
      y = c(1,0.1,0.1,0.1,0.1,1,10,0.1,0.01,0.1)*transpose(data.table(plot.table)[1] %>% select(2:11))$V1,
      type = "bar",
      marker = list(color = "#20B2AA"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c(
                                
                                "Word to Mouth Index",
                                "Opening Week/Budget",
                                "Domestic/Budget",
                                "Foreign/Budget",
                                "World Wide/Budget",
                                "Weekly Avg. Gross Index",
                                "Weekly Drops Index",
                                "Weekly Rank Index",
                                "Investment Index",
                                "Overall BO Index"
                              ),
                              y = c(1,0.1,0.1,0.1,0.1,1,10,0.1,0.01,0.1)*transpose(data.table(plot.table)[2] %>% select(2:11))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              name = plot.table$title[2]
    )
    
    
    chart <- layout(chart_step.2,
                    title = "",
                    showlegend = F,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    return(chart)
    
  }else{
    
    plot.table <- data.table(df) %>% filter(title %in% titles) %>%
      select( which(names(df) %in% c("title",
                                     "avg",
                                     "change",
                                     "rank",
                                     "word_to_mouth",
                                     "ratio.ow_budget",
                                     # "ratio.www_budget",
                                     "ratio.domestic_budget",
                                     "ratio.foreign_budget",
                                     "ratio.worldwide_budget",
                                     "investment_index",
                                     "bo_score",
                                     "combined_score"
                                     )
                    )
              ) 
    
    
    plot.table <- plot.table%>% select(c(11,6,7,8,9,10,1,2,3,4,5,12)) %>% arrange(desc(combined_score))
    # return(plot.table)
    
    chart_step.1 <- plot_ly( 
      x = c(
        
        "Word to Mouth Index",
        "Opening Week/Budget",
        "Domestic/Budget",
        "Foreign/Budget",
        "World Wide/Budget",
        "Weekly Avg. Gross Index",
        "Weekly Drops Index",
        "Weekly Rank Index",
        "Investment Index",
        "Overall BO Index"
      ),
      y = c(1,0.1,0.1,0.1,0.1,1,10,0.1,0.01,0.1)*transpose(data.table(plot.table)[1] %>% select(2:11))$V1,
      type = "bar",
      marker = list(color = "#20B2AA"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c(
                                
                                "Word to Mouth Index",
                                "Opening Week/Budget",
                                "Domestic/Budget",
                                "Foreign/Budget",
                                "World Wide/Budget",
                                "Weekly Avg. Gross Index",
                                "Weekly Drops Index",
                                "Weekly Rank Index",
                                "Investment Index",
                                "Overall BO Index"
                              ),
                              y = c(1,0.1,0.1,0.1,0.1,1,10,0.1,0.01,0.1)*transpose(data.table(plot.table)[2] %>% select(2:11))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              name = plot.table$title[2]
    )
    
    
    chart <- layout(chart_step.2,
                    title = "",
                    showlegend = F,
                    # legend = list(font = list(size = 10, color = "white"),
                    #               x = 0.05, 
                    #               y = 1
                    #               ),
                    titlefont = list(size = 12, color = "#0ce3ac"),
                    # autosize = F,
                    # width = 800,
                    # height = 500,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Scaled Value (higher is better)",
                                 # autorange = "reversed",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    ),
                    xaxis = list(title = "BO Metrics (hover on bars for info)",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F,
                                 tickfont = list(size = 12, color = "white"),
                                 titlefont= list(size = 12, color = "white")
                    )
    )
    
    
    # return(plot.table)
    return(chart)
    
  }

}







versus_critics <- function(titles, df, List){
  
  processed <- data.table(df) %>% filter(title %in% titles)
  
  if(dim(processed)[1] == 0){
    critics.raw <- data.table(rbind(List$Critics$dc$raw, List$Critics$marvel$raw,List$Critics$others$raw))
    critics.raw <- critics.raw %>% filter(title %in% titles) %>% select(c(3,4,5,7,12,13,9,10,11))
    
    bo.raw <- data.frame(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles)
    
    raw <- data.table(bo.raw,critics.raw)
    
    table.1 <- raw %>% select(which(names(raw) %in% c("title","RT_rating","RT_audience_rating",
                                                      "RT_perc","RT_audience_perc","RT_consensus")))
    table.2 <- processed %>% select(which(names(processed) %in% c("title",
                                                                  "imdb_rating",
                                                                  "metascore",
                                                                  "RT_score",
                                                                  "RT_audience_score",
                                                                  "critics_score")))
    
    plot.table <- data.table(merge(table.1,table.2, by = "title"))
    # plot.table$imdb_rating <- 10*plot.table$imdb_rating 
    plot.table$RT_rating <-  100*(plot.table$RT_rating/10)
    plot.table$RT_audience_rating <-  100*(plot.table$RT_audience_rating/5)
    
    plot.table <- plot.table %>% select(c(1:5,7:11,6))%>% arrange(desc(critics_score))
    plot.table <- data.table(plot.table)
    
    chart_step.1 <- plot_ly(
      x = c( 
        "Rotten Tomatoes Critics Tometometer",
        "Rotten Tomates Critics Rating",
        "Rotten Tomatoes Audience Tometometer",
        "Rotten Tomatoes Audience Rating",
        "IMDB",
        "Metascore",
        "Custom Rotten Tomatoes Critics Score",
        "Custom Rotten Tomatoes Audience Score",
        "Overall Estimated Critical Reception"
      ), 
      y = transpose(plot.table[1] %>% select(2:10))$V1,
      type = "bar",
      # orientation = 'h',
      marker = list(color = "#20B2AA"),
      # colors = c("#66cccc"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c( 
                                "Rotten Tomatoes Critics Tometometer",
                                "Rotten Tomates Critics Rating",
                                "Rotten Tomatoes Audience Tometometer",
                                "Rotten Tomatoes Audience Rating",
                                "IMDB",
                                "Metascore",
                                "Custom Rotten Tomatoes Critics Score",
                                "Custom Rotten Tomatoes Audience Score",
                                "Overall Estimated Critical Reception"
                              ), 
                              y = transpose(plot.table[2] %>% select(2:10))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              # orientation = 'h',
                              name = plot.table$title[2])
    
    
    
    
    
    
    
    chart <- layout(chart_step.2,
                    title = "", 
                    showlegend = F,
                    titlefont = list( color = "#0ce3ac"),
                    autosize = F, 
                    width = 1200, 
                    height = 600,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    ),
                    xaxis = list(title = "",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F
                    )
    )
    
    
    # rt_consensus <- plot.table$RT_consensus
    # rt_consensus <- data.table(merge(rt_consensus, plot.table, by = "title")) %>% select(c(1,2))
    
    foreach( i =1:2) %do% {
      paste(
        "</br>","</br>","</br>",
        paste0("<a><h4>"," Rotten Tomatoes Consensus: ","</h4></a>"),
        "</br>",
        paste0("<p>",plot.table$RT_consensus[i],"</p>"), 
        "</br>","</br>","</br>"
      )
    } -> consensus ; rm(i)
    names(consensus) <- plot.table$title
    return(list("chart" = chart, "consensus" = consensus))
    
  }else{
    
    critics.raw <- data.table(rbind(List$Critics$dc$raw, List$Critics$marvel$raw,List$Critics$others$raw))
    critics.raw <- critics.raw %>% filter(title %in% titles) %>% select(c(3,4,5,7,12,13,9,10,11))
    
    bo.raw <- data.frame(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% titles)
    
    raw <- data.table(bo.raw,critics.raw)
    
    table.1 <- raw %>% select(which(names(raw) %in% c("title","RT_rating","RT_audience_rating",
                                                      "RT_perc","RT_audience_perc","RT_consensus")))
    table.2 <- processed %>% select(which(names(processed) %in% c("title",
                                                                  "imdb_rating",
                                                                  "metascore",
                                                                  "RT_score",
                                                                  "RT_audience_score",
                                                                  "critics_score")))
    
    plot.table <- data.table(merge(table.1,table.2, by = "title"))
    # plot.table$imdb_rating <- 10*plot.table$imdb_rating 
    plot.table$RT_rating <-  100*(plot.table$RT_rating/10)
    plot.table$RT_audience_rating <-  100*(plot.table$RT_audience_rating/5)
    
    plot.table <- plot.table %>% select(c(1:5,7:11,6))%>% arrange(desc(critics_score))
    plot.table <- data.table(plot.table)
    
    chart_step.1 <- plot_ly(
      x = c( 
        "Rotten Tomatoes Critics Tometometer",
        "Rotten Tomates Critics Rating",
        "Rotten Tomatoes Audience Tometometer",
        "Rotten Tomatoes Audience Rating",
        "IMDB",
        "Metascore",
        "Custom Rotten Tomatoes Critics Score",
        "Custom Rotten Tomatoes Audience Score",
        "Overall Estimated Critical Reception"
      ), 
      y = transpose(plot.table[1] %>% select(2:10))$V1,
      type = "bar",
      # orientation = 'h',
      marker = list(color = "#20B2AA"),
      # colors = c("#66cccc"),
      name = plot.table$title[1]
    )
    
    
    chart_step.2 <- add_trace(chart_step.1,
                              x = c( 
                                "Rotten Tomatoes Critics Tometometer",
                                "Rotten Tomates Critics Rating",
                                "Rotten Tomatoes Audience Tometometer",
                                "Rotten Tomatoes Audience Rating",
                                "IMDB",
                                "Metascore",
                                "Custom Rotten Tomatoes Critics Score",
                                "Custom Rotten Tomatoes Audience Score",
                                "Overall Estimated Critical Reception"
                              ), 
                              y = transpose(plot.table[2] %>% select(2:10))$V1,
                              type = "bar",
                              marker = list(color = "red"),
                              # orientation = 'h',
                              name = plot.table$title[2])
    
    
    
    
    
    
    
    chart <- layout(chart_step.2,
                    title = "Critical Reception Comparison (hover on bars for info)",
                    legend = list(font = list(size = 15, color = "white")),
                    titlefont = list( color = "#0ce3ac"),
                    autosize = F, 
                    width = 1200, 
                    height = 600,
                    paper_bgcolor='rgba(0,0,0,0)',
                    plot_bgcolor='rgba(0,0,0,0)',
                    yaxis = list(title = "Value (in %)",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = T,
                                 tickfont = list(size = 15, color = "white"),
                                 titlefont= list(size = 15, color = "white")
                    ),
                    xaxis = list(title = "Critical Reception Metrics",
                                 showgrid = F,
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = F,
                                 tickfont = list(size = 15, color = "white"),
                                 titlefont= list(size = 15, color = "white")
                    )
    )
    
    
    # rt_consensus <- plot.table$RT_consensus
    # rt_consensus <- data.table(merge(rt_consensus, plot.table, by = "title")) %>% select(c(1,2))
    
    foreach( i =1:2) %do% {
      paste(
        "</br>","</br>","</br>",
        paste0("<a><h4>"," Rotten Tomatoes Consensus: ","</h4></a>"),
        "</br>",
        paste0("<p>",plot.table$RT_consensus[i],"</p>"), 
        "</br>","</br>","</br>"
      )
    } -> consensus ; rm(i)
    names(consensus) <- plot.table$title
    return(list("chart" = chart, "consensus" = consensus))
    
  }
  
}





# find_movie <- function(titles, df,List){
#   
#   data <- df %>% filter(title %in% titles)
#   if (dim(data)[1] == 0){
#     return(data.table("Empty" = "yes",
#                       "movie_report"= "N/A", 
#                       "poster" = "NoPosterAvailable.png"
#                       )
#            )
#   }else{
#     
#     critics.raw <- data.table(rbind(List$Critics$dc$raw, List$Critics$marvel$raw,List$Critics$others$raw))
#     critics.raw <- critics.raw%>%filter(title %in% data$title)
#     
#     bo.raw <- data.frame(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
#     bo.raw <- bo.raw %>% filter(title %in% critics.raw$title)
#     
#     raw.list <- list("critics"=critics.raw, "BO" = bo.raw)
#     
#     report <- report.text(list("processed.data" = data,"raw.data" = raw.list ))
#     report <- report$processed.data 
#     report <- report %>% select(which(names(report) %in% c("title","movie_report")))
#     
#     posters <- foreach(i =1 :dim(report)[1], .combine = c) %do% {
#       poster.pic.name(report$title[i])
#     }; rm(i)
#     
#     
#     return(data.table(report, "posters" = posters))
#     
#   }
#   
# }