shinyServer(function(input, output) {
  
  get.filtered.data <- reactive({

    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    summary <- data.table(dbGetQuery(database, 'SELECT * FROM summaries'))[,
                                                                           c("distributor","month") := list(
                                                                             set.distributor.names(distributor),
                                                                             get.release.months(Released)
                                                                           )
                                                                           ]
    
    bo <- data.table((dbGetQuery(database, 'SELECT *  FROM boMetrics')))
    critics <- data.table(dbGetQuery(database, 'SELECT imdbID, Metascore, "Rotten Tomatoes", imdbRating FROM critics'))
    setnames(critics, names(critics),  c("imdbID","Metascore","rt","imdbRating"))
    scores <- data.table(dbGetQuery(database, 'SELECT imdbID, critical_score, overall_score from scoring'))

    setkey(summary, imdbID)
    setkey(bo, imdbID)
    setkey(critics, imdbID)
    setkey(scores, imdbID)
    
    data <- summary[bo, nomatch = 0][critics, nomatch = 0][scores, nomatch = 0]
    data <- data[!is.infinite(overall_score) | !is.infinite(bo_score)]
    data[, critical_score := set.critical.reception.range(critical_score)
         ][, grade := as.character(overall.scores.to.grades(overall_score))]
    
    rm(list=c("bo","critics","summary","scores"))
    dbDisconnect(database); rm(database)
    
    data <- data %>%
      filter.by.ip(input$ip) %>%
      filter.by.studio(input$studio) %>%
      filter.by.year(input$years) %>%
      filter.by.month(input$month) %>%
      filter.by.mpaa(input$mpaa) %>%
      filter.by.runtime(input$runtime) %>%
      filter.by.metascore(input$metascore) %>%
      filter.by.rt(input$rt) %>%
      filter.by.imdb(input$imdb)


    if (is.null(data) | dim(data)[1] == 0) return(NULL)
    
    metrics <- get.ranking.metrics(input$rankBy)
    if (is.null(metrics)) return(data)
    
    setkeyv(data, metrics)
    
    data[, ranking := c(dim(data)[1]:1)]
    return(data)
    
    

  })


  output$filter.rank <- renderUI({
    if(is.null(get.filtered.data())){
      return(HTML(render.404.card.template()))
    } 

    foreach(k=1:dim(get.filtered.data())[1], .combine = c)%do%{
      if(is.null(input$rankBy)) return(render.filter.rank.template(get.filtered.data()[k], input$rankBy))
      return(render.filter.rank.template(get.filtered.data()[ranking == k], input$rankBy))
    } -> htmlBlob

    return(HTML(paste0("\n",htmlBlob,"\n")))



  })
  
  
  output$all.time.ranking.table <- renderUI({
    if(is.null(all.time.ranking.data)) return(NULL)
    foreach(k=1:dim(all.time.ranking.data)[1], .combine = c)%do%{
      return(render.all.time.ranking.table.template(all.time.ranking.data[ranking == k]))
    } -> htmlBlob
    HTML(paste0("\n",htmlBlob,"\n"))
  })
  
  
  output$first.movie.poster <- renderUI({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(all.time.ranking.data)) return(NULL)
    
    data <- all.time.ranking.data[title == input$first.movie]
    
    if(is.null(data)) return(NULL)
    if(dim(data)[1] == 0) return(NULL)
    
    
    HTML(render.compare.poster.template(data, "#73182C"))
  })
  
 
  output$second.movie.poster <- renderUI({
    if(is.null(input$second.movie)) return(NULL)
    if(is.null(all.time.ranking.data)) return(NULL)
    
    data <- all.time.ranking.data[title == input$second.movie]
    
    if(is.null(data)) return(NULL)
    if(dim(data)[1] == 0) return(NULL)
  
    
    HTML(render.compare.poster.template(data,"#023373"))
  })
  
  
  output$budget.revenues <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- data.table(dbGetQuery(database, 'SELECT title, budget, domestic_BO, foreign_BO FROM summaries'))
    data <- setnames(data, c("budget","domestic_BO","foreign_BO"), c("Budget","Domestic","Foreign"))
    data[, c("Budget","Domestic","Foreign") := list(as.numeric(Budget), as.numeric(Domestic), as.numeric(Foreign))]
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = c("Budget","Domestic","Foreign")][, title:= "Average"][,c(4,1:3)]
    data <- rbindlist(list(data, average))[ title %in% c(input$first.movie, input$second.movie, "Average")]; rm(average)
    data <- melt(data, id.vars = "title", variable.name ="bo", value.name = "value")
    data[, title := factor(title, levels = c(input$first.movie, input$second.movie, "Average"), ordered = T)]
    
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(bo, value, fill = title)) + 
      geom_bar(position="dodge", width = 0.55, stat="identity") + 
      ggtitle("Budget & Revenues (avg accros movies in yellow)")+
      scale_fill_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label=paste0("$", format(round(value/10^6,2), big.mark = ","), "M")),
        vjust= -1,
        color="black", position=position_dodge(.55), size = 3,fontface = "bold") + 
      theme_economist() + 
      theme(
      
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_text(color ="black", face = "bold", size = 10),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  
  output$critical.performance <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    
    data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM critics')), imdbID)
    data <- setnames(data[,c(4,2,3,5,6)], c("imdbRating","score") ,c("IMDB", "Critical Reception"))
    data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data[,IMDB:= 10*IMDB], nomatch = 0]
    data <- setkey(data[, imdbID := NULL ], title)
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = c("Metascore","Rotten Tomatoes","IMDB", "Critical Reception")]
    average <- average[, lapply(.SD, round, 2)][,title:= "Average"][,c(5,1:4)]
    data <- rbindlist(list(data, average))[title %in% c(input$first.movie, input$second.movie, "Average")]; rm(average)
    data <- melt(data, id.vars = "title", variable.name ="source", value.name = "value")
    data[, title := factor(title, levels = c(input$first.movie, input$second.movie, "Average"), ordered = T)]
    
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(source, value, fill = title)) +
      geom_bar(position="dodge", width = 0.55, stat="identity") +
      ggtitle("Critical Performance")+
      scale_fill_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label=paste0(value,"%")),
        vjust= -1,
        color="black", position=position_dodge(.55), size = 3,fontface = "bold") +
      theme_economist() +
      theme(
        
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 10),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
        
      )
  })
  
  get.bo.metrics.plots.data <- reactive({
    
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM boMetrics')), imdbID)
    data <- data[, which(names(data) %notin% c("ID","rating_coef","runtime_coef")), with = FALSE]
    data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0][!is.infinite(bo_score)]
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = names(data)[which(names(data) %notin% c("imdbID","title"))]]
    average <- average[, lapply(.SD, round, 2)][,title:= "Average"][,c(10,1:9)]
    data <- rbindlist(list(data[, imdbID:= NULL], average))[title %in% c(input$first.movie, input$second.movie, "Average")]; rm(average)

    first.plot.data <- data[,.(title, domestic_over_ow, international_BO_score, bo_score)]
    second.plot.data <- data[,.(title,ow_over_budget,domestic_over_budget, foreign_over_budget)]
    third.plot.data <- data[,.(title,weekly_per_theater_gross_avgs_score,
                               weekly_percent_gross_changes_score,
                               weekly_ranks_score)]

    rm(data)
    setnames(first.plot.data,
             names(first.plot.data)[which(names(first.plot.data) %notin% c("title"))],
             new.col.names.for.bo.metrics.data(first.plot.data))

    setnames(third.plot.data,
             names(third.plot.data)[which(names(third.plot.data) %notin% c("title"))],
             new.col.names.for.bo.metrics.data(third.plot.data))

    setnames(second.plot.data,
             names(second.plot.data)[which(names(second.plot.data) %notin% c("title"))],
             new.col.names.for.bo.metrics.data(second.plot.data))

    first.plot.data <- melt(first.plot.data, id.vars = "title", variable.name ="index", value.name = "value")
    first.plot.data[, title := factor(title, levels = c(input$first.movie, input$second.movie, "Average"), ordered = T)]

    second.plot.data <- melt(second.plot.data, id.vars = "title", variable.name ="index", value.name = "value")
    second.plot.data[, title := factor(title, levels = c(input$first.movie, input$second.movie, "Average"), ordered = T)]

    third.plot.data <- melt(third.plot.data, id.vars = "title", variable.name ="index", value.name = "value")
    third.plot.data[, title := factor(title, levels = c(input$first.movie, input$second.movie, "Average"), ordered = T)]

    dbDisconnect(database); rm(list = c("database"))
    
    if(is.null(first.plot.data) | is.null(second.plot.data) | is.null(third.plot.data)) return(NULL)
    
    return(list("first" = first.plot.data, "second" = second.plot.data, "third"= third.plot.data))
  })
  
  output$first.bo.metrics.plot <- renderPlot({
    if(is.null(get.bo.metrics.plots.data())) return(NULL)
    
    get.bo.metrics.plots.data()$first[, value:=round(value,2)] %>%
      ggplot(aes(index, value, fill = title)) +
      geom_bar(position="dodge", width = 0.3, stat="identity") +
      scale_fill_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label=value),
        vjust= -1,
        color="black", position=position_dodge(.3), size = 3,fontface = "bold") +
      theme_classic() +
      theme(
        
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 7),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none"
      )
  })
  
  output$second.bo.metrics.plot <- renderPlot({
    if(is.null(get.bo.metrics.plots.data())) return(NULL)
    
    get.bo.metrics.plots.data()$second[, value:=round(value,2)] %>%
      ggplot(aes(index, value, fill = title)) +
      geom_bar(position="dodge", width = 0.3, stat="identity") +
      scale_fill_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label=value),
        vjust= -1,
        color="black", position=position_dodge(.3), size = 3,fontface = "bold") +
      theme_classic() +
      theme(
        
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 7),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none"
      )
  })
  
  output$third.bo.metrics.plot <- renderPlot({
    if(is.null(get.bo.metrics.plots.data())) return(NULL)
    
    get.bo.metrics.plots.data()$third[, value:=round(value,2)] %>%
      ggplot(aes(index, value, fill = title)) +
      geom_bar(position="dodge", width = 0.3, stat="identity") +
      scale_fill_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label=value),
        vjust= -1,
        color="black", position=position_dodge(.3), size = 3,fontface = "bold") +
      theme_classic() +
      theme(
        
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size =7),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none"
      )
  })
  
  output$weekly.ranking.plot <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_ranks')), imdbID)[, ID := NULL]
    data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0]
    data <- setkey(data[, imdbID := NULL ], title)[, c(1,8:16,2:7)]
    data <- data[, lapply(.SD, as.numeric), .SDcols = grepl("week_", names(data), fixed = T) ][,title:= data$title]
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = grepl("week_", names(data), fixed = T)]
    average <- average[,  lapply(.SD, round), .SDcols = grepl("week_", names(data), fixed = T)][, title:= "Average"]
    data <- rbindlist(list(data, average))[title %in% c(input$first.movie, input$second.movie,"Average")]; rm(average)
    data <- melt(data, id.vars = "title", variable.name = "week", value.name = "rank")
    data <- data[, week:= foreach(k=1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[k], "_")[[1]][2]))}]
    data <- data[, title := factor(title, levels = c(input$first.movie, input$second.movie,"Average"), ordered = T)][!is.na(rank)]
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(week, rank, color = title)) +
      geom_point() + geom_line()+
      ggtitle("Weekly Ranking")+
      scale_y_reverse()+
      scale_x_continuous(limits = c(1,15), breaks = c(1:15))+
      scale_color_manual(values=c("#73182C","#023373","#E89E01"))+
      geom_text(
        aes(label= rank),
        vjust= -1,
        color="black", size = 3,fontface = "bold") +
      theme_economist() +
      theme(
        
        axis.title.x =  element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 8),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$weekly.avgs.plot <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_avgs')), imdbID)[, ID := NULL]
    data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0]
    data <- setkey(data[, imdbID := NULL ], title)[, c(1,8:16,2:7)]
    data <- data[, lapply(.SD, as.numeric), .SDcols = grepl("week_", names(data), fixed = T) ][,title:= data$title]
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = grepl("week_", names(data), fixed = T)]
    average <- average[,  lapply(.SD, round,2), .SDcols = grepl("week_", names(data), fixed = T)][, title:= "Average"]
    data <- rbindlist(list(data, average))[title %in% c(input$first.movie, input$second.movie,"Average")]; rm(average)
    data <- melt(data, id.vars = "title", variable.name = "week", value.name = "avg")
    data <- data[, week:= foreach(k=1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[k], "_")[[1]][2]))}]
    data <- data[, title := factor(title, levels = c(input$first.movie, input$second.movie,"Average"), ordered = T)][!is.na(avg)]
    dbDisconnect(database); rm(list = c("database"))
    
    
    data%>%
      ggplot(aes(week, avg, color = title)) +
      geom_point() + geom_line()+
      ggtitle("Weekly Averages per Theather")+
      scale_y_continuous(limits = c(0,60000), breaks = seq(0, 60000,10000))+
      scale_x_continuous(limits = c(1,15), breaks = c(1:15))+
      scale_color_manual(values=c("#73182C","#023373","#E89E01"))+
      # geom_text(
      #   aes(label= avg),
      #   vjust= -0.8,
      #   color="black", size = 1,fontface = "bold") +
      theme_economist() +
      theme(
        
        axis.title.x =  element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 8),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color ="black", face = "bold", size = 8),
        
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$weekly.percents.plot <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_percents')), imdbID)[, ID := NULL]
    data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0]
    data <- setkey(data[, imdbID := NULL ], title)[, c(1,8:16,2:7)]
    data <- data[, lapply(.SD, as.numeric), .SDcols = grepl("week_", names(data), fixed = T) ][,title:= data$title]
    average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = grepl("week_", names(data), fixed = T)][, title:= "Average"]
    data <- rbindlist(list(data, average))[title %in% c(input$first.movie, input$second.movie,"Average")]; rm(average)
    data <- melt(data, id.vars = "title", variable.name = "week", value.name = "percent")[, percent:= round(100*percent,2)]
    data <- data[, week:= foreach(k=1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[k], "_")[[1]][2]))}]
    data <- data[, title := factor(title, levels = c(input$first.movie, input$second.movie,"Average"), ordered = T)][!is.na(percent)]
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(week,percent, color = title)) +
      geom_point() + geom_line()+
      ggtitle("Weekly Gross as % of Opening Week")+
      scale_x_continuous(limits = c(1,15), breaks = c(1:15))+
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10))+
      scale_color_manual(values=c("#73182C","#023373","#E89E01"))+
      # geom_text(
      #   aes(label= percent),
      #   vjust= -0.8,
      #   color="black", size = 2,fontface = "bold") +
      theme_economist() +
      theme(
        
        axis.title.x =  element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color ="black", face = "bold", size = 8),
        
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.text.y  = element_blank(),
        axis.text.y = element_text(color ="black", face = "bold", size = 8),
        
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  
  
})



  


