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
    
    
    HTML(render.compare.poster.template(data, "red"))
  })
  
 
  output$second.movie.poster <- renderUI({
    if(is.null(input$second.movie)) return(NULL)
    if(is.null(all.time.ranking.data)) return(NULL)
    
    data <- all.time.ranking.data[title == input$second.movie]
    
    if(is.null(data)) return(NULL)
    if(dim(data)[1] == 0) return(NULL)
  
    
    HTML(render.compare.poster.template(data, "blue"))
  })
  
  
  output$budget.revenues <- renderPlot({
    if(is.null(input$first.movie)) return(NULL)
    if(is.null(input$second.movie)) return(NULL)
    
    database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
    data <- data.table(dbGetQuery(database, 'SELECT title, budget, domestic_BO, foreign_BO FROM summaries'))
    data <- setnames(data[title %in% c(input$first.movie, input$second.movie)], c("budget","domestic_BO","foreign_BO"), c("Budget","Domestic","Foreign"))
    data[, c("Budget","Domestic","Foreign") := list(as.numeric(Budget), as.numeric(Domestic), as.numeric(Foreign))]
    data <- melt(data, id.vars = "title", variable.name ="bo", value.name = "value")
    data[, title := factor(title, levels = c(input$first.movie, input$second.movie), ordered = T)]
    
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(bo, value, fill = title)) + 
      geom_bar(position="dodge", width = 0.5, stat="identity") + 
      scale_y_continuous(labels = scales::dollar) +
      scale_x_discrete() +
      ggtitle("Budget & Revenues")+
      scale_fill_manual(values=c("red", "blue"))+
      geom_text(
        aes(label=paste0("$", format(round(value/10^6,2), big.mark = ","), "M")),
        vjust= -0.8,
        color="black", position=position_dodge(.6), size = 3,fontface = "bold") + 
      theme_classic() + 
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
    data <- setkey(data[, imdbID := NULL ], title)[title %in% c(input$first.movie, input$second.movie)]
    data <- melt(data, id.vars = "title", variable.name ="source", value.name = "value")
    data[, title := factor(title, levels = c(input$first.movie, input$second.movie), ordered = T)]
    
    dbDisconnect(database); rm(list = c("database"))
    
    data%>%
      ggplot(aes(source, value, fill = title)) +
      geom_bar(position="dodge", width = 0.5, stat="identity") +
      scale_y_continuous(labels = scales::dollar) +
      scale_x_discrete() +
      ggtitle("Critical Performance")+
      scale_fill_manual(values=c("red", "blue"))+
      geom_text(
        aes(label=paste0(value,"%")),
        vjust= -0.8,
        color="black", position=position_dodge(.6), size = 3,fontface = "bold") +
      theme_classic() +
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
  
  
})



  


