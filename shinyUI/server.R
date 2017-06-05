shinyServer(function(input, output) {
  
  #============================== ALL TIME RANKING ===================================================
  # output$Viz <- renderPlotly({
  #   
  #   Viz(processed.data)$viz
  #   }) 
  # 
  # output$combined.ranking.table <- renderTable({
  # 
  #   Viz(processed.data)$table%>%select(2,5,3,4,1)
  #   })
  # 
  #============================== VERSUS ===================================================
  
  
  # output$firstmovie.poster.CR <- renderUI({
  #   
  #   poster.pic.name <- function(title){
  #     name <- paste0(gsub(" ","", title),".png")
  #     name <- gsub(":","", name)
  #     return(name)
  #   }
  #   
  #   
  #   if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = poster.pic.name(input$firstmovie.CR))
  #   }
  # })
  # 
  # output$firstmovie.poster.BOP <- renderUI({
  #   
  #   poster.pic.name <- function(title){
  #     name <- paste0(gsub(" ","", title),".png")
  #     name <- gsub(":","", name)
  #     return(name)
  #   }
  #   
  #   if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = poster.pic.name(input$firstmovie.BOP))
  #   }
  #   
  # })
  # 
  # output$VERSUS.BOP<- renderUI({
  #   
  #   if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0) |
  #      (input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = "versus.png", height =300, width = 300)
  #   }
  #   
  # })
  # 
  # 
  # output$VERSUS.CR<- renderUI({
  #   if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0) |
  #      (input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = "versus.png", height =300, width = 300)
  #   }
  # })
  # 
  # output$secondmovie.poster.BOP <- renderUI({
  #   poster.pic.name <- function(title){
  #     name <- paste0(gsub(" ","", title),".png")
  #     name <- gsub(":","", name)
  #     return(name)
  #   }
  #   
  #   if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = poster.pic.name(input$secondmovie.BOP))
  #   }
  # })
  # 
  # 
  # 
  # output$secondmovie.poster.CR <- renderUI({
  #   
  #   poster.pic.name <- function(title){
  #     name <- paste0(gsub(" ","", title),".png")
  #     name <- gsub(":","", name)
  #     return(name)
  #   }
  #   
  #   if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     tags$img(src = poster.pic.name(input$secondmovie.CR))
  #   }
  # })
  # 
  # 
  # output$firstmovie.meta.BOP <- renderUI({
  #   title <- input$firstmovie.BOP
  #   if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
  #     tags$br()
  #   }else{
  #     HTML(versus_meta(title,processed.data, raw.data,"BOP"))
  #   }
  #   
  #   })
  # 
  # output$firstmovie.meta.CR <- renderUI({
  #   title <- input$firstmovie.CR
  #   if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     HTML(versus_meta(title,processed.data, raw.data,"CR"))
  #   }
  # })
  # 
  # output$secondmovie.meta.BOP <- renderUI({
  #   title <- input$secondmovie.BOP
  #   if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
  #     tags$br()
  #   }else{
  #     HTML(versus_meta(title,processed.data, raw.data,"BOP"))
  #   }
  # })
  # 
  # output$secondmovie.meta.CR <- renderUI({
  #   title <- input$secondmovie.CR
  #   if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     HTML(versus_meta(title,processed.data, raw.data,"CR"))
  #   }
  # })
  # 
  # 
  # 
  # output$firstmovie.critics_consensus <- renderUI({
  #   
  #   if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
  #                                 processed.data,
  #                                 raw.data)
  #     
  #     consensus <- consensus$consensus
  #     
  #     HTML(consensus[[which(names(consensus) == input$firstmovie.CR)]])
  #   }
  #   
  #   
  #   })
  # 
  # 
  # output$secondmovie.critics_consensus <- renderUI({
  #   
  #   if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
  #     tags$br()
  #   }else{
  #     consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
  #                                 processed.data,
  #                                 raw.data)
  #     consensus <- consensus$consensus
  #     HTML(consensus[[which(names(consensus) == input$secondmovie.CR)]])
  #   }
  # })
  # 
  # output$versus_critics_chart <- renderPlotly({
  #   
  #   if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0) &
  #      (input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
  #     
  #     chart <- versus_critics(c("",""),
  #                             processed.data,
  #                             raw.data)
  #     chart$chart
  #   }else{
  #     chart <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
  #                               processed.data,
  #                               raw.data)
  #     chart$chart
  #   }
  #   
  # 
  #   
  # })
  # 
  # 
  # output$versus.weekly.avg <- renderPlotly({
  #   
  #   if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
  #      (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
  #     
  #     versus.weekly.avg(c("",""),processed.data,raw.data)
  #     
  #   }else{
  #     versus.weekly.avg(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
  #   }
  #   
  #   
  #   
  # })
  # 
  # output$versus.weekly.perc <- renderPlotly({
  #   
  #   if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
  #      (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
  #     
  #     versus.weekly.perc(c("",""),processed.data,raw.data)
  #     
  #   }else{
  #     versus.weekly.perc(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
  #   }
  #   
  #   
  #   
  # })
  # 
  # output$versus.weekly.rank <- renderPlotly({
  #   
  #   if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
  #      (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
  #     
  #     versus.weekly.rank(c("",""),processed.data,raw.data)
  #     
  #   }else{
  #     versus.weekly.rank(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
  #   }
  #   
  #   
  #   
  # })
  # 
  # 
  # output$versus_BO_chart.1 <- renderPlotly({
  #   versus.BO.chart.1(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data)
  # })
  # 
  # output$versus_BO_chart.2 <- renderPlotly({
  #   versus.BO.chart.2(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data)
  # })
  # 
  
   #============================== RANKING ===================================================
  # 
  # output$rank.number <- renderUI({
  #   
  #   
  #   ip <- input$ip
  #   studio <- input$studio
  #   years <- input$years
  #   
  #   months <- input$months
  #   
  #   runtime <- input$runtime
  #   
  #   rating <- input$rating
  #   
  #   metascore <- input$metascore
  #   imdb <- input$imdb
  #   rt <- input$rt
  #   
  #   filters <- list(
  #     ip,studio,
  #     years,months,
  #     runtime, rating,
  #     metascore,imdb,rt
  #   )
  #   
  #   filtering <- filter.by(filters)
  #   
  #   
  #   foreach(i=1:dim(filtering$processed.data)[1]) %do% {
  #     paste(
  #       tags$table(style="width:100%;border-spacing:15px;padding:5px",
  #                  tags$tr(
  #                    tags$td(height="700", tags$a(as.character(i)))
  #                  )
  #       ),
  #       tags$br(),
  #       tags$br(),
  #       tags$br()
  #     )
  #   } -> ranking; rm(i)
  #   
  #   HTML(
  #     paste(ranking[1:length(ranking)],sep='<br/>')
  #   )
  #   
  # })
  

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
    
    
    HTML(render.compare.poster.template(data))
  })
  
  output$second.movie.poster <- renderUI({
    if(is.null(input$second.movie)) return(NULL)
    if(is.null(all.time.ranking.data)) return(NULL)
    
    data <- all.time.ranking.data[title == input$second.movie]
    
    if(is.null(data)) return(NULL)
    if(dim(data)[1] == 0) return(NULL)
  
    
    HTML(render.compare.poster.template(data))
  })
  
  
  # output$weekly.ranks.plot <- plotly::renderPlotly({
  #   if(is.null(input$first.movie)) return(NULL)
  #   if(is.null(input$second.movie)) return(NULL)
  #   if(is.null(all.time.ranking.data)) return(NULL)
  #   
  #   database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
  #   ranks <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_ranks')), imdbID)
  #   summary <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)
  #   table <- summary[ranks, nomatch=0][title %in% c(input$first.movie, input$second.movie)][,c(2,10:18,4:9)]
  #   table <- melt(table, id.vars = "title", variable.name ="week", value.name = "rank")
  #   table[, week:= foreach(i= 1:length(table$week), .combine = c) %do%{return(stri_split_fixed(table$week[i], "_")[[1]][2])}]
  #   
  #   
  #   dbDisconnect(database); rm(list = c("database","ranks","summary"))
  #   
  #   
  #   plot_ly(data,
  #           x = ~week,
  #           y = ~rank,
  #           color = ~title,
  #           colors = c(input$first.movie="red",input$second.movie="blue"),
  #           type = 'scatter', mode = 'lines+markers'
  #   ) %>% layout(yaxis = list(autorange = "reversed"))
  # 
  # 
  # })

  
  # output$reports <- renderUI({
  #   if(is.null(get.filtered.data())) return("DATA CAME BACK NULL YO! FIX IT!!!")
  #   
  #   foreach(k=1:dim(get.filtered.data())[1], .combine = c)%do%{
  #     return(render.rank.report(get.filtered.data()[k]))
  #   } -> htmlBlob
  #   
  #   
  #   HTML(htmlBlob)
  #   
  #   
  #   
  # })
  
  # database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
  # summary <- data.table(dbGetQuery(database, 'SELECT * FROM summaries'))[, distributor := set.distributor.names(distributor)]
  # dbDisconnect(database); rm(database)
  
  
  # output$report <- renderUI({
  # 
  #     ip <- input$ip
  #     studio <- input$studio
  #     years <- input$years
  # 
  #     months <- input$months
  # 
  #     runtime <- input$runtime
  # 
  #     rating <- input$rating
  # 
  #     metascore <- input$metascore
  #     imdb <- input$imdb
  #     rt <- input$rt
  # 
  #     filters <- list(
  #       ip,studio,
  #       years,months,
  #       runtime, rating,
  #       metascore,imdb,rt
  #     )
  # 
  #     filtering <- filter.by(filters)
  # 
  #   filtering <- filter.by(filters,processed.data,raw.data)
  # 
  # 
  # 
  #   rank.options1 <- c("Overall Critical Reception",
  #                      "Overall Box Office Performance",
  #                      "Rotten Tomatoes Critics Tomatometer",
  #                      "Rotten Tomatoes Critics Rating",
  #                      "Custom Rotten Tomatoes Critics Score",
  #                      "Rotten Tomatoes Audience Tomatometer",
  #                      "Rotten Tomatoes Audience Rating",
  #                      "Custom Rotten Tomatoes Audience Score",
  #                      "Metascore","IMDB",
  #                      "Foreign Box Office Gross", 
  #                      "Domestic Box Office Gross",
  #                      "Foreign & Domestic BO - Weighted Mean (0.3 & 0.7)",
  #                      "Weekly Average per Theater",
  #                      "Weekly Grosses as % of Opening Week",
  #                      "Weekly Ranking")
  # 
  #   rank.options2 <- c(
  #                       "critics_score",
  #                       "bo_score",
  #                       "RT_perc",
  #                       "RT_rating",
  #                       "RT_score",
  #                       "RT_audience_perc",
  #                       "RT_audience_rating",
  #                       "RT_audience_score",
  #                       "metascore","imdb_rating",
  #                       "foreign_BO","domestic_BO","combined_BO",
  #                       "avg","change","rank"
  #                       )
  #   
  #   input$rank.by -> criterion
  #   
  #   if (criterion %in% "" | length(which(criterion %in% rank.options1 )) == 0){
  #     criterion <- c("")
  #   }else{
  #     
  #     foreach(i = 1:length(rank.options2), .combine = c) %do% {
  #       if (criterion == rank.options1[i]){
  #         return(rank.options2[i])
  #       }
  #       # else{
  #       #   return(criterion)
  #       # }
  #     } -> criterion; rm(i)
  #     
  #   }
  #   
  #   rank <- rank.by(criterion,filtering)
  #   rm(list = c("rank.options1","rank.options2", "filters"))
  #   
  #   if (names(rank)[1] %in% "Empty"){
  #     display <- paste(tags$br(),tags$br(),tags$br())
  #   }else{
  #     
  #     foreach(i=1:dim(rank)[1], .combine = c) %do% {
  #       paste(
  #         tags$table(style="width:100%;border-spacing:15px;padding:5px",
  #                    tags$tr(
  #                      tags$td(height="700",HTML(paste(rank$movie_report[i])))
  #                    )
  #         ),
  #         tags$br(),
  #         tags$br(),
  #         tags$br()
  #       )
  #     } -> reports; rm(i)
  #     
  #     display <-  paste(reports[1:length(reports)],sep='<br/>')
  #     
  #   }
  #   
  #   HTML(display)
  # })
  
  
  
  
  # output$poster <- renderUI({
  #   ip <- input$ip
  #   studio <- input$studio
  #   years <- input$years
  # 
  #   months <- input$months
  #   months[which(months == "Jan")] <- 1
  #   months[which(months == "Feb")] <- 2
  #   months[which(months == "Mar")] <- 3
  #   months[which(months == "Apr")] <- 4
  #   months[which(months == "May")] <- 5
  #   months[which(months == "Jun")] <- 6
  #   months[which(months == "Jul")] <- 7
  #   months[which(months == "Aug")] <- 8
  #   months[which(months == "Sep")] <- 9
  #   months[which(months == "Oct")] <- 10
  #   months[which(months == "Nov")] <- 11
  #   months[which(months == "Dec")] <- 12
  # 
  #   runtime <- input$runtime
  #   runtime[which(runtime == "Under 1h30")] <- 1
  #   runtime[which(runtime == "Between 1h:30 and 2h:00")] <- 2
  #   runtime[which(runtime == "Between 2h:00 and 2h:30")] <- 3
  #   runtime[which(runtime == "Between 2h:30 and 3h:00")] <- 4
  # 
  #   rating <- input$rating
  #   rating[which(rating == "PG")] <- 1
  #   rating[which(rating == "PG-13")] <- 2
  #   rating[which(rating == "R")] <- 3
  # 
  #   metascore <- input$metascore
  #   imdb <- input$imdb
  #   RT_perc <- input$RT_perc
  #   RT_rating <- input$RT_rating
  #   RT_audience_perc <- input$RT_audience_perc
  #   RT_audience_rating <- input$RT_audience_rating
  # 
  #   filters <- list(ip,studio,years,months,runtime, rating,
  #                   metascore,imdb,RT_perc,RT_rating,
  #                   RT_audience_perc,RT_audience_rating)
  # 
  #   filtering <- filter.by(filters,processed.data,raw.data)
  # 
  # 
  # 
  #   rank.options1 <- c("Overall Critical Reception",
  #                      "Overall Box Office Performance",
  #                      "Rotten Tomatoes Critics Tomatometer",
  #                      "Rotten Tomatoes Critics Rating",
  #                      "Custom Rotten Tomatoes Critics Score",
  #                      "Rotten Tomatoes Audience Tomatometer",
  #                      "Rotten Tomatoes Audience Rating",
  #                      "Custom Rotten Tomatoes Audience Score",
  #                      "Metascore","IMDB","Foreign Box Office Gross", 
  #                      "Domestic Box Office Gross",
  #                      "Foreign & Domestic BO - Weighted Mean (0.3 & 0.7)",
  #                      "Weekly Average per Theater",
  #                      "Weekly Grosses as % of Opening Week",
  #                      "Weekly Ranking")
  # 
  #   rank.options2 <- c(
  #     "critics_score",
  #     "bo_score",
  #     "RT_perc",
  #     "RT_rating",
  #     "RT_score",
  #     "RT_audience_perc",
  #     "RT_audience_rating",
  #     "RT_audience_score",
  #     "metascore","imdb_rating",
  #     "foreign_BO","domestic_BO","combined_BO",
  #     "avg","change","rank"
  #   )
  #   
  #   input$rank.by -> criterion
  #   
  #   if (criterion %in% "" | length(which(criterion %in% rank.options1 )) == 0){
  #     criterion <- c("")
  #   }else{
  #     
  #     foreach(i = 1:length(rank.options2), .combine = c) %do% {
  #       if (criterion == rank.options1[i]){
  #         return(rank.options2[i])
  #       }
  #       # else{
  #       #   return(criterion)
  #       # }
  #     } -> criterion; rm(i)
  #     
  #   }
  #   
  #   rank <- rank.by(criterion,filtering)
  #   rm(list = c("rank.options1","rank.options2", "filters"))
  #   
  #   if (names(rank)[1] %in% "Empty"){
  #     display <- paste(tags$br(),tags$br(),tags$br())
  #   }else{
  #     
  #     foreach(i=1:dim(rank)[1], .combine = c) %do% {
  #       paste(
  #         tags$table(style="width:100%;border-spacing:15px;padding:5px",
  #                    tags$tr(
  #                      tags$td(height="700",tags$img(src = rank$poster[i]))
  #                    )
  #         ),
  #         tags$br(),
  #         tags$br(),
  #         tags$br()
  #       )
  #     } -> posters; rm(i)
  #     
  #     display <-  paste(posters[1:length(posters)],sep='<br/>')
  #     
  #   }
  #   
  #   HTML(display)
  # 
  #  
  # })
  
  
})



  


