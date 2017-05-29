names <- c("dc","marvel","others")
data <- getData(names)

raw.data <- list("BO" = data$BO, "Critics" = data$Critics)
raw.data$BO$dc$raw$title <- raw.data$Critics$dc$raw$title
raw.data$BO$marvel$raw$title <- raw.data$Critics$marvel$raw$title
raw.data$BO$others$raw$title <- raw.data$Critics$others$raw$title
processed.data <- data$df

rm(list = c("data","names"))

shinyServer(function(input, output) {
  
  #============================== ALL TIME RANKING ===================================================
  output$Viz <- renderPlotly({
    
    Viz(processed.data)$viz
    }) 
  
  output$combined.ranking.table <- renderTable({

    Viz(processed.data)$table%>%select(2,5,3,4,1)
    })
  
  #============================== VERSUS ===================================================
  
  
  output$firstmovie.poster.CR <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    
    if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
      tags$br()
    }else{
      tags$img(src = poster.pic.name(input$firstmovie.CR))
    }
  })
  
  output$firstmovie.poster.BOP <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
      tags$br()
    }else{
      tags$img(src = poster.pic.name(input$firstmovie.BOP))
    }
    
  })
  
  output$VERSUS.BOP<- renderUI({
    
    if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0) |
       (input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
      tags$br()
    }else{
      tags$img(src = "versus.png", height =300, width = 300)
    }
    
  })
  
  
  output$VERSUS.CR<- renderUI({
    if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0) |
       (input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
      tags$br()
    }else{
      tags$img(src = "versus.png", height =300, width = 300)
    }
  })
  
  output$secondmovie.poster.BOP <- renderUI({
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
      tags$br()
    }else{
      tags$img(src = poster.pic.name(input$secondmovie.BOP))
    }
  })
  
  
  
  output$secondmovie.poster.CR <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
      tags$br()
    }else{
      tags$img(src = poster.pic.name(input$secondmovie.CR))
    }
  })
  
  
  output$firstmovie.meta.BOP <- renderUI({
    title <- input$firstmovie.BOP
    if((input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
      tags$br()
    }else{
      HTML(versus_meta(title,processed.data, raw.data,"BOP"))
    }
    
    })
  
  output$firstmovie.meta.CR <- renderUI({
    title <- input$firstmovie.CR
    if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
      tags$br()
    }else{
      HTML(versus_meta(title,processed.data, raw.data,"CR"))
    }
  })
  
  output$secondmovie.meta.BOP <- renderUI({
    title <- input$secondmovie.BOP
    if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0)){
      tags$br()
    }else{
      HTML(versus_meta(title,processed.data, raw.data,"BOP"))
    }
  })
  
  output$secondmovie.meta.CR <- renderUI({
    title <- input$secondmovie.CR
    if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
      tags$br()
    }else{
      HTML(versus_meta(title,processed.data, raw.data,"CR"))
    }
  })
  
  
  
  output$firstmovie.critics_consensus <- renderUI({
    
    if((input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
      tags$br()
    }else{
      consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                  processed.data,
                                  raw.data)
      
      consensus <- consensus$consensus
      
      HTML(consensus[[which(names(consensus) == input$firstmovie.CR)]])
    }
    
    
    })


  output$secondmovie.critics_consensus <- renderUI({
    
    if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0)){
      tags$br()
    }else{
      consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                  processed.data,
                                  raw.data)
      consensus <- consensus$consensus
      HTML(consensus[[which(names(consensus) == input$secondmovie.CR)]])
    }
  })

  output$versus_critics_chart <- renderPlotly({
    
    if((input$secondmovie.CR %in% c("") | length(input$secondmovie.CR) == 0) &
       (input$firstmovie.CR %in% c("") | length(input$firstmovie.CR) == 0)){
      
      chart <- versus_critics(c("",""),
                              processed.data,
                              raw.data)
      chart$chart
    }else{
      chart <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                processed.data,
                                raw.data)
      chart$chart
    }
    

    
  })
  
  
  output$versus.weekly.avg <- renderPlotly({
    
    if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
       (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
      
      versus.weekly.avg(c("",""),processed.data,raw.data)
      
    }else{
      versus.weekly.avg(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    }
    
    
    
  })
  
  output$versus.weekly.perc <- renderPlotly({
    
    if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
       (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
      
      versus.weekly.perc(c("",""),processed.data,raw.data)
      
    }else{
      versus.weekly.perc(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    }
    
    
    
  })
  
  output$versus.weekly.rank <- renderPlotly({
    
    if((input$secondmovie.BOP %in% c("") | length(input$secondmovie.BOP) == 0) &
       (input$firstmovie.BOP %in% c("") | length(input$firstmovie.BOP) == 0)){
      
      versus.weekly.rank(c("",""),processed.data,raw.data)
      
    }else{
      versus.weekly.rank(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    }
    
    
    
  })
  
  
  output$versus_BO_chart.1 <- renderPlotly({
    versus.BO.chart.1(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data)
  })
  
  output$versus_BO_chart.2 <- renderPlotly({
    versus.BO.chart.2(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data)
  })
  
  
   #============================== RANKING ===================================================
  
  output$rank.number <- renderUI({
    
    
    ip <- input$ip
    studio <- input$studio
    years <- input$years
    
    months <- input$months
    months[which(months == "Jan")] <- 1
    months[which(months == "Feb")] <- 2
    months[which(months == "Mar")] <- 3
    months[which(months == "Apr")] <- 4
    months[which(months == "May")] <- 5
    months[which(months == "Jun")] <- 6
    months[which(months == "Jul")] <- 7
    months[which(months == "Aug")] <- 8
    months[which(months == "Sep")] <- 9
    months[which(months == "Oct")] <- 10
    months[which(months == "Nov")] <- 11
    months[which(months == "Dec")] <- 12
    
    runtime <- input$runtime
    runtime[which(runtime == "Under 1h30")] <- 1
    runtime[which(runtime == "Between 1h:30 and 2h:00")] <- 2
    runtime[which(runtime == "Between 2h:00 and 2h:30")] <- 3
    runtime[which(runtime == "Between 2h:30 and 3h:00")] <- 4
    
    rating <- input$rating
    rating[which(rating == "PG")] <- 1
    rating[which(rating == "PG-13")] <- 2
    rating[which(rating == "R")] <- 3
    
    metascore <- input$metascore
    imdb <- input$imdb
    RT_perc <- input$RT_perc
    RT_rating <- input$RT_rating
    RT_audience_perc <- input$RT_audience_perc
    RT_audience_rating <- input$RT_audience_rating
    
    filters <- list(ip,studio,years,months,runtime, rating,
                    metascore,imdb,RT_perc,RT_rating,
                    RT_audience_perc,RT_audience_rating)
    
    filtering <- filter.by(filters,processed.data,raw.data)
    
    
    foreach(i=1:dim(filtering$processed.data)[1]) %do% {
      paste(
        tags$table(style="width:100%;border-spacing:15px;padding:5px",
                   tags$tr(
                     tags$td(height="700", tags$a(as.character(i)))
                   )
        ),
        tags$br(),
        tags$br(),
        tags$br()
      )
    } -> ranking; rm(i)
    
    HTML(
      paste(ranking[1:length(ranking)],sep='<br/>'))
    
  })
  
  
  
  
  
  
  output$report <- renderUI({
    ip <- input$ip
    studio <- input$studio
    years <- input$years
  
    months <- input$months
    months[which(months == "Jan")] <- 1
    months[which(months == "Feb")] <- 2
    months[which(months == "Mar")] <- 3
    months[which(months == "Apr")] <- 4
    months[which(months == "May")] <- 5
    months[which(months == "Jun")] <- 6
    months[which(months == "Jul")] <- 7
    months[which(months == "Aug")] <- 8
    months[which(months == "Sep")] <- 9
    months[which(months == "Oct")] <- 10
    months[which(months == "Nov")] <- 11
    months[which(months == "Dec")] <- 12
  
    runtime <- input$runtime
    runtime[which(runtime == "Under 1h30")] <- 1
    runtime[which(runtime == "Between 1h:30 and 2h:00")] <- 2
    runtime[which(runtime == "Between 2h:00 and 2h:30")] <- 3
    runtime[which(runtime == "Between 2h:30 and 3h:00")] <- 4
  
    rating <- input$rating
    rating[which(rating == "PG")] <- 1
    rating[which(rating == "PG-13")] <- 2
    rating[which(rating == "R")] <- 3
  
    metascore <- input$metascore
    imdb <- input$imdb
    RT_perc <- input$RT_perc
    RT_rating <- input$RT_rating
    RT_audience_perc <- input$RT_audience_perc
    RT_audience_rating <- input$RT_audience_rating
  
    filters <- list(ip,studio,years,months,runtime, rating,
                    metascore,imdb,RT_perc,RT_rating,
                    RT_audience_perc,RT_audience_rating)
  
    filtering <- filter.by(filters,processed.data,raw.data)
  
  
  
    rank.options1 <- c("Overall Critical Reception",
                       "Overall Box Office Performance",
                       "Rotten Tomatoes Critics Tomatometer",
                       "Rotten Tomatoes Critics Rating",
                       "Custom Rotten Tomatoes Critics Score",
                       "Rotten Tomatoes Audience Tomatometer",
                       "Rotten Tomatoes Audience Rating",
                       "Custom Rotten Tomatoes Audience Score",
                       "Metascore","IMDB",
                       "Foreign Box Office Gross", 
                       "Domestic Box Office Gross",
                       "Foreign & Domestic BO - Weighted Mean (0.3 & 0.7)",
                       "Weekly Average per Theater",
                       "Weekly Grosses as % of Opening Week",
                       "Weekly Ranking")
  
    rank.options2 <- c(
                        "critics_score",
                        "bo_score",
                        "RT_perc",
                        "RT_rating",
                        "RT_score",
                        "RT_audience_perc",
                        "RT_audience_rating",
                        "RT_audience_score",
                        "metascore","imdb_rating",
                        "foreign_BO","domestic_BO","combined_BO",
                        "avg","change","rank"
                        )
    
    input$rank.by -> criterion
    
    if (criterion %in% "" | length(which(criterion %in% rank.options1 )) == 0){
      criterion <- c("")
    }else{
      
      foreach(i = 1:length(rank.options2), .combine = c) %do% {
        if (criterion == rank.options1[i]){
          return(rank.options2[i])
        }
        # else{
        #   return(criterion)
        # }
      } -> criterion; rm(i)
      
    }
    
    rank <- rank.by(criterion,filtering)
    rm(list = c("rank.options1","rank.options2", "filters"))
    
    if (names(rank)[1] %in% "Empty"){
      display <- paste(tags$br(),tags$br(),tags$br())
    }else{
      
      foreach(i=1:dim(rank)[1], .combine = c) %do% {
        paste(
          tags$table(style="width:100%;border-spacing:15px;padding:5px",
                     tags$tr(
                       tags$td(height="700",HTML(paste(rank$movie_report[i])))
                     )
          ),
          tags$br(),
          tags$br(),
          tags$br()
        )
      } -> reports; rm(i)
      
      display <-  paste(reports[1:length(reports)],sep='<br/>')
      
    }
    
    HTML(display)
  })
  
  
  
  
  output$poster <- renderUI({
    ip <- input$ip
    studio <- input$studio
    years <- input$years
  
    months <- input$months
    months[which(months == "Jan")] <- 1
    months[which(months == "Feb")] <- 2
    months[which(months == "Mar")] <- 3
    months[which(months == "Apr")] <- 4
    months[which(months == "May")] <- 5
    months[which(months == "Jun")] <- 6
    months[which(months == "Jul")] <- 7
    months[which(months == "Aug")] <- 8
    months[which(months == "Sep")] <- 9
    months[which(months == "Oct")] <- 10
    months[which(months == "Nov")] <- 11
    months[which(months == "Dec")] <- 12
  
    runtime <- input$runtime
    runtime[which(runtime == "Under 1h30")] <- 1
    runtime[which(runtime == "Between 1h:30 and 2h:00")] <- 2
    runtime[which(runtime == "Between 2h:00 and 2h:30")] <- 3
    runtime[which(runtime == "Between 2h:30 and 3h:00")] <- 4
  
    rating <- input$rating
    rating[which(rating == "PG")] <- 1
    rating[which(rating == "PG-13")] <- 2
    rating[which(rating == "R")] <- 3
  
    metascore <- input$metascore
    imdb <- input$imdb
    RT_perc <- input$RT_perc
    RT_rating <- input$RT_rating
    RT_audience_perc <- input$RT_audience_perc
    RT_audience_rating <- input$RT_audience_rating
  
    filters <- list(ip,studio,years,months,runtime, rating,
                    metascore,imdb,RT_perc,RT_rating,
                    RT_audience_perc,RT_audience_rating)
  
    filtering <- filter.by(filters,processed.data,raw.data)
  
  
  
    rank.options1 <- c("Overall Critical Reception",
                       "Overall Box Office Performance",
                       "Rotten Tomatoes Critics Tomatometer",
                       "Rotten Tomatoes Critics Rating",
                       "Custom Rotten Tomatoes Critics Score",
                       "Rotten Tomatoes Audience Tomatometer",
                       "Rotten Tomatoes Audience Rating",
                       "Custom Rotten Tomatoes Audience Score",
                       "Metascore","IMDB","Foreign Box Office Gross", 
                       "Domestic Box Office Gross",
                       "Foreign & Domestic BO - Weighted Mean (0.3 & 0.7)",
                       "Weekly Average per Theater",
                       "Weekly Grosses as % of Opening Week",
                       "Weekly Ranking")
  
    rank.options2 <- c(
      "critics_score",
      "bo_score",
      "RT_perc",
      "RT_rating",
      "RT_score",
      "RT_audience_perc",
      "RT_audience_rating",
      "RT_audience_score",
      "metascore","imdb_rating",
      "foreign_BO","domestic_BO","combined_BO",
      "avg","change","rank"
    )
    
    input$rank.by -> criterion
    
    if (criterion %in% "" | length(which(criterion %in% rank.options1 )) == 0){
      criterion <- c("")
    }else{
      
      foreach(i = 1:length(rank.options2), .combine = c) %do% {
        if (criterion == rank.options1[i]){
          return(rank.options2[i])
        }
        # else{
        #   return(criterion)
        # }
      } -> criterion; rm(i)
      
    }
    
    rank <- rank.by(criterion,filtering)
    rm(list = c("rank.options1","rank.options2", "filters"))
    
    if (names(rank)[1] %in% "Empty"){
      display <- paste(tags$br(),tags$br(),tags$br())
    }else{
      
      foreach(i=1:dim(rank)[1], .combine = c) %do% {
        paste(
          tags$table(style="width:100%;border-spacing:15px;padding:5px",
                     tags$tr(
                       tags$td(height="700",tags$img(src = rank$poster[i]))
                     )
          ),
          tags$br(),
          tags$br(),
          tags$br()
        )
      } -> posters; rm(i)
      
      display <-  paste(posters[1:length(posters)],sep='<br/>')
      
    }
    
    HTML(display)
  
   
  })
  
  
  output$find.movie.poster <- renderUI({
    
    dt <- find_movie(input$find.movie, processed.data, raw.data)
    
    if (names(dt)[1] %in% "Empty"){
      display <- paste(tags$br(),
                       tags$br(),
                       tags$br()
      )
    }else{
      foreach(i=1:dim(dt)[1], .combine = c) %do% {
        paste(
          tags$table(style="width:100%;border-spacing:15px;padding:5px",
                     tags$tr(tags$td(height="700",tags$img(src = dt$poster[i])))
          ),
          tags$br(),
          tags$br(),
          tags$br()
        )
      } -> posters; rm(i)
      
      display <- paste(posters[1:length(posters)],sep='<br/>')
    }
    
    
    HTML(display)
    
    
  })
  
  
  output$find.movie.report <- renderUI({
    
    dt <- find_movie(input$find.movie, processed.data, raw.data)
    
    if (names(dt)[1] %in% "Empty"){
      display <- paste(tags$br(),
                       tags$br(),
                       tags$br()
                      )
    }else{
      foreach(i=1:dim(dt)[1], .combine = c) %do% {
        paste(
          tags$table(style="width:100%;border-spacing:15px;padding:5px",
                     tags$tr(tags$td(height="700",HTML(paste(dt$movie_report[i]))))
                     ),
          tags$br(),
          tags$br(),
          tags$br()
          )
      } -> text.reports; rm(i)
      
      display <- paste(text.reports[1:length(text.reports)],sep='<br/>')
    }
    
    
    HTML(display)
    
    
  })
  
  
})



  


