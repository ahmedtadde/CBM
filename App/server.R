source('../R/helpers.R')
libraries()

names <- c("dc","marvel")
data <- getData(names)

raw.data <- list("BO" = data$BO, "Critics" = data$Critics)
raw.data$BO$dc$raw$title <- raw.data$Critics$dc$raw$title
raw.data$BO$marvel$raw$title <- raw.data$Critics$marvel$raw$title
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
    
  
    tags$img(src = poster.pic.name(input$firstmovie.CR))
  })
  
  output$firstmovie.poster.BOP <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    
    tags$img(src = poster.pic.name(input$firstmovie.BOP))
  })
  
  output$VERSUS.BOP<- renderUI({
    
    tags$img(src = "versus.png", height =300, width = 300)
  })
  
  
  output$VERSUS.CR<- renderUI({
    
    tags$img(src = "versus.png", height =300, width = 300)
    })
  
  output$secondmovie.poster.BOP <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    tags$img(src = poster.pic.name(input$secondmovie.BOP))
    })
  
  
  
  output$secondmovie.poster.CR <- renderUI({
    
    poster.pic.name <- function(title){
      name <- paste0(gsub(" ","", title),".png")
      name <- gsub(":","", name)
      return(name)
    }
    
    tags$img(src = poster.pic.name(input$secondmovie.CR))
  })
  
  
  output$firstmovie.meta.BOP <- renderUI({
    title <- input$firstmovie.BOP
    HTML(versus_meta(title,processed.data, raw.data))
    })
  
  output$firstmovie.meta.CR <- renderUI({
    title <- input$firstmovie.CR
    HTML(versus_meta(title,processed.data, raw.data))
    })
  
  output$secondmovie.meta.BOP <- renderUI({
    title <- input$secondmovie.BOP
    HTML(versus_meta(title,processed.data ,raw.data))
    })
  
  output$secondmovie.meta.CR <- renderUI({
    title <- input$secondmovie.CR
    HTML(versus_meta(title,processed.data ,raw.data))
  })
  
  
  
  output$firstmovie.critics_consensus <- renderUI({
    consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                   processed.data,
                                   raw.data)

    consensus <- consensus$consensus

    HTML(consensus[[which(names(consensus) == input$firstmovie.CR)]])
    })


  output$secondmovie.critics_consensus <- renderUI({
    consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                   processed.data,
                                   raw.data)
    consensus <- consensus$consensus
    HTML(consensus[[which(names(consensus) == input$secondmovie.CR)]])
    })

  output$versus_critics_chart <- renderPlotly({
    consensus <- versus_critics(c(input$firstmovie.CR, input$secondmovie.CR),
                                processed.data,
                                raw.data)

    consensus$chart
    })
  
  
  output$versus.weekly.avg <- renderPlotly({
    
    versus.weekly.avg(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    
  })
  
  output$versus.weekly.perc <- renderPlotly({
    
    versus.weekly.perc(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    
  })
  
  output$versus.weekly.rank <- renderPlotly({
    
    versus.weekly.rank(c(input$firstmovie.BOP, input$secondmovie.BOP),processed.data,raw.data)
    
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
                     tags$td(height="600", tags$a(as.character(i)))
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
                       "Metascore","IMDB","Foreign Box Office Gross", 
                       "Domestic Box Office Gross",
                       "Foreign & Domestic BO - Weighted Mean (30/70)",
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
  
    foreach(i = 1:length(rank.options2), .combine = c) %do% {
      if (criterion == rank.options1[i]){
        return(rank.options2[i])
        }
    } -> criterion; rm(i)
  
    criterion
    rank <- rank.by(criterion,filtering)
    rm(list = c("rank.options1","rank.options2", "filters"))
  
    foreach(i=1:dim(rank)[1]) %do% {
      paste(
        tags$table(style="width:100%;border-spacing:15px;padding:5px",
                   tags$tr(
                     tags$td(height="600",HTML(paste(rank$movie_report[i])))
                   )
        ),
        tags$br(),
        tags$br(),
        tags$br()
      )
    } -> text.reports; rm(i)
  
    HTML(
      paste(text.reports[1:length(text.reports)],sep='<br/>'))
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
                       "Foreign & Domestic BO - Weighted Mean (30/70)",
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
  
    foreach(i = 1:length(rank.options2), .combine = c) %do% {
      if (criterion == rank.options1[i]){
        return(rank.options2[i])
      }
    } -> criterion; rm(i)
  
    criterion
    rank <- rank.by(criterion,filtering)
    rm(list = c("rank.options1","rank.options2", "filters"))
  
    foreach(i=1:dim(rank)[1]) %do% {
      paste(
        tags$table(style="width:100%;border-spacing:15px;padding:5px",
          tags$tr(
            tags$td(height="600",tags$img(src = rank$poster[i]))
            )
        ),
        tags$br(),
        tags$br(),
        tags$br()
      )
    } -> posters; rm(i)
  
     HTML(
      paste(posters[1:length(posters)],sep='<br/>'))
  
   
  })
  
  
})



  


