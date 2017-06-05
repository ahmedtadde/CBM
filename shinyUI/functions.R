libraries <- function(){
  library(pacman)
  p_load(Hmisc)
  p_load(stringi)
  p_load(data.table)
  p_load(shiny)
  p_load(foreach)
  p_load(DBI)
  p_load(RSQLite)
  
}

get.all.time.ranking.data <- function(){
  database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
  
  poster <- data.table(dbGetQuery(database, 'SELECT imdbID, Title, Poster, runtime, Released, rating FROM summaries'))
  scores <- data.table(dbGetQuery(database, 'SELECT imdbID,bo_score,critical_score,overall_score FROM scoring'))
  
  setkey(poster, imdbID)
  setkey(scores, imdbID)
  
  data <- poster[scores, nomatch = 0]
  data <- data[!is.infinite(overall_score) | !is.infinite(bo_score)]
  data[, critical_score := set.critical.reception.range(critical_score)
       ][, grade := as.character(overall.scores.to.grades(overall_score))] 
  
  setorder(data, -overall_score)
  
  data[, ranking := 1:c(dim(data)[1])]
  
  dbDisconnect(database)
  rm(list=c("database","poster","scores"))
  
  data
}

format.money  <- function(x) {
  paste0("$",formatC(as.numeric(x), format="f", digits=1, big.mark=","))
}

set.distributor.names <- function(studios){
  
  foreach(k=1:length(studios)) %do%{
    if(studios[k] == "Buena Vista") studios[k] <- "Disney"
    if(studios[k] == "Sony / Columbia") studios[k] <- "Sony"
    if(studios[k] == "Sony (Revolution)") studios[k] <- "Sony"
    if(studios[k] == "Lionsgate/Summit") studios[k] <- "Lionsgate"
    if(studios[k] == "Lions Gate") studios[k] <- "Lionsgate"
    if(studios[k] == "Weinstein / Dimension") studios[k] <- "Weinstein"
    
  }
  
  return(studios)
}


get.release.months <- function(releases){
  foreach(k=1:length(releases), .combine = c) %do%{
    return(stri_trim_both(unlist(stri_split_fixed(releases[k], " "))[2]))
  } -> results
  
  return(results)
}

get.ranking.metrics <- function(inputs){
  if(is.null(inputs) | length(inputs) == 0) return(NULL)
  metrics <- uiInputOptions$rankMetrics
  
  foreach(i =1:length(inputs), .combine = c)%do%{
    foreach(j=1:length(metrics),.combine = c) %do%{
      if(inputs[i] == names(metrics)[j]) return(unname(metrics[j]))
    } -> metric
    
    return(metric)
  } -> metrics
  
  return(metrics)
}

set.critical.reception.range <- function(scores){
  foreach(i=1:length(scores), .combine = c)%do%{
    if (scores[i] < 0 ) scores[i] <- 0
    if (scores[i] > 100 ) scores[i] <- 100
    return(scores[i])
  } -> new.scores
  
  return(new.scores)
}

overall.scores.to.grades <- function(scores){
  cut(scores,
      breaks = 11,
      labels = c("F*** You!",
                 "F",
                 "E",
                 "D-",
                 "D",
                 "C-",
                 "C",
                 "B-",
                 "B",
                 "A-",
                 "A"),
      ordered_result = T
    )
}


filter.by.ip <- function(data,ips){
  if(is.null(ips) | length(ips) == 0) return(data)
  if(is.null(data)) return(NULL)
  return(data[source %in% ips])
}

filter.by.month <- function(data,months){
  if("All" %in% months | is.null(months) | length(months) == 0) return(data)
  if(is.null(data)) return(NULL)
  return(data[month %in% months])
}


filter.by.year <- function(data,years){
  if(is.null(data)) return(NULL)
  return(data[as.numeric(Year) %between% list(years[1],years[2])])
}


filter.by.studio <- function(data,studios){
  if("All" %in% studios | is.null(studios) | length(studios) == 0) return(data)
  if(is.null(data)) return(NULL)
  return(data[distributor %in% studios])
}


filter.by.mpaa <- function(data,mpaas){
  if(is.null(mpaas)  | length(mpaas) == 0) return(data)
  if(is.null(data)) return(NULL)
  return(data[rating %in% mpaas])
}

filter.by.runtime <- function(data, runtimes){
  if(is.null(data)) return(NULL)
  if("All" %in% runtimes | is.null(runtimes) | length(runtimes) == 0) return(data)
  foreach(k=1:length(runtimes), .combine = c) %do%{
    if(runtimes[k] == "0 to 100 minutes") return(1)
    if(runtimes[k] == "101 to 120 minutes") return(2)
    if(runtimes[k] == "121 to 150 minutes") return(3)
    if(runtimes[k] == "151 to 180 minutes") return(4)
    if(runtimes[k] == "180+ minutes") return(5)
  } -> values
  return(data[runtime_coef %in% values])
}

filter.by.imdb <- function(data, imdbs){
  if(is.null(data)) return(NULL)
  return(data[imdbRating %between% list(imdbs[1], imdbs[2])])
}

filter.by.metascore <- function(data, metascores){
  if(is.null(data)) return(NULL)
  return(data[Metascore %between% list(metascores[1],metascores[2])])
}

filter.by.rt <- function(data, rts){
  if(is.null(data)) return(NULL)
  return(data[rt %between% list(rts[1],rts[2])])
}


render.compare.poster.template <- function(data){
  # if(is.null(data)) return('')
  
  paste0(
    shiny::htmlTemplate("./comparePosterTemplate.html",
                        release = data$Release,
                        runtime = data$runtime,
                        poster = data$Poster,
                        rating = data$rating,
                        grade = data$grade
                        )
    )
}

render.filter.rank.template <- function(data,rank.by.metrics){
  
  if(data$source[1] == "DC"){
    iplogo = "https://4.bp.blogspot.com/-7K0dR8MmfI4/Vzx4nd5MJzI/AAAAAAAAVz4/59mGrWzNWnMWy880PfkHClXDP8LeiiHpACLcB/s1600/fixed.png"
    ipcolor = "#0282f9"
  }else if(data$source[1] == "MARVEL"){
    iplogo = "https://logorealm.com/wp-content/uploads/2016/07/Marvel-Logo.png"
    ipcolor = "#ed1717"
  }else{
    iplogo = "https://cdn.dribbble.com/users/1960/screenshots/877446/logo_1x.png"
    ipcolor = "#c9a318"
  }
  
  keys <- key(data)
  if(length(keys) == 3){
    
    metricOne <- rank.by.metrics[1]
    metricTwo <- rank.by.metrics[2]
    metricThree <- rank.by.metrics[3]
    
    metricOneValue <- data[,get(keys[1])]
    metricTwoValue <- data[,get(keys[2])]
    metricThreeValue <- data[,get(keys[3])]
    
    ranking <-  data$ranking
    
  }else if(length(keys) == 2){
    
    metricOne = rank.by.metrics[1]
    metricTwo = rank.by.metrics[2]
    metricThree = ''
    
    metricOneValue <- data[,get(keys[1])]
    metricTwoValue <- data[,get(keys[2])]
    metricThreeValue<- ''
    
    ranking <-  data$ranking
  }else if(length(keys) ==1 & "imdbID" %notin% keys){
    
    metricOne = rank.by.metrics[1]
    metricTwo = ''
    metricThree = ''
    
    metricOneValue <- data[,get(keys[1])]
    metricTwoValue <- ''
    metricThreeValue <- ''
    
    ranking <-  data$ranking
  }else{
    metricOne = ''
    metricTwo = ''
    metricThree = ''
    
    metricOneValue <- ''
    metricTwoValue <- ''
    metricThreeValue <- ''
    
    ranking <-  ''
    
  }
  
  
  paste0(
    shiny::htmlTemplate("./filterRankTemplate.html", 
                        iplogo = iplogo,
                        ipcolor= ipcolor,
                        studio = data$distributor,
                        release = data$Release,
                        runtime = data$runtime,
                        poster = data$Poster,
                        ranking = ranking,
                        rating = data$rating,
                        budget = format.money(data$budget),
                        openingWeek = format.money(data$week_1_gross),
                        domestic = format.money(data$domestic_BO),
                        foreign = format.money(data$foreign_BO),
                        metascore = data$Metascore,
                        rt = data$rt,
                        imdb = data$imdbRating,
                        grade = data$grade,
                        plot= data$Plot,
                        metricOne=metricOne,
                        metricTwo=metricTwo,
                        metricThree=metricThree,
                        metricOneValue=metricOneValue,
                        metricTwoValue=metricTwoValue,
                        metricThreeValue=metricThreeValue
                      )
  )
}


render.all.time.ranking.table.template <- function(data){
  paste0(shiny::htmlTemplate("./rankingTableTemplate.html", 
                             ranking = data$ranking,
                             poster = data$Poster,
                             Title = data$title,
                             grade = data$grade,
                             critics = data$critical_score,
                             bo = data$bo_score))
}


render.404.card.template <- function(){
  paste0(shiny::htmlTemplate("./404cardTemplate.html"))
}




"%notin%" <- Negate("%in%") 