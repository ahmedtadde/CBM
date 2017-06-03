libraries <- function(){
  library(pacman)
  p_load(Hmisc)
  p_load(stringi)
  p_load(httr)
  p_load(magrittr)
  p_load(dplyr)
  p_load(data.table)
  p_load(plotly)
  p_load(shiny)
  p_load(shinythemes)
  p_load(foreach)
  p_load(DBI)
  p_load(RSQLite)
  
}

format.money  <- function(x) {
  formatC(as.numeric(x), format="f", digits=1, big.mark=",")
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


get.release.months <- function(releases){
  foreach(k=1:length(releases), .combine = c) %do%{
    return(stri_trim_both(unlist(stri_split_fixed(releases[k], " "))[2]))
  } -> results
  
  return(results)
}


filter.by.ip <- function(data,ips){
  if(is.null(ips) | length(ips) == 0) return(data)
  return(data[source %in% ips])
}

filter.by.month <- function(data,months){
  if("All" %in% months | is.null(months) | length(months) == 0) return(data)
  return(data[month %in% months])
}


filter.by.year <- function(data,years){
  return(data[as.numeric(Year) %between% list(years[1],years[2])])
}


filter.by.studio <- function(data,studios){
  if("All" %in% studios | is.null(studios) | length(studios) == 0) return(data)
  return(data[distributor %in% studios])
}


filter.by.mpaa <- function(data,mpaas){
  if(is.null(mpaas)  | length(mpaas) == 0) return(data)
  return(data[rating %in% mpaas])
}

filter.by.runtime <- function(data, runtimes){
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
  return(data[imdbRating %between% list(imdbs[1], imdbs[2])])
}

filter.by.metascore <- function(data, metascores){
  return(data[Metascore %between% list(metascores[1],metascores[2])])
}

filter.by.rt <- function(data, rts){
  return(data[rt %between% list(rts[1],rts[2])])
}

render.filterRankTemplate <- function(data){
  
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
  
  # shiny::htmlTemplate("./flipCardBack.html", 
  #                     rating = data$rating,
  #                     budget = data$budget,
  #                     plot= data$Plot
  #                     ) -> backHTML
  
  # return(
  #   shiny::htmlTemplate("./filterRankTemplate.html", 
  #                       poster = data$Poster,
  #                       flipCardBack = backHTML
  #                       )
  # )
  result <- paste0(
                   shiny::htmlTemplate("./filterRankTemplate.html", 
                                       iplogo = iplogo,
                                       ipcolor= ipcolor,
                                       studio = data$distributor,
                                       release = data$Release,
                                       runtime = data$runtime,
                                       poster = data$Poster,
                                       ranking = data$ID,
                                       rating = data$rating,
                                       budget = format.money(data$budget),
                                       openingWeek = format.money(data$week_1_gross),
                                       domestic = format.money(data$domestic_BO),
                                       foreign = format.money(data$foreign_BO),
                                       metascore = data$Metascore,
                                       rt = data$rt,
                                       imdb = data$imdbRating,
                                       plot= data$Plot
                                       )
                  )
  
  
  # result <- paste0("\n",
  #                  shiny::htmlTemplate("./filterRankTemplate.html",
  #                                      logo = logo,
  #                                      color = color,
  #                                      studio = data$distributor, 
  #                                      poster = data$Poster
  #                                     ),
  #                  "\n"
  #                  )
  return(result)
}




