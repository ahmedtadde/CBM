libraries <- function(){
  library(pacman)
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
  if(is.nan(ips)) return(data)
  return(data[source %in% ips])
}

filter.by.month <- function(data,months){
  if("All" %in% months) return(data)
  return(data[month %in% months])
}


filter.by.year <- function(data,years){
  return(data[as.numeric(Year) %between% list(years[1],years[2])])
}


filter.by.studio <- function(data,studios){
  if("All" %in% studios ) return(data)
  return(data[distributor %in% studios])
}


filter.by.mpaa <- function(data,mpaas){
  if(is.nan(mpaas)) return(data)
  return(data[rating_coef %in% mpaas])
}

filter.by.runtime <- function(data, runtimes){
  if(is.nan(runtimes)) return(data)
  return(data[runtime_coef %in% runtimes])
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

render.rank.poster <- function(data){
  result <- paste0("\n",
                   shiny::htmlTemplate("./PosterTemplate.html",
                                       logo = ipLogos[[data$source]],
                                       color = ipColors[[data$source]],
                                       studio = data$distributor, 
                                       poster = data$Poster
                                      ),
                   "\n"
                   )
  return(result)
}