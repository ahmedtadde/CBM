
libraries <- function(){
  library(shiny)
  library(shinyapps)
  library(shinythemes)
  library(data.table)
  library(dplyr)
  library(httr)
  library(plotly)
  library(foreach)
  library(arules)
  library(psych)
  library(DataCombine)
  
}

getBoxOfficeData <- function(names){
  
  setwd('../R')
  foreach( i = 1:length(names)) %do% {
    bom = read.csv(paste0(names[i],"_bom.csv"),
                   na.strings = "NA",
                   stringsAsFactors = F,
                   header = TRUE)
    
    bom = bom[ , order(names(bom))]
    
    bom = data.table(bom,keep.rownames = FALSE)
    
    colum.group.1 <- c("director",
                       "distributor",
                       "week_1_gross",
                       "budget",
                       "domestic_BO",
                       "foreign_BO",
                       "genre",
                       "rating",
                       "runtime",
                       "Title")
    
    
    df.1 <- select(bom, which(names(bom)%in% colum.group.1))
    df.1 <- data.table(data.frame(df.1)[ , order(names(df.1),decreasing = TRUE)])
    setnames(df.1, c("Title","week_1_gross"),c("title","ow_gross"))
    df.info <- df.1

    colum.group.2.1 <- paste("week",seq(1,9),"avg", sep="_")
    colum.group.2.2 <- paste("week",seq(10,15),"avg", sep="_")
    df.2.1 <- select(bom, which(names(bom)%in% colum.group.2.1))
    df.2.2 <- select(bom, which(names(bom)%in% colum.group.2.2))
    df.avg <- data.table(cbind(df.2.1,df.2.2))

    df.avg <- data.frame(apply(data.frame(df.avg),2,as.numeric,na.rm = T))
    df.avg <- data.table(cbind("title"=bom$Title, df.avg))
    df.avg[, score := apply(select(df.avg,c(2:16)),1, weighted.mean, seq(1,15), na.rm = T)]
    df.avg[, score := score/10000]

    colum.group.2.1 <- paste("week",seq(1,9),"change", sep="_")
    colum.group.2.2 <- paste("week",seq(10,15),"change", sep="_")
    df.2.1 <- select(bom, which(names(bom)%in% colum.group.2.1))
    df.2.2 <- select(bom, which(names(bom)%in% colum.group.2.2))
    df.change <- data.table(cbind(df.2.1,df.2.2))
    df.change <- data.frame(apply(data.frame(df.change),2,as.numeric,na.rm = T))

    normalize.change <- function(vector) {

      negative <- vector[which(vector<0)]
      negative <- (1+negative)
      vector[which(vector<0)] <- negative
      return(vector)
    }

    df.change <- data.frame(apply(data.frame(df.change),2,normalize.change))

    cumulative.change <- function(vector){
      vector[which(is.na(vector))] <- 0
      vector <-  cumprod(vector)
      vector[which(vector == 0.0)] <- NA

      return(vector)
    }

    columns <- names(df.change)
    df.change <- transpose(data.frame(apply(data.frame(df.change),1,cumulative.change)))
    names(df.change) <- columns
    df.change$score <- apply(df.change,1, weighted.mean, na.rm = T, seq(2,15))
    df.change <- data.table(cbind("title"=bom$Title, df.change))

    colum.group.2.1 <- paste("week",seq(1,9),"rank", sep="_")
    colum.group.2.2 <- paste("week",seq(10,15),"rank", sep="_")
    df.2.1 <- select(bom, which(names(bom)%in% colum.group.2.1))
    df.2.2 <- select(bom, which(names(bom)%in% colum.group.2.2))
    df.rank <- data.table(cbind(df.2.1,df.2.2))
    df.rank <- data.frame(apply(data.frame(df.rank),2,as.numeric,na.rm = T))

    normalize.rank <- function(vector) {10/vector}
    df.rank.bis <- transpose(data.frame(apply(df.rank,1,normalize.rank)))

    rank.score <- apply(df.rank.bis,1, weighted.mean, na.rm = T, seq(1,15))
    df.rank <- data.table(cbind("title"=bom$Title, df.rank, "score" = rank.score))


    bom_score <- data.table("avg" = df.avg$score,
                            "change" = df.change$score,
                            "rank" = df.rank$score)
    
    
    
    
    info <- df.info %>% select(c(2,1,3,4,6,7,10))
    
    # return(info)
    
    
    info$runtime[which(info$runtime < 90)] <- 1
    info$runtime[which(info$runtime >= 90 & info$runtime < 120 )] <- 2
    info$runtime[which(info$runtime >= 120 & info$runtime < 150 )] <- 3
    info$runtime[which(info$runtime >= 150 & info$runtime < 180 )] <- 4
    info$runtime[which(info$runtime >= 180)] <- 5

    info$rating <- as.character(info$rating)
    info$rating[which(info$rating== "PG")] <- 1
    info$rating[which(info$rating== "PG-13")] <- 2
    info$rating[which(info$rating== "R")] <- 3
    info$rating <- as.numeric(info$rating)
    
    
    
    info$budget[which(info$title %in% c("The Amazing Spider-Man 2") & is.na(info$budget))] <- 2.55e+08
    info$budget[which(info$title %in% c("Batman: Mask of the Phantasm") & is.na(info$budget))] <- 6e+06
    info$budget[which(info$title %in% c("Blade") & is.na(info$budget))] <- 4.5e+07
    info$budget[which(info$title %in% c("Superman III") & is.na(info$budget))] <- 3.9e+07
    info$budget[which(info$title %in% c("Supergirl") & is.na(info$budget))] <- 3.5e+07
    
    info$budget[which(info$title %in% c("americansplendor"))] <- 2e+07
    info$budget[which(info$title %in% c("avp2") & is.na(info$budget))] <- 4e+07
    info$budget[which(info$title %in% c("buffythevampireslayer") & is.na(info$budget))] <- 7e+06
    info$budget[which(info$title %in% c("bulletproofmonk") & is.na(info$budget))] <- 5.2e+07
    info$budget[which(info$title %in% c("bullettothehead") & is.na(info$budget))] <- 5.5e+07
    info$budget[which(info$title %in% c("chronicle2012") & is.na(info$budget))] <- 1.2e+07
    info$budget[which(info$title %in% c("crowcityofangels") & is.na(info$budget))] <- 6.5e+07
    info$budget[which(info$title %in% c("leagueofextraordinarygentlemen") & is.na(info$budget))] <- 7.8e+07
    info$budget[which(info$title %in% c("maskofzorro") & is.na(info$budget))] <- 9.5e+07
    info$budget[which(info$title %in% c("powerpuffgirls") & is.na(info$budget))] <- 1.1e+07
    info$budget[which(info$title %in% c("priest07") & is.na(info$budget))] <- 6.0e+07
    info$budget[which(info$title %in% c("roadtoperdition") & is.na(info$budget))] <- 8.0e+07
    info$budget[which(info$title %in% c("spawn") & is.na(info$budget))] <- 4.0e+07
    info$budget[which(info$title %in% c("super2011") & is.na(info$budget))] <- 2.0e+06
    info$budget[which(info$title %in% c("superexgirlfriend") & is.na(info$budget))] <- 3.0e+07
    info$budget[which(info$title %in% c("wanted") & is.na(info$budget))] <- 7.5e+07
    info$budget[which(info$title %in% c("spirit09") & is.na(info$budget))] <- 6.0e+07
    info$budget[which(info$title %in% c("zorro2") & is.na(info$budget))] <- 8.0e+07
    info$budget[which(info$title %in% c("tmntcg") & is.na(info$budget))] <- 3.4e+07
    info$budget[which(info$title %in% c("tmnt2013") & is.na(info$budget))] <- 1.25e+08
    info$budget[which(info$title %in% c("tmnt3") & is.na(info$budget))] <- 2.1e+07
    info$budget[which(info$title %in% c("tnmt2") & is.na(info$budget))] <- 2.5e+07
    
    # return(info)
    # fill.budgets_1 <- which(is.na(info$budget))
    # fill.budgets_2 <- c( 255*10^6,)
    # 
    # foreach( i = 1: length(fill.budgets_2)) %do%{
    #   
    #   info$budget[fill.budgets_1[i],] <- fill.budgets_2[i]
    #   
    # }
    
    info <- data.table(info)
    info[,title := NULL]
    

    # info$combined_BO <- apply(select(info, which(names(info) %in% c("foreign_BO","domestic_BO"))),
    #                           1, weighted.mean,c(0.3,0.7) ,na.rm = T)
    # info$combined_BO <- (info$combined_BO)/(10^8)
    # info$ow_score.1 <- 1 - (info$ow_gross/info$domestic_BO)
    # info$ow_score.2 <- info$ow_gross/info$budget
    # info$investment_return <- info$domestic_BO/info$budget

    
    v <- apply(select(info, which(names(info) %in% c("foreign_BO","domestic_BO"))),
               1, weighted.mean,c(0.3,0.7) ,na.rm = T)

    w <- 1 - (info$ow_gross/info$domestic_BO)
    x <- info$ow_gross/info$budget
    y <- v/info$budget
    
    weeks_in_theater <- function(vector){length(na.omit(vector))}
    z <- apply(select(df.rank, c(2:16)),1,weeks_in_theater)/15
    
    table <- data.table(v,w,x,y,z,
                        info$budget/10^8,
                        info$ow_gross/10^8,
                        info$domestic_BO/10^8,
                        info$domestic_BO/info$budget,
                        info$foreign_BO/10^8,
                        info$foreign_BO/info$budget,
                        (info$domestic_BO/10^8 + info$foreign_BO/10^8),
                        (info$domestic_BO + info$foreign_BO)/info$budget
                        )

    # table <- data.table(v,w,x,y,z,info$ow_gross/10^8)

    info$investment_index <- apply(table,1, geometric.mean,na.rm = T) 
    
    
    
    info$budget -> a
    info$foreign_BO -> b
    info$domestic_BO -> c
    info$ow_gross -> d
    
    
    
    info$budget <- NULL
    info$foreign_BO <- NULL
    info$domestic_BO <- NULL
    info$ow_gross <- NULL
    
    
    # weeks_in_theater <- function(vector){length(na.omit(vector))}
    # theater_run <- apply(select(df.rank, c(2:16)),1,weeks_in_theater)

    # bom <- data.table(bom_score, info, "theater_run_score" = theater_run/15)
    bom <- data.table(bom_score, info)
    
    # bom$bo_score <- apply(bom,1, weighted.mean,c(0.15,0.15,0.15,0.05,0.05,0.1,0.1,0.1,0.15,1), na.rm = T)
    bom$bo_score <- apply(bom,1, weighted.mean,c(0.1,0.1,0.1,0.1,0.1,0.5), na.rm = T)
    
    
    bom$combined_BO <- v
    bom$word_to_mouth <- w
    bom$ratio.ow_budget <- x
    bom$ratio.www_budget <- y
    bom$theater_run_score <- z
    
    bom$budget <- a
    bom$foreign_BO <- b
    bom$domestic_BO <- c
    bom$ow_gross <- d
    
    bom$ratio.domestic_budget <- bom$domestic_BO/bom$budget
    bom$ratio.foreign_budget <- bom$foreign_BO/bom$budget
    bom$ratio.worldwide_budget <- (bom$domestic_BO + bom$foreign_BO)/bom$budget
    
    

    bom$studio <- df.info$distributor
    bom$director <- df.info$director

    df.avg$score <- NULL
    df.change$score <- NULL
    df.rank$score <- NULL

    original <- merge.data.frame(x = df.info, y = df.rank, by.y = "title")
    original <- merge.data.frame(x = original, y = df.avg, by.y = "title")
    original <- merge.data.frame(x = original, y = df.change, by.y = "title")
    original <- data.table(original) %>% arrange(title)

    rm(list = c("df.avg", "df.change",
                "df.rank","x","y",
                "info","bom_score",
                "df.info","weeks_in_theater",
                # "theater_run",
                "df.2.1","df.2.2","df.1","colum.group.1","df.rank.bis","rank.score",
                "colum.group.2.1","colum.group.2.2","cumulative.change",
                "normalize.rank","normalize.change","columns"))

    return(list("processed" = bom, "raw" = original))
    # return(bom)
    
  } -> results ; rm(i)
  
  names(results) <- names
 
  setwd('../App')
  return(results)
}








getCriticsData <- function(names){
  
  setwd('../R')
  
  foreach( i = 1:length(names)) %do% {
    
    omdb = read.csv(paste0(names[i],"_omdb.csv"),
                    na.strings = "N/A", 
                    stringsAsFactors = F,
                    header = TRUE)
    
    omdb = omdb[ , order(names(omdb))]
    omdb = data.table(omdb,keep.rownames = FALSE)
    setnames(omdb, "Title", "title")
    
    # omdb$title -> titles
    
    if (names[i] %in% "dc"){
      omdb$RT_perc[which(is.na(omdb$RT_perc))] <- 27
      
    }
    
    if (names[i] %in% "marvel"){
      omdb$RT_consensus[which(is.na(omdb$RT_consensus) & (omdb$title %in% c("Fantastic Four: Rise of the Silver Surfer")))] <- "While an improvement on its predecessor, Fantastic Four: Rise of the Silver Surfer is nevertheless a juvenile, simplistic picture that has little benefit beyond its special effects."
      omdb$metascore[which(is.na(omdb$metascore) & (omdb$title %in% c("Fantastic Four: Rise of the Silver Surfer")))] <- 45
      omdb$RT_audience_perc[which(is.na(omdb$RT_audience_perc))] <- 51
      omdb$RT_audience_rating[which(is.na(omdb$RT_audience_rating))] <- 3.1
      omdb$RT_perc[which(is.na(omdb$RT_perc))] <- 37
      omdb$RT_rating[which(is.na(omdb$RT_rating))] <- 4.8
    }
    
    if (names[i] %in% "others"){
      omdb$metascore[which(is.na(omdb$metascore) & (omdb$title %in% c("Whiteout")))] <- 28
      # omdb$metascore[which(is.na(omdb$metascore) & (omdb$title %in% c("The Rocketeer")))] <- 62
      omdb$RT_audience_perc[which(is.na(omdb$RT_audience_perc) & (omdb$title %in% c("The Incredibles")))] <- 75
      omdb$RT_audience_rating[which(is.na(omdb$RT_audience_rating) & (omdb$title %in% c("The Incredibles")))] <- 3.4
      omdb$RT_perc[which(is.na(omdb$RT_perc) & (omdb$title %in% c("The Incredibles")))] <- 97
      omdb$RT_rating[which(is.na(omdb$RT_rating) & (omdb$title %in% c("The Incredibles")))] <- 8.3
      omdb$RT_consensus[which(is.na(omdb$RT_consensus) & (omdb$title %in% c("The Incredibles")))] <- "Bringing loads of wits and tons of fun to the animated superhero genre, The Incredibleseasily lives up to its name."
      
      
    }
    
    
    omdb_score <- omdb %>% select(c(3,4,9,10,12,13))
    cols <- names(omdb_score)
    omdb_score <- transpose(data.frame(apply(omdb_score, 1, as.character, na.rm = T)))
    omdb_score <- data.frame(apply(omdb_score, 1, as.numeric, na.rm = T))
    omdb_score <- transpose(omdb_score)
    names(omdb_score) <- cols; rm(cols)
    # omdb_score$title <- titles
    omdb_score <- data.table(omdb_score)
    
    
    # if (names[i] %in% "dc"){
    #   omdb_score$RT_perc[which(is.na(omdb_score$RT_perc))] <- 27
    #   
    # }
    # 
    # if (names[i] %in% "marvel"){
    #   omdb_score$metascore[which(is.na(omdb_score$metascore) & (omdb_score$title %in% c("Fantastic Four: Rise of the Silver Surfer")))] <- 45
    #   omdb_score$RT_audience_perc[which(is.na(omdb_score$RT_audience_perc))] <- 51
    #   omdb_score$RT_audience_rating[which(is.na(omdb_score$RT_audience_rating))] <- 3.1
    #   omdb_score$RT_perc[which(is.na(omdb_score$RT_perc))] <- 37
    #   omdb_score$RT_rating[which(is.na(omdb_score$RT_rating))] <- 4.8
    # }
    # 
    # if (names[i] %in% "others"){
    #   omdb_score$metascore[which(is.na(omdb_score$metascore) & (omdb_score$title %in% c("Whiteout")))] <- 28
    #   omdb_score$metascore[which(is.na(omdb_score$metascore) & (omdb_score$title %in% c("The Rocketeer")))] <- 62
    #  
    #   
    # }
    
    # omdb_score$title <- NULL
    
    omdb_score[, RT_score := 100*(RT_rating*RT_perc/1000)]
    omdb_score[, imdb_rating := 10*(imdb_rating)]
    omdb_score[, RT_audience_score := 100*(RT_audience_rating*RT_audience_perc/500)]
    
    omdb_score <- omdb_score%>% select(c(1,2,7,8))
    omdb_score$critics_score <- apply(omdb_score,1,weighted.mean,c(0.1,0.4,0.4,0.1),na.rm = T)
    
    info <- data.frame("plot" = omdb$plot,
                       "release_date" = omdb$release_date,
                       "poster" = omdb$poster,
                       "title" = omdb$title)
    
    critics <- data.table(omdb_score, info)
    
    
    rm(omdb_score)
    rm(info)
    
    omdb <- omdb%>% arrange(title)
    return(list("processed"= critics, "raw" = omdb))
    
  } -> results; rm(list=c("i","critics","omdb"))
  
  names(results) <- names
  setwd('../App')
  return(results)
  
}





getData <- function(names){
  
  BO <- getBoxOfficeData(names)
  Critics <- getCriticsData(names)
 
  dc <- data.table(BO$dc$processed, Critics$dc$processed)
  # return(dc)
  # dc$combined_score <- apply(select(dc,c(11,22)),1,weighted.mean, c(0.4,0.6),na.rm =T)
  dc$combined_score <- apply(select(dc,which(names(dc)%in% c("bo_score","critics_score"))),
                             1,weighted.mean, c(0.4,0.6),na.rm =T)
  dc$IP <- rep("DC", dim(dc)[1])
  
  
  marvel <- data.table(BO$marvel$processed, Critics$marvel$processed)
  # marvel$combined_score <- apply(select(marvel,c(11,22)),1,weighted.mean, c(0.4,0.6), na.rm =T)
  marvel$combined_score <- apply(select(marvel,which(names(marvel)%in% c("bo_score","critics_score"))),
                                 1,weighted.mean, c(0.4,0.6),na.rm =T)
  marvel$IP <- rep("Marvel", dim(marvel)[1])
  
  others <- data.table(BO$others$processed, Critics$others$processed)
  # others <- others %>% filter(title != "Buffy The Vampire Slayer")
  # marvel$combined_score <- apply(select(marvel,c(11,22)),1,weighted.mean, c(0.4,0.6), na.rm =T)
  others$combined_score <- apply(select(others,which(names(others)%in% c("bo_score","critics_score"))),
                                 1,weighted.mean, c(0.4,0.6),na.rm =T)
  others$IP <- rep("Other", dim(others)[1])

  movies <- data.table(rbind(dc,marvel,others))
  movies$plot <- as.character(movies$plot)
  movies$release_date <- as.character(movies$release_date)
  movies$poster <- as.character(movies$poster)
  movies$title <- as.character(movies$title)

  get_year <- function(x){
    foreach( i = 1:length(x), .combine = c) %do% {
      if (is.na(x[i])) {
        return(x[i])
      }
      else{
        return(strsplit(x[i], " ")[[1]][3])
      }

    } -> result; rm(i)

    return(result)
  }

  get_month <- function(x){
    foreach( i = 1:length(x), .combine = c) %do% {
      if (is.na(x[i])) {
        return(x[i])
      }
      else{
        return(strsplit(x[i], " ")[[1]][2])
      }

    } -> result; rm(i)

    return(result)
  }

  get_day <- function(x){
    foreach( i = 1:length(x), .combine = c) %do% {
      if (is.na(x[i])) {
        return(x[i])
      }
      else{
        return(strsplit(x[i], " ")[[1]][1])
      }

    } -> result; rm(i)

    return(result)
  }

  # movies$release_day <- get_day(movies$release_date)
  movies$release_month <- get_month(movies$release_date)
  movies$release_year <- as.integer(get_year(movies$release_date))
  movies$release_period <- rep("", dim(movies)[1])
  movies$release_period[which(movies$release_year < 1998)] <- "First Wave"
  movies$release_period[which(movies$release_year >= 1998 & movies$release_year < 2008)] <- "Second Wave"
  movies$release_period[which(movies$release_year >= 2008 & movies$release_year < 2012)] <- "Third Wave"
  movies$release_period[which(movies$release_year >= 2012)] <- "Fourth Wave"

  rm(list=c("get_day","get_year","get_month"))

  movies$class <- cut(movies$combined_score,
                      breaks = 11,
                      labels = c("F*** You!",
                                 "F",
                                 "E",
                                 "D-",
                                 "D",
                                 "C-",
                                 "C",
                                 "B",
                                 "B+",
                                 "A",
                                 "A+"),
                      ordered_result = T
                      )

  # movies$class <- as.character(movies$class)

  movies$studio <- as.character(movies$studio)
  movies$studio[which(movies$studio == "Buena Vista")] <- "Disney"
  movies$studio[which(movies$studio == "Sony / Columbia")] <- "Sony"
  movies$studio[which(movies$studio == "Sony Classics")] <- "Sony"
  movies$studio[which(movies$studio == "Sony (Revolution)"  )] <- "Sony"
  movies$studio[which(movies$studio == "Lionsgate/Summit")] <- "Lionsgate"
  movies$studio[which(movies$studio == "Lions Gate")] <- "Lionsgate"
  movies$studio[which(movies$studio == "Weinstein / Dimension")] <- "Weinstein"
  



  movies <- movies%>%arrange(desc(combined_score))
  movies$overall_rank <- seq(1,dim(movies)[1])
  

  fix.director.names <- function(vector){
    x <- na.omit(vector)
    foreach(i = 1:length(x), .combine = c) %do% {
      strsplit(x[i],"'")[[1]][2]
    } -> fixed.names

    vector[which(is.na(vector) == F)] <- fixed.names
    return(vector)
  }

  movies$director <- fix.director.names(movies$director)

  ff4.revised.plot <- c("Scientist Reed Richards (Loan Gruffudd) persuades his arrogant former classmate Victor von Doom (Julian McMahon), to fund his experiments with cosmic energy. On von Doom's space station, the crew - including astronaut Ben Grimm (Michael Chiklis), researcher Sue Storm (Jessica Alba) and pilot Johnny Storm (Chris Evans) - are exposed to a mysterious cosmic storm that bestows super powers upon them. As they cope with their transfortmations, von Doom vows his revenge.")
  ff4.2.plot <- c("Reed (Ioan Gruffudd), Susan (Jessica Alba), Johnny (Chris Evans) and Ben (Michael Chiklis) face an intergalactic messenger who has arrived to prepare Earth for its destruction. While the enigmatic being wreaks havoc around the world, the heroic quartet must also contend with the unexpected return of their enemy, Victor Von Doom.")
  thor2.revised.plot <- c("In ancient times, the gods of Asgard fought and won a war against an evil race known as the Dark Elves. The survivors were neutralized, and their ultimate weapon -- the Aether -- was buried in a secret location. Hundreds of years later, Jane Foster (Natalie Portman) finds the Aether and becomes its host, forcing Thor (Chris Hemsworth) to bring her to Asgard before Dark Elf Malekith (Christopher Eccleston) captures her and uses the weapon to destroy the Nine Realms -- including Earth.")
  xmen_dofp.revised.plot <- c("Convinced that mutants pose a threat to humanity, Dr. Bolivar Trask (Peter Dinklage) develops the Sentinels, enormous robotic weapons that can detect a mutant gene and zero in on that person. In the 21st century, the Sentinels have evolved into highly efficient killing machines. With mutants now facing extinction, Wolverine (Hugh Jackman) volunteers to go back in time and rally the X-Men of the past to help change a pivotal moment in history and thereby save their future.")
  dredd.revised.plot <- c("Mega City One is a vast, violent metropolis where felons rule the streets. The only law lies with cops called 'judges', who act as judge, jury and executioner, and Dredd (Karl Urban) is one of the city's most feared. One day, Dredd is partnered with Cassandra (Olivia Thirlby), a rookie with powerful psychic abilities. A report of a terrible crime sends Dredd and Cassandra to a dangerous area controlled by Ma-Ma (Lena Headey), a drug lord who will stop at nothing to protect her empire.")
  tmnt2016.revised.plot <- c("The turtles face a new challenge when Shredder escapes from custody and joins forces with Baxter Stockman, a mad scientist who plans to use a serum to take over the world. Along for the ride are Bebop and Rocksteady, two dimwitted henchmen who provide plenty of muscle. Luckily, the turtles have their own allies in April O'Neil, Vernon Fenwick and Casey Jones, a hockey-masked vigilante. As the pizza-loving heroes prepare for battle, the notorious Krang also emerges to pose an even greater threat.")
  kickass2.revised.plot <- c("Dave (Aaron Taylor-Johnson), aka Kick-Ass, and Mindy (Chloë Grace Moretz), aka Hit Girl, are trying to live as normal teenagers and briefly form a crime-fighting team. After Mindy is busted and forced to retire as Hit Girl, Dave joins a group of amateur superheroes led by Col. Stars and Stripes (Jim Carrey), a reformed mobster. Just as Dave and company start to make a real difference on the streets, the villain formerly known as Red Mist (Christopher Mintz-Plasse) rears his head yet again.")
 
  movies <- data.table(movies)
  
  movies[which(movies$title %in% "Fantastic Four(2005)")]$plot <- ff4.revised.plot
  movies[which(movies$title %in% "Fantastic Four: Rise of the Silver Surfer")]$plot <- ff4.2.plot
  movies[which(movies$title %in% "Thor 2: The Dark World")]$plot <- thor2.revised.plot
  movies[which(movies$title %in% "X-Men: Days of Future Past")]$plot <- xmen_dofp.revised.plot
  movies[which(movies$title %in% "Dredd")]$plot <- dredd.revised.plot
  movies[which(movies$title %in% "Kick-Ass 2")]$plot <- kickass2.revised.plot
  movies[which(movies$title %in% "Teenage Mutant Ninja Turtles: Out of the Shadows")]$plot <- tmnt2016.revised.plot
  
 
  
  rm(list =c("ff4.revised.plot","ff4.2.plot",
             "thor2.revised.plot",
             "xmen_dofp.revised.plot","dredd.revised.plot","kickass2.revised.plot","tmnt2016.revised.plot"
             )
     )

  return(list("df" = movies%>%arrange(desc(combined_score)), "BO" = BO, "Critics" = Critics))
  # return(list("dc" = dc, "marvel" = marvel))
}







Viz <- function(df){
  
  df <- data.table(df)
  df[, mapping_size:= critics_score * bo_score]
  plot <- df %>% plot_ly(x = bo_score,
                         y = critics_score,
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
  
  
  
  # Get the list for the plot
  plot <- plotly_build(plot)
  
  foreach(i =1:length(plot$data)) %do% {
    
    # Pick up the hover text
    hvrtext <- plot$data[[i]]$text
    # Split by line break and wt
    hvrtext_fixed <- strsplit(hvrtext, split = '<br>mapping_size')
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
      
      poster.pic.name <- function(vector){
        foreach(i =1:length(vector),.combine = c)%do%{
          name <- paste0(gsub(" ","", vector[i]),".png")
          name <- gsub(":","", name)
          return(name)
        } -> result ; rm(i)
        
        return(result)
      }
      
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
      
      poster.pic.name <- function(vector){
        foreach(i =1:length(vector),.combine = c)%do%{
          name <- paste0(gsub(" ","", vector[i]),".png")
          name <- gsub(":","", name)
          return(name)
        } -> result ; rm(i)
        
        return(result)
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
      
      poster.pic.name <- function(vector){
        foreach(i =1:length(vector),.combine = c)%do%{
          name <- paste0(gsub(" ","", vector[i]),".png")
          name <- gsub(":","", name)
          return(name)
          
        } -> result ; rm(i)
        
        return(result)
      }
      
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






poster.pic.name <- function(vector){
  foreach(i =1:length(vector),.combine = c)%do%{
    name <- paste0(gsub(" ","", vector[i]),".png")
    name <- gsub(":","", name)
    return(name)
  } -> result ; rm(i)
  
  # wd <- getwd()
  # setwd("./www")
  # foreach(i=1:length(posters)) %do% {
  #   GET(test$poster[i], write_disk(posters[i]))
  # };rm(i)
  # setwd(wd)
  
  return(result)
}





find_movie <- function(titles, df,List){
  
  data <- df %>% filter(title %in% titles)
  if (dim(data)[1] == 0){
    return(data.table("Empty" = "yes",
                      "movie_report"= "N/A", 
                      "poster" = "NoPosterAvailable.png"
                      )
           )
  }else{
    
    critics.raw <- data.table(rbind(List$Critics$dc$raw, List$Critics$marvel$raw,List$Critics$others$raw))
    critics.raw <- critics.raw%>%filter(title %in% data$title)
    
    bo.raw <- data.frame(rbind(List$BO$dc$raw, List$BO$marvel$raw,List$BO$others$raw))
    bo.raw <- bo.raw %>% filter(title %in% critics.raw$title)
    
    raw.list <- list("critics"=critics.raw, "BO" = bo.raw)
    
    report <- report.text(list("processed.data" = data,"raw.data" = raw.list ))
    report <- report$processed.data 
    report <- report %>% select(which(names(report) %in% c("title","movie_report")))
    
    posters <- foreach(i =1 :dim(report)[1], .combine = c) %do% {
      poster.pic.name(report$title[i])
    }; rm(i)
    
    
    return(data.table(report, "posters" = posters))
    
  }
  
}