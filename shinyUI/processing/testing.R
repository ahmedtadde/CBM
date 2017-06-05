source('functions.R')
libraries()

database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
ranks <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_ranks')), imdbID)
summary <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)
data <- summary[ranks, nomatch=0][title %in% c("The Dark Knight", "Logan")][,c(2,10:18,4:9)]
data <- melt(data, id.vars = "title", variable.name ="week", value.name = "rank")
data[, week:= foreach(i= 1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[i], "_")[[1]][2]))}]


dbDisconnect(database); rm(list = c("database","ranks","summary"))

plot_ly(data,
        x = ~week,
        y = ~rank,
        color = ~title,
        colors = c("The Dark Knight"="red","Logan"="blue"),
        type = 'scatter', mode = 'lines+markers'
        ) %>% layout(yaxis = list(autorange = "reversed")) -> p


# headerhtml <- '<!DOCTYPE HTML>
# <link rel="stylesheet" type ="text/css" href="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.css">'

# scripts <- '<script src="https://code.jquery.com/jquery-3.1.1.min.js"
# integrity="sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8="
# crossorigin="anonymous"></script>
# <script src="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.js"></script>'
# 
# logos <- c("dc.png","marvel.png","other.png")
# studiocolors <- c("#0282f9","#ed1717","#c9a318")
# studios <- c("Warner Bros.", "Disney","Warner Bros.")
# movies <- c("BatmanBegins.png", "Antman.png","300.png")
# result = c()
# 
# for (k in c(1:3)){
#   result = paste0(result, "\n",
#                   shiny::htmlTemplate("./www/PosterTemplate.html",
#                                       IPlogo = logos[k],
#                                       studiocolor = studiocolors[k],
#                                       studio = studios[k], 
#                                       postername = movies[k]
#                                       )
#                   )
# }
# 
# rm(k)
# result <- paste0(headerhtml, "\n", result,'\n',scripts)
# 
# rendr.rank.poster <- function(data){
#   result <- ""
#   result <- paste0(result, "\n",
#                   shiny::htmlTemplate("PosterTemplate.html",
#                                       logo = ipLogos[[data$tag]],
#                                       color = ipColors[[data$tag]],
#                                       studio = data$studio, 
#                                       poster = data$Poster
#                   )
#   )
#   
# }