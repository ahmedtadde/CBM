source('functions.R')
libraries()

database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_ranks')), imdbID)[, ID := NULL]
data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0]
data <- setkey(data[, imdbID := NULL ], title)[title %in% c("Man of Steel", "Logan"), c(1,8:16,2:7)]
data <- data[, lapply(.SD, as.numeric), .SDcols = grepl("week_", names(data), fixed = T) ][, title:= data$title]
data <- melt(data, id.vars = "title", variable.name = "week", value.name = "rank")
data <- data[, week:= foreach(k=1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[k], "_")[[1]][2]))}]
data <- data[, title := factor(title, levels = c("Man of Steel", "Logan"), ordered = T)][!is.na(rank)]
dbDisconnect(database); rm(list = c("database"))

data%>%
  ggplot(aes(week, rank, color = title)) +
  geom_point() + geom_line()+
  ggtitle("Weekly Ranking")+
  scale_y_reverse()+
  scale_x_continuous(limits = c(1,15), breaks = c(1:15))+
  scale_color_manual(values=c("red", "blue"))+
  geom_text(
    aes(label= rank),
    vjust= -0.8,
    color="black", size = 2,fontface = "bold") +
  theme_classic() +
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
  ) -> p


# # headerhtml <- '<!DOCTYPE HTML>
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