source('functions.R')
libraries()

database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM boMetrics')), imdbID)
# data <- setnames(data[,c(4,2,3,5,6)], c("imdbRating","score") ,c("IMDB", "Critical Reception"))
# data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data[,IMDB:= 10*IMDB], nomatch = 0]
# data <- setkey(data[, imdbID := NULL ], title)[title %in% c("Man of Steel", "Logan")]
# data <- setnames(data[title %in% c("Man of Steel", "Logan")], c("budget","domestic_BO","foreign_BO"), c("Budget","Domestic","Foreign"))
# data[, c("Budget","Domestic","Foreign") := list(as.numeric(Budget), as.numeric(Domestic), as.numeric(Foreign))]
# data <- melt(data, id.vars = "title", variable.name ="index", value.name = "value")
# data[, title := factor(title, levels = c("Man of Steel", "Logan"), ordered = T)]

dbDisconnect(database); rm(list = c("database"))

# data%>%
#   ggplot(aes(source, value, fill = title)) +
#   geom_bar(position="dodge", width = 0.5, stat="identity") +
#   scale_y_continuous(labels = scales::dollar) +
#   scale_x_discrete() +
#   ggtitle("Critics Performance")+
#   scale_fill_manual(values=c("#999999", "#E69F00"))+
#   geom_text(
#     aes(label=paste0(value,"%")),
#     vjust= -0.8,
#     color="black", position=position_dodge(.5), size = 3,fontface = "bold") +
#   theme_classic() +
#   theme(
# 
#     axis.title.x = element_blank(),
#     axis.line.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.text.x = element_text(color ="black", face = "bold", size = 10),
# 
#     axis.title.y = element_blank(),
#     axis.line.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank(),
# 
#     legend.position="none"
#   ) -> p


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