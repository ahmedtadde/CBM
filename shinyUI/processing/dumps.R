




# movies$class <- cut(movies$combined_score,
#                     breaks = 11,
#                     labels = c("F*** You!",
#                                "F",
#                                "E",
#                                "D-",
#                                "D",
#                                "C-",
#                                "C",
#                                "B",
#                                "B+",
#                                "A",
#                                "A+"),
#                     ordered_result = T
# )
# 
# # movies$class <- as.character(movies$class)
# 
# movies$studio <- as.character(movies$studio)
# movies$studio[which(movies$studio == "Buena Vista")] <- "Disney"
# movies$studio[which(movies$studio == "Sony / Columbia")] <- "Sony"
# movies$studio[which(movies$studio == "Sony Classics")] <- "Sony"
# movies$studio[which(movies$studio == "Sony (Revolution)"  )] <- "Sony"
# movies$studio[which(movies$studio == "Lionsgate/Summit")] <- "Lionsgate"
# movies$studio[which(movies$studio == "Lions Gate")] <- "Lionsgate"
# movies$studio[which(movies$studio == "Weinstein / Dimension")] <- "Weinstein"
# 
# 
# 
# movies[which(movies$title %in% "Fantastic Four(2005)")]$plot <- revision[[1]]
# movies[which(movies$title %in% "Fantastic Four: Rise of the Silver Surfer")]$plot <- revision[[2]]
# movies[which(movies$title %in% "Thor: The Dark World")]$plot <- revision[[3]]
# movies[which(movies$title %in% "X-Men: Days of Future Past")]$plot <- revision[[4]]
# movies[which(movies$title %in% "Dredd")]$plot <- revision[[5]]
# movies[which(movies$title %in% "Teenage Mutant Ninja Turtles: Out of the Shadows")]$plot <- revision[[6]]
# movies[which(movies$title %in% "Kick-Ass 2")]$plot <- revision[[7]]
# movies[which(movies$title %in% "Blade: Trinity")]$plot <- revision[[8]]
# movies[which(movies$title %in% "Spider-Man")]$plot <- revision[[9]]
# movies[which(movies$title %in% "Batman Forever")]$plot <- revision[[10]]
# movies[which(movies$title %in% "The Mask")]$plot <- revision[[11]]
# movies[which(movies$title %in% "RIPD")]$plot <- revision[[12]]
# 
# 
# 
# 
# # tabPanel("Search Movie",
#          tags$br(),tags$br(),tags$br(),
#          fluidRow(
#                   column(4),
#                   column(4,
#                          selectizeInput('find.movie', 
#                                           h3('Search Movie'),
#                                           choices = movie.titles,
#                                           multiple = T,
#                                           selected = ""
#                                           )
#                          ),
#                   column(4)
#                   ),
#          fluidRow(column(1),
#                   column(4, align= 'center', htmlOutput("find.movie.poster")),
#                   column(6, htmlOutput("find.movie.report")),
#                   column(1)
#                   )
#          ),


