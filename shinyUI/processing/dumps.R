
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


# rankingViz <- function(data){
#   
#   df <- copy(data)[, mapping_size:= critical_score * bo_score]
#   
#   plot <- df %>% plot_ly(x = ~bo_score,
#                          y = ~critical_score,
#                          width = 1400,
#                          height = 650,
#                          type = "scatter",
#                          mode = "markers",
#                          size = ~mapping_size,
#                          color = ~grade,
#                          colors = "RdYlGn",
#                          opacity = ~mapping_size
#                          # text = paste(
#                          #   # toupper(~title),"<br>"
#                          #    # "Grade: ", toupper(~grade), "<br>",
#                          #    # "Overall Critical Reception: ", round(~critical_score,2),"%" ,"<br>",
#                          #    # "Box Office Performance Index: ", round(bo_score,2),"<br>",
#                          #    # "All-time Ranking: ", ~ranking
#                          #  ),
#                          # hoverinfo = "text"
#                          
#   ) %>% layout(
#     
#     
#     title = "Ranking Visulation (hover on bubble for movie info)",
#     titlefont = list(size = 15),
#     legend = list(font = list(size = 15), x = 0.9,y = 0.1),
#     # autosize = T, 
#     # width = 1400, 
#     # height = 650, 
#     # paper_bgcolor='rgba(0,0,0,0)',
#     # plot_bgcolor='rgba(0,0,0,0)',
#     # margin = m,
#     xaxis = list(title = "Overall Box Office Performance Index (higher is better)",
#                  showgrid = F,
#                  zeroline = FALSE,
#                  showline = FALSE,
#                  showticklabels = F
#                  # tickfont = list(size = 15),
#                  # titlefont = list(size = 15)
#                  # showgrid = F,
#                  # tickcolor = toRGB("white"),
#                  # tickfont = list(color = toRGB("white"), size = 10),
#                  # zeroline = FALSE,
#                  # showline = FALSE,
#                  # showticklabels = T
#     ),
#     yaxis = list(title = "Overall Critical Reception (in %)",
#                  showgrid = F,
#                  zeroline = FALSE,
#                  showline = FALSE,
#                  showticklabels = F
#                  # tickfont = list(size = 15),
#                  # titlefont= list(size = 15)
#                  # showgrid = F,
#                  # tickcolor = toRGB("white"),
#                  # tickfont = list(color = toRGB("white"), size = 10),
#                  # zeroline = FALSE,
#                  # showline = FALSE,
#                  # showticklabels = T
#     )
#   )
#   
#   
#   
#   # #Get the list for the plot
#   # plot <- plotly_build(plot)
#   # 
#   # foreach(i =1:length(plot$data)) %do% {
#   # 
#   #   # Pick up the hover text
#   #   hvrtext <- plot$data[[i]]$text
#   #   # Split by line break and wt
#   #   hvrtext_fixed <- stri_split(hvrtext, fixed = '<br>mapping_size')
#   #   # Get the first element of each split
#   #   hvrtext_fixed <- lapply(hvrtext_fixed, function(x) x[1])
#   #   # Convert back to vector
#   #   hvrtext_fixed <- as.character(hvrtext_fixed)
#   #   # Assign as hovertext in the plot
#   #   plot$data[[i]]$text <- hvrtext_fixed
#   # 
#   # 
#   # }
#   
#   # rm(list=c("hvrtext","hvrtext_fixed","df"))
#   # 
#   return(plot)
#   
# }