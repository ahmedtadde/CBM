
ip <- checkboxGroupInput("ip",
                         h4("Comicbook Company"),
                         choices = list("DC", "MARVEL", "OTHER"),
                         inline =TRUE)

studio <- selectizeInput('studio',
                         h4('Studio'),
                         choices = studios,
                         multiple = TRUE,
                         selected = NULL
                        )

years <- sliderInput("years",
                     h4("Years"),
                     min = 1978,
                     max = 2017,
                     value = c(2010, 2017),
                     step=1, sep = ""
                    ) 

month <- selectizeInput('months',
                         h4('Months'),
                         choices = months,
                         selected = NULL,
                         multiple = TRUE,
                         options = list(maxItems = 6)
                        )

runtime <- selectizeInput('runtime',
                           h4('Runtime'),
                           choices = names(uiInputOptions$runtime),
                           selected = NULL,
                           multiple = TRUE,
                           options = list(maxItems = 3)
                          )

mpaa <- checkboxGroupInput("mpaa",
                           h4("MPAA Rating"),
                           choices = list("PG", "PG-13", "R"),
                           inline =TRUE)

metascore <- sliderInput("metascore",
                         h4("Metascore"),
                         min = 0,
                         max = 100,
                         value = c(60,100)
                        )


rt <- sliderInput("rt",
                  h4("Rotten Tomatoes"),
                   min = 0,
                   max = 100,
                   value = c(70,100)
                  )

imdb <- sliderInput("imdb",
                    h4("IMDB"),
                     min = 0,
                     max = 10,
                     value = c(6,10)
                    )


rank.metrics <- selectizeInput('rankBy',
                                h4('Rank By'),
                                selected = "Overal Score",
                                choices = names(uiInputOptions$rankMetrics),
                                options = list(maxItems = 3),
                                multiple = T
                              )


first.movie <- selectizeInput('first.movie', 
                              h4('Select Movie'), 
                              selected = "The Dark Knight",
                              choices = all.time.ranking.data$title,
                              multiple = F
                              )

second.movie <- selectizeInput('second.movie', 
                              h4('Select Movie'), 
                              selected = "Logan",
                              choices = all.time.ranking.data$title,
                              multiple = F
                              )
shinyUI(
  
  navbarPage(
    title = "Comics Adaptation",
    tags$header(
      tags$link(rel="stylesheet", type="text/css", href="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.css"),
      tags$script(src="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.js"),
      tags$script(
        src="https://code.jquery.com/jquery-3.2.1.slim.js",
        integrity="sha256-tA8y0XqiwnpwmOIl3SGAcFl2RvxHjA8qp0+1uCGmRmg=",
        crossorigin="anonymous"),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/axios/0.15.3/axios.min.js"),
      tags$script(src="index.js")
    ),
    inverse = TRUE,
    selected = "Filter and Rank",
    tabPanel("Filter and Rank",
             htmlTemplate("filterRank.html",
                          ipInput = ip,
                          studioInput = studio,
                          yearInput = years,
                          monthInput = month,
                          runtimeInput = runtime,
                          mpaaInput = mpaa,
                          metascoreInput = metascore,
                          rtInput = rt,
                          imdbInput = imdb,
                          rankInput = rank.metrics
                        )
            ),
    tabPanel("Compare", htmlTemplate("compare.html", first.movie  = first.movie, second.movie = second.movie)),
    navbarMenu("All-Time Ranking",
              tabPanel("Table",htmlTemplate("rankingTable.html")),
              tabPanel("About",htmlTemplate("about.html"))
              )
  )
)



# htmlTemplate("index.html",
#              ipInput = ip,
#              studioInput = studio,
#              yearInput = years,
#              monthInput = month,
#              runtimeInput = runtime,
#              mpaaInput = mpaa,
#              metascoreInput = metascore,
#              rtInput = rt,
#              imdbInput = imdb,
#              rankInput = rank.metrics
#             )



# shinyUI(
#   navbarPage( 
#     
#               # tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
#               # tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
#               # tags$head(tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Architects+Daughter|Ubuntu")),
# 
#               theme = "/bootstrap.css",
#               title =  "Comic Book Adaptations",
#               
#               
#               tabPanel("Filter and Rank",
#                        fluidRow(
# 
#                          column(4,selectizeInput('ip',
#                                                  h4('Comic Book Company'),
#                                                  choices =  names(uiInputOptions$ip),
#                                                  multiple = F,
#                                                  selected = "All"
#                                                  # options = list(maxItems = 3)
#                                                  )
#                                 ),
# 
#                          column(4,selectizeInput('studio',
#                                                  h4('Studio'),
#                                                  choices = uiInputOptions$studio,
#                                                  multiple = FALSE,
#                                                  selected = "All"
#                                                  )),
# 
#                          column(4, sliderInput("years",
#                                                label = h4("Years"),
#                                                min = 1978,
#                                                max = 2016,
#                                                value = c(2010, 2016),
#                                                step=1, sep = ""
#                                                ))
#                          ),
#                        
#                        fluidRow(
# 
#                          column(4,selectizeInput('months',
#                                                  h4('Months'),
#                                                  choices = uiInputOptions$month,
#                                                  selected = "All",
#                                                  multiple = FALSE
#                                                  )
#                                 ),
# 
#                          column(4,selectizeInput('runtime',
#                                                  h4('Runtime'),
# 
#                                                  choices = names(uiInputOptions$runtime),
#                                                  selected = "All",
#                                                  multiple = FALSE)),
# 
#                          column(4,selectizeInput('mpaa',
#                                                  h4('MPAA Rating'),
#                                                  choices = names(uiInputOptions$mpaa),
#                                                  selected = "All",
#                                                  multiple = FALSE
#                                                  ))
#                          ),
#                        
#                        fluidRow(
#                          column(4, sliderInput("metascore",
#                                                label = h4("Metascore"),
#                                                min = 0,
#                                                max = 100,
#                                                value = c(60,100))
#                                 ),
# 
#                          column(4, sliderInput("imdb",
#                                                label = h4("IMDB Rating"),
#                                                min = 0,
#                                                max = 10,
#                                                value = c(6,10))),
# 
#                          column(4, sliderInput("rt",
#                                                label = h4("Rotten Tomatoes"),
#                                                min = 0,
#                                                max = 100,
#                                                value = c(70,100)))
#                          ),
#                        
#                         fluidRow(
#                          column(4),
#                          column(4,selectizeInput('rank.by',
#                                                  h4('Rank By'),
#                                                  selected = "",
#                                                  choices = names(uiInputOptions$rankMetrics),
#                                                  options = list(maxItems = 3),
#                                                  multiple = T)
#                                 ),
#                          column(4)
#                          ),
#                        hr(),
#                        fluidRow(
#                                 column(4, htmlOutput("posters")),
#                                 column(6, htmlOutput("reports"))
#                                 )
#                        
#               )
#               # navbarMenu("Movie Fight",
#               #            tabPanel("Critical Reception",
#               #                     fluidRow(column(1),
#               #                              column(3,
#               #                                     align = 'center',
#               #                                     selectizeInput("firstmovie.CR",
#               #                                                    h4('Select'),
#               #                                                    selected = "",
#               #                                                    choices = c("",movie.titles),
#               #                                                    multiple =F )),
#               #                              column(4),
#               #                              column(3,
#               #                                     align = 'center',
#               #                                     selectizeInput("secondmovie.CR",
#               #                                                    h4('Select'),
#               #                                                    selected = "",
#               #                                                    choices = c("",movie.titles),
#               #                                                    multiple = F)),
#               #                              column(1)
#               #                     ),
#               #                     fluidRow(column(1),
#               #                              column(3, align = 'center',htmlOutput("firstmovie.poster.CR")),
#               #                              column(4, align = 'center', htmlOutput("VERSUS.CR")),
#               #                              column(3, align = 'center',htmlOutput("secondmovie.poster.CR")),
#               #                              column(1)
#               #                     ),
#               #                     fluidRow(column(1),
#               #                              column(3, align = 'center', htmlOutput("firstmovie.meta.CR")),
#               #                              column(4),
#               #                              column(3, align = 'center',htmlOutput("secondmovie.meta.CR")),
#               #                              column(1)
#               #                     ),
#               #                     fluidRow(
#               #                              column(1),
#               #                              column(3, htmlOutput("firstmovie.critics_consensus")),
#               #                              column(4 ),
#               #                              column(3, htmlOutput("secondmovie.critics_consensus")),
#               #                              column(1)
#               #                     ),
#               #                     fluidRow(column(1),
#               #                              column(10),
#               #                              # column(10, align = "center", tags$a(h4('Critical Reception Comparison (hover on bars for info)'))),
#               #                              column(1)
#               #                     ),
#               #                     fluidRow(
#               #                       # column(1),
#               #                       column(12, align = "center", plotlyOutput("versus_critics_chart"))
#               #                       # column(1)
#               #                     ),
#               #                     fluidRow(column(1),
#               #                              column(10),
#               #                              column(1)
#               #                     )
#               #                     
#               #            ),
#               #            
#               #            tabPanel("Box Office Performance",
#               #                     fluidRow(column(1),
#               #                              column(3,
#               #                                     align = 'center',
#               #                                     selectizeInput("firstmovie.BOP",
#               #                                                    h4('Select'),
#               #                                                    selected = "",
#               #                                                    choices =  c("",movie.titles),
#               #                                                    multiple =F )),
#               #                              column(4),
#               #                              column(3, 
#               #                                     align = 'center',
#               #                                     selectizeInput("secondmovie.BOP",
#               #                                                    h4('Select'),
#               #                                                    selected = "",
#               #                                                    choices =  c("",movie.titles),
#               #                                                    multiple = F)),
#               #                              column(1)
#               #                              ),
#               #                     fluidRow(column(1),
#               #                              column(3, align = 'center',htmlOutput("firstmovie.poster.BOP")),
#               #                              column(4, align = 'center', htmlOutput("VERSUS.BOP")),
#               #                              column(3, align = 'center',htmlOutput("secondmovie.poster.BOP")),
#               #                              column(1)
#               #                              ),
#               #                     fluidRow(column(1),
#               #                              column(3, align = 'center', htmlOutput("firstmovie.meta.BOP")),
#               #                              column(4),
#               #                              column(3, align = 'center',htmlOutput("secondmovie.meta.BOP")),
#               #                              column(1)
#               #                              ),
#               #                     fluidRow(column(4,align = "center",plotlyOutput("versus.weekly.avg")),
#               #                              column(4,align = "center",plotlyOutput("versus.weekly.perc")),
#               #                              column(4,align = "center",plotlyOutput("versus.weekly.rank"))
#               #                              ),
#               #                     fluidRow(column(1),
#               #                              column(10), column(1)
#               #                              ),
#               #                     fluidRow(
#               #                              # column(6),
#               #                              column(4, align = "center", plotlyOutput("versus_BO_chart.1")),
#               #                              column(8, align = "center", plotlyOutput("versus_BO_chart.2"))
#               #                              )
#               #                     )
#               #            ),
#               # 
#               # navbarMenu("All-time Ranking",
#               #            tabPanel("Visual Classification",plotlyOutput("Viz")),
#               #            tabPanel("Ranking Table",tableOutput("combined.ranking.table"))
#               #            )
#             )
#   )
