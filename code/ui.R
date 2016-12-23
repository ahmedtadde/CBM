

movie.titles <- sort(movie.titles)

studios <- c("All","Dimension Films","Disney","DreamWorks","Fine Line","Fox",
             "Lionsgate","MGM","Miramax","New Line","Paramount","Sony",
             "Summit Entertainment","TriStar","Universal","Warner Bros.","Weinstein" )


shinyUI(
  navbarPage( 
              theme = "bootstrap.css",
              "Comic Book Adaptations",
              # tabPanel("Search Movie",
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
              
              tabPanel("Filter and Rank",
                       fluidRow(

                         column(2,selectizeInput('ip',
                                                 h4('Comic Book Company'),
                                                 choices = c("All","DC","Marvel","Other"),
                                                 multiple = T,
                                                 selected = "All"
                                                 )),

                         column(2,selectizeInput('studio',
                                                 h4('Studio'),
                                                 choices = studios,
                                                 multiple = TRUE,
                                                 selected = "All"
                                                 )),

                         column(2, sliderInput("years",
                                               label = h4("Years"),
                                               min = 1978,
                                               max = 2016,
                                               value = c(2006, 2016))),

                         column(2,selectizeInput('months',
                                                 h4('Months'),
                                                 choices = c("All",
                                                             "Jan",
                                                             "Feb",
                                                             "Mar",
                                                             "Apr",
                                                             "May",
                                                             "June",
                                                             "Jul",
                                                             "Aug",
                                                             "Sep",
                                                             "Oct",
                                                             "Nov",
                                                             "Dec"
                                                 ),
                                                 selected = "All",
                                                 multiple = TRUE)),

                         column(2,selectizeInput('runtime',
                                                 h4('Runtime'),

                                                 choices = c("All",
                                                             "Under 1h:30",
                                                             "Between 1h:30 and 2h:00",
                                                             "Between 2h:00 and 2h:30",
                                                             "Between 2h:30 and 3h:00"),
                                                 selected = "All",
                                                 multiple = TRUE)),

                         column(2,selectizeInput('rating',
                                                 h4('Rating'),
                                                 choices = c("All",
                                                             "PG",
                                                             "PG-13",
                                                             "R"),
                                                 selected = "All",
                                                 multiple = TRUE))
                         ),

                       fluidRow(
                         column(2, sliderInput("metascore",
                                               label = h4("Metascore"),
                                               min = 0,
                                               max = 100,
                                               value = c(50,100))),

                         column(2, sliderInput("imdb",
                                               label = h4("IMDB Rating"),
                                               min = 0,
                                               max = 10,
                                               value = c(5,10))),

                         column(2, sliderInput("RT_perc",
                                               label = h4("RT(Critics) Percent"),
                                               min = 0,
                                               max = 100,
                                               value = c(50,100))),

                         column(2, sliderInput("RT_rating",
                                               label = h4("RT(Critics) Rating"),
                                               min = 0,
                                               max = 10,
                                               value = c(5,10))),

                         column(2, sliderInput("RT_audience_perc",
                                               label = h4("RT(Audience) Percent"),
                                               min = 0,
                                               max = 100,
                                               value = c(50,100))),

                         column(2, sliderInput("RT_audience_rating",
                                               label = h4("RT(Audience) Rating"),
                                               min = 0,
                                               max = 5,
                                               value = c(3,5)))
                         ),
                       
                        fluidRow(
                         column(4),
                         column(4,selectizeInput('rank.by',
                                                 h4('Rank By'),
                                                 selected = "",
                                                 choices = c("Overall Critical Reception",
                                                             "Overall Box Office Performance",
                                                             "Rotten Tomatoes Critics Tomatometer",
                                                             "Rotten Tomatoes Critics Rating",
                                                             "Custom Rotten Tomatoes Critics Score",
                                                             "Rotten Tomatoes Audience Tomatometer",
                                                             "Rotten Tomatoes Audience Rating",
                                                             "Custom Rotten Tomatoes Audience Score",
                                                             "Metascore","IMDB","Foreign Box Office Gross", 
                                                             "Domestic Box Office Gross",
                                                             "Foreign & Domestic BO - Weighted Mean (30/70)",
                                                             "Weekly Average per Theater",
                                                             "Weekly Grosses as % of Opening Week",
                                                             "Weekly Ranking",""),
                                                 multiple = F)
                                ),
                         column(4)
                         ),
                       hr(),
                       fluidRow(
                                # htmlOutput("report")
                                # DT::dataTableOutput("report")
                                column(1, align='center',htmlOutput("rank.number")),
                                column(4, align= "center", htmlOutput("poster")),
                                column(6, align= "left", htmlOutput("report")),
                                column(1)
                                
                                )
                       
              ),
              navbarMenu("Movie Fight",
                         tabPanel("Critical Reception",
                                  fluidRow(column(1),
                                           column(3,
                                                  align = 'center',
                                                  selectizeInput("firstmovie.CR",
                                                                 h4('Select'),
                                                                 selected = "",
                                                                 choices = c("",movie.titles),
                                                                 multiple =F )),
                                           column(4),
                                           column(3,
                                                  align = 'center',
                                                  selectizeInput("secondmovie.CR",
                                                                 h4('Select'),
                                                                 selected = "",
                                                                 choices = c("",movie.titles),
                                                                 multiple = F)),
                                           column(1)
                                  ),
                                  fluidRow(column(1),
                                           column(3, align = 'center',htmlOutput("firstmovie.poster.CR")),
                                           column(4, align = 'center', htmlOutput("VERSUS.CR")),
                                           column(3, align = 'center',htmlOutput("secondmovie.poster.CR")),
                                           column(1)
                                  ),
                                  fluidRow(column(1),
                                           column(3, align = 'center', htmlOutput("firstmovie.meta.CR")),
                                           column(4),
                                           column(3, align = 'center',htmlOutput("secondmovie.meta.CR")),
                                           column(1)
                                  ),
                                  fluidRow(
                                           column(1),
                                           column(3, htmlOutput("firstmovie.critics_consensus")),
                                           column(4 ),
                                           column(3, htmlOutput("secondmovie.critics_consensus")),
                                           column(1)
                                  ),
                                  fluidRow(column(1),
                                           column(10),
                                           # column(10, align = "center", tags$a(h4('Critical Reception Comparison (hover on bars for info)'))),
                                           column(1)
                                  ),
                                  fluidRow(
                                    # column(1),
                                    column(12, align = "center", plotlyOutput("versus_critics_chart"))
                                    # column(1)
                                  ),
                                  fluidRow(column(1),
                                           column(10),
                                           column(1)
                                  )
                                  
                         ),
                         
                         tabPanel("Box Office Performance",
                                  fluidRow(column(1),
                                           column(3,
                                                  align = 'center',
                                                  selectizeInput("firstmovie.BOP",
                                                                 h4('Select'),
                                                                 selected = "",
                                                                 choices =  c("",movie.titles),
                                                                 multiple =F )),
                                           column(4),
                                           column(3, 
                                                  align = 'center',
                                                  selectizeInput("secondmovie.BOP",
                                                                 h4('Select'),
                                                                 selected = "",
                                                                 choices =  c("",movie.titles),
                                                                 multiple = F)),
                                           column(1)
                                           ),
                                  fluidRow(column(1),
                                           column(3, align = 'center',htmlOutput("firstmovie.poster.BOP")),
                                           column(4, align = 'center', htmlOutput("VERSUS.BOP")),
                                           column(3, align = 'center',htmlOutput("secondmovie.poster.BOP")),
                                           column(1)
                                           ),
                                  fluidRow(column(1),
                                           column(3, align = 'center', htmlOutput("firstmovie.meta.BOP")),
                                           column(4),
                                           column(3, align = 'center',htmlOutput("secondmovie.meta.BOP")),
                                           column(1)
                                           ),
                                  fluidRow(column(4,align = "center",plotlyOutput("versus.weekly.avg")),
                                           column(4,align = "center",plotlyOutput("versus.weekly.perc")),
                                           column(4,align = "center",plotlyOutput("versus.weekly.rank"))
                                           ),
                                  fluidRow(column(1),
                                           column(10), column(1)
                                           ),
                                  fluidRow(
                                           # column(6),
                                           column(4, align = "center", plotlyOutput("versus_BO_chart.1")),
                                           column(8, align = "center", plotlyOutput("versus_BO_chart.2"))
                                           )
                                  )
                         ),
              
              navbarMenu("All-time Ranking",
                         tabPanel("Visual Classification",plotlyOutput("Viz")),
                         tabPanel("Ranking Table",tableOutput("combined.ranking.table"))
                         )
            )
  )
