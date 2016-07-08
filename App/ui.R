source('../R/helpers.R')
libraries()
library(shinythemes)
movie.titles <- c("Howard The Duck",
                  "Blade",
                  "X-Men","Blade II",
                  "Spider-Man","Daredevil",
                  "X-men 2","Hulk",
                  "The Punisher","Spider-Man 2",
                  "Blade: Trinity", "Elektra",
                  "Fantastic Four(2005)","X-Men: The Last Stand",
                  "Ghost Rider","Spider-Man 3",
                  "Fantastic Four: Rise of the Silver Surfer","Iron Man",
                  "The Incredible Hulk","Punisher: War Zone",
                  "X-Men Origins: Wolverine","Iron Man 2",
                  "Thor","X-Men: First Class",
                  "Captain America: The First Avenger",
                  "Ghost Rider: Spirit of Vengeance",
                  "Marvel's The Avengers","The Amazing Spider-Man",
                  "Iron Man 3","The Wolverine",
                  "Thor 2: The Dark World","Captain America: The Winter Soldier",
                  "The Amazing Spider-Man 2","X-Men: Days of Future Past",
                  "Guardians of the Galaxy","Big Hero 6",
                  "Avengers: Age of Ultron", "Ant-Man",
                  "Fantastic Four(2015)", "Deadpool",
                  "Captain America: Civil War","Superman","Superman II",
                  "Superman III", "Supergirl",
                  "Superman IV: The Quest for Peace","Batman",
                  "Batman Returns","Batman: Mask of the Phantasm",
                  "Batman Forever", "Batman Robin",
                  "Steel", "Catwoman",
                  "Constantine", "Batman Begins",
                  "V for Vendetta", "Superman Returns",
                  "The Dark Knight", "Watchmen",
                  "The Losers","Jonah Hex","Red",
                  "Green Lantern", "The Dark Knight Rises",
                  "Man of Steel","Red 2",
                  "Batman v Superman: Dawn of Justice")

movie.titles <- sort(movie.titles)




shinyUI(
  navbarPage( 
              theme = "bootstrap.css",
              "Comic Book Movies Explorer",
              
              tabPanel("Filter and Rank",
                       fluidRow(

                         column(2,selectizeInput('ip',
                                                 h4('Comic Book Company'),
                                                 choices = c("All","DC","Marvel"),
                                                 multiple = TRUE,
                                                 selected = "Marvel"
                                                 )),

                         column(2,selectizeInput('studio',
                                                 h4('Studio'),
                                                 choices = c("All",
                                                             "Warner Bros.",
                                                             "Sony",
                                                             "Disney",
                                                             "Paramount",
                                                             "Fox",
                                                             "Summit Entertainment",
                                                             "Universal",
                                                             "New Line",
                                                             "Lionsgate",
                                                             "TriStar"
                                                             ),
                                                 multiple = TRUE,
                                                 selected = "Disney"
                                                 )),

                         column(2, sliderInput("years",
                                               label = h4("Years"),
                                               min = 1978,
                                               max = 2016,
                                               value = c(2005, 2015))),

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
                                                 selected = "Rotten Tomatoes Critics Tomatometer",
                                                 choices = c("Overall Estimated Critical Reception",
                                                             "Box Office Performance index",
                                                             "Rotten Tomatoes Critics Tomatometer",
                                                             "Rotten Tomatoes Critics Rating",
                                                             "Custom Rotten Tomatoes Critics Score",
                                                             "Rotten Tomatoes Audience Tomatometer",
                                                             "Rotten Tomatoes Audience Rating",
                                                             "Custom Rotten Tomatoes Audience Score",
                                                             "Metascore","IMDB","Foreign BO", "Domestic BO",
                                                             "Foreign & Domestic BO - Weighted Mean (30/70)",
                                                             "Average per Theater",
                                                             "Weekly Relative Percent Gross (vs Opening Week)",
                                                             "Weekly Ranking Index"),
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
              navbarMenu("Versus",
                         tabPanel("Critical Reception",
                                  fluidRow(column(1),
                                           column(3,
                                                  align = 'center',
                                                  selectizeInput("firstmovie.CR",
                                                                 h4(''),
                                                                 selected = "The Dark Knight",
                                                                 choices = movie.titles,
                                                                 multiple =F )),
                                           column(4),
                                           column(3,
                                                  align = 'center',
                                                  selectizeInput("secondmovie.CR",
                                                                 h4(''),
                                                                 selected = "Marvel's The Avengers",
                                                                 choices = movie.titles,
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
                                  fluidRow(column(1),
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
                                    column(1),
                                    column(10, align = "center", plotlyOutput("versus_critics_chart"))
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
                                                                 h4(''),
                                                                 selected = "The Dark Knight",
                                                                 choices = movie.titles,
                                                                 multiple =F )),
                                           column(4),
                                           column(3, 
                                                  align = 'center',
                                                  selectizeInput("secondmovie.BOP",
                                                                 h4(''),
                                                                 selected = "Marvel's The Avengers",
                                                                 choices = movie.titles,
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
