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
    title = "Comics Adaptations",
    tags$header(
      tags$link(rel="stylesheet", type="text/css", href="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.css"),
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      tags$script(src="https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.js"),
      tags$script(
        src="https://code.jquery.com/jquery-3.2.1.min.js",
        integrity="sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4=",
        crossorigin="anonymous"
      )
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
    
    tabPanel("Compare", htmlTemplate("compare.html", first.movie = first.movie, second.movie = second.movie)),
    tabPanel("All-Time Ranking",htmlTemplate("ranking.html"))
  )
)