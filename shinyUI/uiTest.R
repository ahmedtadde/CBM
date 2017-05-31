ui <- fluidPage(
  checkboxGroupInput("ip", "Comicbook Company",
                     label = list("DC", "Marvel","Other"),
                     choices = list("<h2>DC", "MARVEL", "OTHER"),
                     inline =TRUE),
  textOutput("txt")
)

server <- function(input, output, session) {
  output$txt <- renderText({
    icons <- input$ip
    if(is.null(icons)){
      paste("Please make a selection")
    }else{
      paste("You chose", paste0(icons, collapse = ", "))
    }
  })
}

shinyApp(ui, server)