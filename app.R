library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(purrr)
library(googlesheets4)
library(highcharter)

gs4_deauth()

# which fields get saved 
fieldsAll <- c("GAME", "WINNER", "LOSER")

#Sheet link
sheet_url <- "https://docs.google.com/spreadsheets/d/1vh1Fjc7JRIGOsZ7osZD2BJ27wdxb5eALq1-2XhSUuUY/edit?usp=sharing"
last_results <- read_sheet(sheet_url)

# which fields are mandatory
fieldsMandatory <- fieldsAll

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("cerulean"),
    shinyjs::inlineCSS(appCSS),
    title = "Torneig de bar",
    div(id = "header",
        titlePanel(tagList(
          img(src = "http://marrecs.cat/wp-content/uploads/2016/01/icona-marrecs-300x300.png", height = 50),
          span(strong("Torneig de bar"))
        ))
    ),
    
    fluidRow(
      column(4,
        div(
          id = "form",
    
          selectInput(
            "GAME", "Quin joc heu jugat?", c("Futbolí", "Dards", "Butifarra")
          ),
          textInput(
            "WINNER", "Persona o parella guanyadora:"
          ),
          textInput(
            "LOSER", "Persona o parella no-guanyadora:"
          ),
          
          actionButton("submit", "Registra el resultat", class = "btn-primary"),
          
          shinyjs::hidden(
            span(id = "submit_msg", "Submitting..."),
            div(id = "error",
                div(br(), tags$b("Error: "), span(id = "error_msg"))
            )
          )
        ),
        
        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            br(),
            h2(strong(
              "Gràcies per jugar!"
            )),
            # wellPanel(
            #   uiOutput("form_answer")
            # ),
            # uiOutput('signup_form'),
            hr(),
            actionLink("submit_another", "Entra una altra resposta")
          )
        )

      ),
      column(
        8,
        highchartOutput('plot')
      )
    )
  ),
  server = function(input, output, session) {
    
    plotSettings <- reactiveValues(
      df = last_results
    )
    
    all_filled <- reactive({
      all(map_lgl(
        fieldsMandatory,
        ~ !is.na(input[[.x]])
      ))
    })
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      shinyjs::toggleState(id = "submit", condition = all_filled())
    })
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })

      new_result <- tibble(
        datetime = c(now(), now()),
        player = c(input$WINNER, input$LOSER),
        game = c(input$GAME, input$GAME),
        points = c(3, 1)
      )
      
      plotSettings$df <- bind_rows(
        last_results,
        new_result
      )

      sheet_append(sheet_url, new_result)
      
    })
    
    
    output$plot <- renderHighchart({
      plotSettings$df %>% 
        group_by(player) %>% 
        summarise(points = sum(points)) %>% 
        arrange(desc(points)) %>% 
        hchart(type = "column", hcaes(x = player, y = points)) %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::reset("form")
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
  }
)
