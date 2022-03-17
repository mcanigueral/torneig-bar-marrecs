library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(purrr)
library(googlesheets4)
library(highcharter)
library(DT)

gs4_deauth()

# which fields get saved 
fieldsAll <- c("GAME", "WINNER", "LOSER")

#Sheet link
sheet_url <- "https://docs.google.com/spreadsheets/d/1vh1Fjc7JRIGOsZ7osZD2BJ27wdxb5eALq1-2XhSUuUY/edit?usp=sharing"
last_results_dards <- read_sheet(sheet_url, sheet = 'dards')
last_results_futboli <- read_sheet(sheet_url, sheet = 'futboli')
last_results_butifarra <- read_sheet(sheet_url, sheet = 'butifarra')

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
    # shinyjs::inlineCSS(appCSS),
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
            "GAME", "Quin joc heu jugat?",
            setNames(
              c("dards", "futboli", "butifarra"),
              nm = c("Dards", "Futbolí", "Butifarra")
            )
          ),
          
          div(
            id = "player",
            tags$p("Entra el MALNOM dels participants:"),
            textInput(
              "WINNER", "Persona guanyadora:"
            ),
            textInput(
              "LOSER", "Persona no-guanyadora:"
            )
          ),
          
          shinyjs::hidden(
            div(
              id = "players",
              tags$p("Entra el MALNOM dels participants:"),
              fluidRow(
                column(
                  6,
                  textInput(
                    "WINNER", "Persona guanyadora 1:"
                  ),
                ),
                column(
                  6,
                  textInput(
                    "WINNER2", "Persona guanyadora 2:"
                  ),
                ),
              ),
              fluidRow(
                column(
                  6,
                  textInput(
                    "LOSER", "Persona no-guanyadora 1:"
                  )
                ),
                column(
                  6,
                  textInput(
                    "LOSER2", "Persona no-guanyadora 2:"
                  )
                )
              )
            )
          ),

          # hr(),
          
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
            hr(),
            actionLink("submit_another", "Entra una altra resposta")
          )
        )

      ),
      column(
        4,
        dataTableOutput('table_dards'),
        highchartOutput('plot_dards')
      ),
      column(
        4,
        dataTableOutput('table_futboli'),
        highchartOutput('plot_dards')
      ),
      column(
        4,
        dataTableOutput('table_butifarra'),
        highchartOutput('plot_dards')
      )
      # column(
      #   8,
      #   highchartOutput('plot')
      # )
    )
  ),
  
  server = function(input, output, session) {
    
    dadesTorneig <- reactiveValues(
      dards = last_results_dards,
      futboli = last_results_futboli,
      butifarra = last_results_butifarra
    )
    
    all_filled <- reactive({
      all(map_lgl(
        fieldsMandatory,
        ~ !is.na(input[[.x]])
      ))
    })
    
    # # Once game is selected show players input
    # observeEvent(input$GAME, {
    #   
    #   print(input$GAME)
    #   
    #   if (input$GAME == "dards") {
    #     shinyjs::show("player")
    #     shinyjs::hide("players")
    #   } else if (input$GAME %in% c("futboli", "butifarra")) {
    #     shinyjs::show("players")
    #     shinyjs::hide("player")
    #   } else {
    #     shinyjs::hide("player")
    #     shinyjs::hide("players")
    #   }
    #   
    # })
    
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
      
      
      
      if (input$GAME == "dards") {
        new_result <- tibble(
          datetime = c(now(), now()),
          jugador = c(input$WINNER, input$LOSER),
          punts = c(3, 1)
        )
      } else {
        new_result <- tibble(
          datetime = c(now(), now()),
          jugador = c(
            paste0(input$WINNER, "-", input$WINNER2),
            paste0(input$LOSER, "-", input$LOSER2)
          ),
          punts = c(3, 1)
        )
      }
      
      new_result <- new_result %>% 
        mutate(jugador = stringr::str_to_upper(jugador))
      
      message("Adjuntant els resultats al Google Sheets")
      
      sheet_append(sheet_url, new_result, sheet = input$game)
      
      message("Actualitzant taules de dades")
      
      if (input$GAME == "dards") {
        plotSettings$dards <- bind_rows(
          last_results_dards,
          new_result
        )
      }
      
      if (input$GAME == "futboli") {
        plotSettings$futboli <- bind_rows(
          last_results_futboli,
          new_result
        )
      }
      
      if (input$GAME == "butifarra") {
        plotSettings$butifarra <- bind_rows(
          last_results_butifarra,
          new_result
        )
      }
      
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::reset("form")
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    

# Tables ------------------------------------------------------------------

    output$table_dards <- DT::renderDataTable({
      # dadesTorneig$dards %>% 
      last_results_dards %>% 
        DT::datatable(colnames = c("Dia i hora", "Jugador", "Punts"))
    })    
    
    output$table_futboli <- DT::renderDataTable({
      # dadesTorneig$futboli %>% 
      last_results_futboli %>% 
        DT::datatable(colnames = c("Dia i hora", "Parella", "Punts"))
    })   
    
    output$table_butifarra <- DT::renderDataTable({
      # dadesTorneig$butifarra %>% 
      last_results_butifarra %>% 
        DT::datatable(colnames = c("Dia i hora", "Parella", "Punts"))
    })   
    
    

# Plots -------------------------------------------------------------------
    
    output$plot_dards <- renderHighchart({
      # dadesTorneig$dards %>% 
      last_results_dards %>% 
        group_by(jugador) %>% 
        summarise(punts = sum(punts)) %>% 
        arrange(desc(punts)) %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts)) %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    output$plot_futboli <- renderHighchart({
      # dadesTorneig$futboli %>% 
      last_results_futboli %>% 
        group_by(jugador) %>% 
        summarise(punts = sum(punts)) %>% 
        arrange(desc(punts)) %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts)) %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    output$plot_butifarra <- renderHighchart({
      # dadesTorneig$butifarra %>% 
      last_results_butifarra %>% 
        group_by(jugador) %>% 
        summarise(punts = sum(punts)) %>% 
        arrange(desc(punts)) %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts)) %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
  }
)
