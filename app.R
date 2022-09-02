library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(purrr)
library(googlesheets4)
library(highcharter)

config <- config::get(file = "config.yml")

# Google authentication ---------------------------------------------------

options(
  gargle_oauth_cache = config$auth$cache,
  gargle_oauth_email = config$auth$email
)
# googlesheets4::gs4_auth()


# Read Google sheet -------------------------------------------------------

sheet_url <- config$sheet_url


# Functions ---------------------------------------------------------------

get_classification <- function(historic, game) {
  # if (game == "dards") {
  #   historic_game <- historic
  # } else {
  #   historic_sep <- historic %>% 
  #     tidyr::separate(jugador, c("jugador1", "jugador2"), sep =  "-") %>% 
  #     group_by(jugador1, jugador2) %>% 
  #     summarise(punts = sum(punts))
  #   for (i in seq(1, nrow(historic_sep))) {
  #     parella_reves_idx <- which((historic_sep$jugador1[i] == historic_sep$jugador2) & 
  #                                  (historic_sep$jugador1 == historic_sep$jugador2[i]))
  #     if (length(parella_reves_idx) > 0) {
  #       print("parella girada")
  #       parella_reves <- historic_sep[parella_reves_idx, ]
  #       parella_reves2 <- parella_reves
  #       names(parella_reves2) <- c("jugador2", "jugador1", "punts")
  #       historic_sep2 <- historic_sep[- parella_reves_idx, ]
  #       historic_sep <- bind_rows(historic_sep2, parella_reves2)
  #     }
  #   }
  #   
  #   historic_game <- historic_sep %>% 
  #     mutate(jugador = paste0(jugador1, "-", jugador2))
  # }
  
  return(
    historic %>% 
      group_by(jugador) %>% 
      summarise(punts = sum(punts)) %>% 
      arrange(desc(punts))
  )
}

get_classification_table <- function(classification, game) {
  # if (game == "dards") {
  #   names(classification) <- c("Persona", "Punts")
  # } else {
  #   names(classification) <- c("Parella", "Punts")
  # }
  names(classification) <- c("Persona", "Punts")
  classification %>% 
    mutate(`Posició` = row_number()) %>% 
    select("Posició", everything()) %>% 
    head(6)
}


# Shiny app ---------------------------------------------------------------

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("cerulean"),
    shinyjs::inlineCSS(
      "
      body { background: #fff5e6; }
      "
    ),
    title = "Triatló de bar",
    div(id = "header",
        titlePanel(tagList(
          img(src = "http://marrecs.cat/wp-content/uploads/2016/01/icona-marrecs-300x300.png", height = 50),
          span(
            strong("Triatló de bar"),
            actionButton("bases", "",
                         icon("info", class="fa fa-info-circle", style="color:#4682B4; font-size: 30px;"),
                         style="background:transparent; border:transparent;")
          )
        )),
        hr(style="border-color: #66a3ff;")
    ),
    
    fluidRow(
      column(3,
             
             div(
               id = "form",
               
               selectInput(
                 "GAME", "Quin joc heu jugat?",
                 setNames(
                   c("dards", "futboli", "butifarra", "catan"),
                   nm = c("Dards", "Futbolí", "Butifarra", "Catan")
                 )
               ),
               
               tags$p("Entra el MALNOM dels participants:"),
               
               fluidRow(
                 column(
                   6,
                   textInput(
                     "WINNER", "Persona guanyadora:"
                   )
                 ),
                 column(
                   6,
                   shinyjs::hidden(div(
                     id = "winner2",
                     # style = "padding-top: 25px;",
                     textInput(
                       "WINNER2", "Persona guanyadora 2:"
                     )
                   )
                 ))
               ),
               fluidRow(
                 column(
                   6,
                   textInput(
                     "LOSER", "Persona no-guanyadora:"
                   )
                 ),
                 column(
                   6,
                   shinyjs::hidden(div(
                     id = "loser2",
                     # style = "padding-top: 25px;",
                     textInput(
                       "LOSER2", "Persona no-guanyadora 2:"
                     )
                   )
                 ))
               ),
               
               shinyjs::disabled(
                 actionButton("submit", "Registra el resultat", class = "btn-primary")
               ),
               
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
                 ))
               )
             ),
             
             div(
               style = "padding-top: 50px; padding-bottom: 50px; text-align: center;",
               img(
                 src = "logo_triatlo.png",
                 # src = "https://upload.wikimedia.org/wikipedia/commons/7/75/Logotip_dels_Marrecs_de_Salt.png",
                 height = 300
               )
             )
             
      ),
      column(
        9,
        fluidRow(
          column(
            3, align = 'center',
            h2(strong("Dards")),
            tableOutput('table_dards'),
            highchartOutput('plot_dards')
          ),
          column(
            3, align = 'center',
            h2(strong("Futbolí")),
            tableOutput('table_futboli'),
            highchartOutput('plot_futboli')
          ),
          column(
            3, align = 'center',
            h2(strong("Butifarra")),
            tableOutput('table_butifarra'),
            highchartOutput('plot_butifarra')
          ),
          column(
            3, align = 'center',
            h2(strong("Catan")),
            tableOutput('table_catan'),
            highchartOutput('plot_catan')
          )
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    last_results <- map(
      set_names(c('dards', 'futboli', 'butifarra', 'catan')),
      ~ read_sheet(sheet_url, sheet = .x)
    )
    
    # Classification ----------------------------------------------------------
    
    classification <- reactiveValues(
      dards = get_classification(last_results$dards, game = "dards"),
      futboli = get_classification(last_results$futboli, game = "futboli"),
      butifarra = get_classification(last_results$butifarra, game = "butifarra"),
      catan = get_classification(last_results$catan, game = "catan")
    )
    
    # Information
    observeEvent(input$bases, {
      showModal(modalDialog(
        title = "Bases del Triatló de bar",
        footer = modalButton("Tanca"),
        HTML(paste(readLines('www/bases.html'), collapse = "\n"))
      ))
    })
    
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      if (input$GAME == "dards") {
        shinyjs::hide("winner2")
        shinyjs::hide("loser2")
        if (stringr::str_length(input$WINNER) > 0 & stringr::str_length(input$LOSER) > 0) {
          shinyjs::enable("submit")
        } else {
          shinyjs::disable("submit")
        }
      } else {
        shinyjs::show("winner2")
        shinyjs::show("loser2")
        if (stringr::str_length(input$WINNER) > 0 & stringr::str_length(input$LOSER) > 0 &
            stringr::str_length(input$WINNER2) > 0 & stringr::str_length(input$LOSER2) > 0) {
          shinyjs::enable("submit")
        } else {
          shinyjs::disable("submit")
        }
      }
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
          datetime = c(now(), now(), now(), now()),
          jugador = c(input$WINNER, input$WINNER2, input$LOSER, input$LOSER2),
          punts = c(3, 3, 1, 1)
        )
      }
      
      new_result <- new_result %>% 
        mutate(jugador = stringr::str_to_upper(jugador))
      
      message("Adjuntant els resultats al Google Sheets")
      
      sheet_append(sheet_url, new_result, sheet = input$GAME)
      
      session$reload()
      
    })
    
    
    # Tables ------------------------------------------------------------------
    
    output$table_dards <- renderTable({
      classification$dards %>% 
        mutate(punts = as.integer(punts)) %>% 
        get_classification_table(game = input$GAME)
    })    
    
    output$table_futboli <- renderTable({
      classification$futboli %>% 
        mutate(punts = as.integer(punts)) %>% 
        get_classification_table(game = input$GAME)
    })   
    
    output$table_butifarra <- renderTable({
      classification$butifarra %>% 
        mutate(punts = as.integer(punts)) %>% 
        get_classification_table(game = input$GAME)
    })   
    
    output$table_catan <- renderTable({
      classification$catan %>% 
        mutate(punts = as.integer(punts)) %>% 
        get_classification_table(game = input$GAME)
    }) 
    
    
    # Plots -------------------------------------------------------------------
    
    output$plot_dards <- renderHighchart({
      classification$dards %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts), name = "Punts") %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    output$plot_futboli <- renderHighchart({
      classification$futboli %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts), name = "Punts") %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    output$plot_butifarra <- renderHighchart({
      classification$butifarra %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts), name = "Punts") %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
    output$plot_catan <- renderHighchart({
      classification$catan %>% 
        hchart(type = "column", hcaes(x = jugador, y = punts), name = "Punts") %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "Punts"))
    })
    
  }
)
