box::use(shiny[stopApp, br, h1, h2, h3, moduleServer, modalButton, 
               h4, div, tagList, tags, reactiveVal, modalDialog,
               updateActionButton, uiOutput, reactiveValues, showModal,
               renderUI, icon, actionButton, observeEvent, req],
         shinyjs[hide, show, useShinyjs, delay],
         utils[read.csv],
         bslib[page_fluid, card],
         purrr[map],
         waiter[useWaiter, waiterShowOnLoad, 
                waiterOnBusy, autoWaiter, spin_plus, 
                spin_dots, spin_6, waiter_hide, Waiter],
         shinyWidgets[radioGroupButtons],
         htmltools[HTML])

source("global.R")

ui <- page_fluid(
    useWaiter(),
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400..900&display=swap"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css")
    ),
    title = "Quizapp",
    card(
      full_screen = FALSE,
      div(
        class = "header-title",
        tags$img(
          src = "appsilon-logo.png",
          height = 62,
          width = 140
        ),
        h1("Find out which R package you are! 👀"),
        actionButton(
          "info", 
          "", 
          icon = icon("circle-info")
        )
      )
    ),
    uiOutput("questions"),
    div(
      class = "proceed-btn",
      actionButton(
        inputId = "next_level",
        label = "Next",
        icon = icon("forward-step")
      )
    ),
    waiterShowOnLoad(
      color = "white",
      html = tagList(
        spin_plus(),
        br(),
        br(),
        h2("Loading...")
      )
    )
  )

#' @export
server <- function(input, output, session) {
    
    # Loading screen
    Sys.sleep(2)
    waiter_hide()
    w <- Waiter$new(color = "white", 
                    html = result_waiting_screen(result_waiting_msgs[1]))
    
    
    # modal function
    display_modal <- function() {
      modalDialog(
        description,
        size = "l",
        easyClose = FALSE,
        fade = TRUE, 
        footer = modalButton(
          label = "Got it", 
          icon = icon("thumbs-up")
        )
      ) |> showModal()
    }
    
    display_modal()
    
    # Function to generate question cards
    question_page <- function(data) {
      question_list <- map(1:nrow(data), ~ {
        
        question_card <-
          div(class = "question-container",
              card(
                full_screen = FALSE,
                div(
                  class = "question",
                  data[.x, "Question"]
                ),
                radioGroupButtons(
                  inputId = paste0("options_", .x),
                  label = "",
                  choiceNames = c(data[.x, "Option1"],
                                  data[.x, "Option2"],
                                  data[.x, "Option3"],
                                  data[.x, "Option4"]),
                  choiceValues = c("a", "b", "c", "d")
                )
              ))
        return(question_card)
      })
      return(question_list)
    }
    
    # Option selection count
    option_count <-
      reactiveValues(
        options = list(
        a = 0,
        b = 0,
        c = 0,
        d = 0
      ))
    
    
    # Record selected options
    record_option_freq <- function(data) {
      map(1:nrow(data), ~ {
        id <- paste0("options_", .x)
        option_count$options[[input[[id]]]] = option_count$options[[input[[id]]]] + 1
      })
      option_count_vector <- unlist(option_count$options)
      max_count_option <-
        names(option_count_vector)[which.max(option_count_vector)]
      return(max_count_option)
    }
    
    # Page number count
    page_number <- reactiveVal(1)
    
    # Total # of question pages
    all_question_pages <- question_page(questions)
    
    # First question render
    output$questions <- renderUI({
      hide("next_level")
      div(
        class = "start-screen-container",
        actionButton(
          inputId = "start_btn",
          label = "Start",
          icon = icon("play"))
      )
    })
    
    # Function to navigate to subsequent questions
    next_page <- function() {
      page_ui <- 
        if (page_number() < total_no_of_questions + 1) {
          all_question_pages[[page_number()]]
        }
      else {
        hide("next_level")
        max_count_option <- record_option_freq(questions)
        div(class = "question-container",
            card(
              full_screen = FALSE,
              div(
                class = "analysis-result",
                div(
                  class = "emoji",
                  sample(emojis, 1)
                ),
                div(
                  analysis[[max_count_option]] |> 
                    HTML() |> h3()
                )
              )
            )
        )
      }
      output$questions <- renderUI({
        if (page_number() < total_no_of_questions + 2) 
          page_ui
        else {
          w$show()
          for(i in 1:length(result_waiting_msgs)){
            w$update(html = result_waiting_screen(result_waiting_msgs[i]))
            Sys.sleep(3)
          }
          w$hide()
          page_ui
        }
      })
      
      if (page_number() == 10) {
        updateActionButton(
          inputId = "next_level",
          label = "Analyse!",
          icon = icon("gears")
        )
      }
      page_number(page_number() + 1)
    }
    
    observeEvent(input$info, {
      display_modal()
    })
    
    observeEvent(input$start_btn, {
      next_page()
      hide("start_btn")
      shinyjs::delay(1000, show("next_level"))
    })
    
    # Question navigation on button click
    observeEvent(input$next_level, {
      next_page()
    })
  }

shinyApp(ui, server)