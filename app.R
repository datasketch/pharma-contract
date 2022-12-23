library(airtabler)
library(dsapptools)
library(shiny)
library(shinypanels)


ui <- panelsPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.min.css"),
    tags$script(src="app.min.js")
  ),
  panel(title = "Filters",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 350,
        body =  div(
          uiOutput("menu_buttons")
          #uiOutput("controls")
        )
  ),
  panel(title = "Visualization",
        id = "pharma-panel",
        can_collapse = FALSE,
        header_right = div(
          class = "head-viz",
          div(class = "viz-style",
              uiOutput("viz_icons")),
          uiOutput("downloads")
        ),
        body =  div(
          verbatimTextOutput("test")
          #uiOutput("viz_view")
        )
  ),
  panel(title = "Detail",
        id = "pharma-detail",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(
          shinycustomloader::withLoader(
            uiOutput("click_info"),
            type = "html", loader = "loader4"
          )
        ),
        footer =  div(class = "footer-logos",
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src= 'img/logos/logo_ds.svg',
                            align = "left", width = 130, height = 70)),
                      img(src= 'img/logos/logo_ins.svg',
                          width = 150, height = 150)
        )
  )
)


server <- function(input, output, session) {
  
  
  
  # controls ----------------------------------------------------------------
  
  list_menu <- reactive({
    dsapptools:::make_buttons(c("a", "b", "c"),
                              c("<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Request</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Appeals</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Contracts details</div>"),
                              class_buttons = "menu-line")
  })
  
  
  chosen_menu <- reactiveValues(id = NULL)
  observeEvent(input$last_click,{
    chosen_menu$id <- input$last_click
  })
  
  
  observe({
    if(is.null(list_menu())) return()
    l <- list_menu()
    last_btn <- chosen_menu$id
    if (!is.null(last_btn)) {
      button_id <- which(c("a", "b", "c") %in% last_btn)
      print(button_id)
      l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
      l[[button_id]] <- gsub("arrow-menu", "arrow-menu-active", l[[button_id]])
      l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
      if (last_btn == "a") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("request_countries", 
                                             "Country",
                                             setdiff(unique(df$`Request Country`), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("request_status", 
                                             "Status",
                                             setdiff(unique(df$Status), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "b") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("appeals_countries", 
                                             "Country",
                                             setdiff(unique(df$Country), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("appeals_status", 
                                             "Status",
                                             setdiff(unique(df$Status), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "c") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("contracts_countries", 
                                             "Country",
                                             setdiff(unique(df$Country), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("contracts_supplier", 
                                             "Status",
                                             setdiff(unique(df$Supplier), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("contracts_vaccine", 
                                             "Vaccine",
                                             setdiff(unique(df$Vaccine), NA),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
    }
    output$menu_buttons <- renderUI({
      l
    })
  })
  
  
  # data --------------------------------------------------------------------
  
  data_load <-
    isolate({
      reactivePoll(1000,
                   session,
                   checkFunc = function() {
                     if (is.null(chosen_menu$id)) return()
                     df <- NULL
                     if (chosen_menu$id == "a") {
                       df <-    "data/data_request.RData"
                     }
                     if (chosen_menu$id %in% "b") {
                       df <- "data/data_appeals.RData"
                     }
                     if (chosen_menu$id %in% "c") {
                       df <- "data/data_contracts.RData"
                     }  
                     if (file.exists(df))
                       file.info(df)$uid
                     else
                       shinyalert(title = "file",text = "Archivo no encontrado")
                   },
                   
                   valueFunc = function() {
                     if (is.null(chosen_menu$id)) return()
                     load_data <- NULL
                     if (chosen_menu$id == "a") {
                       load_data <- "load_request_data.R"
                     }
                     if (chosen_menu$id %in% "b") {
                       load_data <- "load_appeals_data.R"
                     }
                     if (chosen_menu$id %in% "c") {
                       load_data <- "load_contracts_data.R"
                     }  
                     source(load_data)$value
                   }
      )
    })
  
  
  output$test <- renderPrint({
    data_load()
  })
  
}

shinyApp(ui, server)



