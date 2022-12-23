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
  
  
  # data --------------------------------------------------------------------
  
  
  
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
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("test1", "test bla", c("un", "dos", "tres")),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "b") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        l[[button_id]] <- div(l[[button_id]],
                              radioButtons("test1", "test bla", c("un", "dos", "tres")),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "c") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        l[[button_id]] <- div(l[[button_id]],
                              checkboxGroupInput("test1", "test bla", c("un", "dos", "tres")),
                              div(class = "menu-line")
        )
      }
    }
    output$menu_buttons <- renderUI({
      l
    })
  })
  
  
  output$test <- renderPrint({
    chosen_menu$id
  })
  
}

shinyApp(ui, server)



