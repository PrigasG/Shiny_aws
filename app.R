library(shiny)
library(bs4Dash)
library(plotly)
library(DT)


#header----
header = dashboardHeader(
      title = tags$div(
        dashboardBrand(
          title = "NJ Census Report",
          color = "primary",
          href = "https://adminlte.io/themes/v3",
          image = 'https://github.com/PrigasG/NJ_CENSUS/blob/main/NJ_IMAGE.png?raw=true'
        ),
        style = "text-align: center;" # Center the content
      )
    )

#sidebar ----
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Maps", tabName = "maps", icon = icon("map")),
    menuItem(text = "Tables", tabName = "tables", icon = icon("table-list")),
    menuItem(text = "State map", tabName = "state", icon = icon("globe"))
  )
)

#body -----
body = dashboardBody(
      tags$head(
        tags$style(HTML("
          .brand-image {
            width: 100px; /* Adjust the width as needed */
            height: 100px; /* Adjust the height as needed */
            object-fit: cover; /* Ensures the image covers the area without distortion */
            border-radius: 1; /* Ensures the image is square */
          }
        "))
      ),
      tabItems(
        tabItem('maps', #maps tab
          tabsetPanel(type = 'pill',
               tabPanel("Decennial", br(), 
                        fluidRow(numericInput(inputId = "year_id_dec", label = "Decennial Year", 
                                              value = 2010, min = 2010, max = 2020, step = 10)),
                        actionButton("calculate_dec", "Click", class = "blink-green"),
                        tags$p(), 
                        fluidRow(box(title = 'Decennial Box', 
                            uiOutput('decennial_table'))
                        )), 
               tabPanel('ACS', br(), 
                        fluidRow(numericInput(inputId = "year_id_acs", label = "ACS Year", 
                                              value = 2019, min = 2019, max = 2022, step = 1)),
                        actionButton("calculate_acs", "Click", class = "blink-green"),
                        tags$p(), 
                        box(title = 'ACS Box', 
                            uiOutput('ACS_table')
                        ))
      ))
      )
    )

  

#controlbar ----
controlbar = dashboardControlbar()
  
#ui -----
ui <- dashboardPage(header, 
                    sidebar, 
                    body,
                    controlbar)    

#server -----
server <- function(input, output, session) {
  source("Decennial_script.R")
  
  data_dec <- eventReactive(input$calculate_dec, {
    req(input$calculate_dec)  # Ensure the button has been clicked
    
    withProgress(message = 'Generating Data', value = 0, {
      tryCatch({
        dec_result <- process_decennial_data(input$year_id_dec)
        dec_result
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      })
    })
  }, ignoreNULL = FALSE)
  
  output$decennial_table <- renderUI({
    req(data_dec())
    DTOutput("dec_table")
  })
  
  output$dec_table <- renderDataTable({
    req(data_dec())
    datatable(
      data_dec(),
      filter = 'top',
      extensions = c("Buttons"),
      options = list(
        paging = TRUE,
        pageLength = 20,
        lengthMenu = list(c(10, 20, 50, -1), c('10', '20', '50')),
        pagingType = "full_numbers",
        info = TRUE,
        dom = 'Blfrtip',  # This order controls the layout of table elements
        buttons = list(
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel')
        ),
        language = list(
          lengthMenu = "Display _MENU_ records per page"
        )
      ),
      class = "display"
    )
  })

}



#call both ui and server -----
shinyApp(ui, server)
