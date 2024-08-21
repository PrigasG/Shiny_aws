#server -----
server <- function(input, output, session) {
  source("Decennial_script.R")
  
  data_dec <- eventReactive(input$calculate_dec, {
    req(input$calculate_dec)  # Ensure the button has been clicked
    
    withProgress(message = 'Generating Data', value = 0, {
      tryCatch({
        dec_result <- process_decennial_data(input$year_id_dec)
        return(dec_result)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
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
          lengthMenu = "Display *MENU* records per page"
        )
      ),
      class = "display"
    )
  })
}
