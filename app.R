#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


################################################################################ Load Package and Install Libraries
library(devtools)
# devtools::install_github("ThomasSiegmund/D3TableFilter")

library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(openxlsx)
library(usethis)
library(shinyfilter)
library(htmlwidgets)
library(D3TableFilter)
library(shinyBS)
library(reactable)

##################################################### Functions
selectinput_function<-function(id,label_name){
  selectInput(inputId = id,
              label = label_name,
              choices = NULL,
              selected = NULL,
              multiple = FALSE)
}

selectJoininput_function<-function(join){
  selectInput(join, label = "", choices = c("OR", "AND"))
}


############################################ UI

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  navbarPage(
    strong("EMPLOYEE DATA REPORT"),                               ## Name of App
    
    img(src = 'logo.png', height = '100px', width = '100px'),
    tabPanel(
      "Instruction and Upload Data",                        
      sidebarLayout(
        sidebarPanel(
          h3("UPLOAD DATA"),
          fileInput(
            "file1",
            "Choose xlsx file",
            accept = c(".xlsx")
          ),
          h3("INSTRUCTION"),
          h4("1. Please upload sample xlsx data file first"),
          h4("2. Basic Data Filter is used when you want limited filter for each column, and the logical filter between columns is AND"),
          h4("3. Advanced Data Filter is used when you want one or multiple filter for each column, and logical filter between columns could be AND/OR")
        ),
        mainPanel(DTOutput("file2"))
      )),
    
    
    tabPanel(                                                      
      "Basic Data Filter",
      DTOutput("file3"),
      
    ),
    
    tabPanel(                                                      
      "Advanced Data Filter",
      sidebarLayout(
        sidebarPanel(
          
          selectinput_function("var1","Select First Variable"),
          selectinput_function("var2","Select Values for First Variable"),
          selectJoininput_function("join1"),
          
          selectinput_function("var3","Select Second Variable"),
          selectinput_function("var4","Select Values for Second Variable"),
          selectJoininput_function("join2"),
          
          selectinput_function("var5","Select Third Variable"),
          selectinput_function("var6","Select Values for Third Variable"),
        ),
        mainPanel(
          DT::dataTableOutput("advanced_table")))),
    
    
    # tabPanel(
    #   "Map",
    #   sidebarLayout(
    #     sidebarPanel(
    #       h3("UPLOAD DATA"),
    #       fileInput(
    #         "file2",
    #         "Choose xlsx file",
    #         accept = c(".xlsx")
    #       )
    #       
    #     ),
    #     mainPanel( plotOutput("plot", brush = "plot_brush"))
      )
    )
    
    
  


######################################### Server

server <- function(input, output,session) {
  
  
  mydata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    readxl::read_excel(inFile$datapath)
  })
  
  mydata1<-reactive({
    mydata()
  })
  
  output$file2<-DT::renderDT(                                       
    mydata1() %>%
      as.data.frame() %>%
      DT::datatable(rownames = FALSE, options = list(pageLength =5))
    
  )
  
  ####################################################
  
  
  output$file3 <-DT::renderDT({                                      ## Ouput for Second Tab
    
    datatable(mydata1(),
              extensions = 'Buttons', 
              rownames = FALSE,
              filter = "top", 
              options = list(
                #pageLength = 5,
                paging = FALSE,
                scrollX=TRUE,
                #searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'l<"sep">Bfrtip',
                
                buttons = c('copy','print', 'csv', 'excel')
              )
              
    )
  })
  
  ####################################################  
  # get the column names of the dataset
  column_list <- reactive({
    req(mydata())
    column_list <- names(mydata())
    c(column_list)
  })
  
  
  selectVariable_function <-function(variable){
    observeEvent(column_list(), {
      updateSelectInput(session, variable, choices = column_list())
    })
  }
  
  
  # update the first selectInput based on the column names
  selectVariable_function("var1")
  
  #update the second selectInput based on the first input
  observeEvent(input$var1, {
    choicesvar1=unique(mydata()[[input$var1]])
    req(choicesvar1)
    updateSelectInput(session, "var2",label = "First Filter", choices = choicesvar1)
  })
  
  
  observeEvent(input$var2, {
    updateSelectInput(session, "join1", choices= c("OR" ="|","AND" ="&"))
  })
  
  # update the third selectInput based on the column names
  selectVariable_function("var3")
  
  # update the fourth selectInput based on the third input
  observeEvent(input$var3, {
    choicesvar2=unique(mydata()[[input$var3]])
    req(choicesvar2)
    updateSelectInput(session, "var4",label ="Second Filter", choices = choicesvar2)
  })
  
  
  observeEvent(input$var4, {
    updateSelectInput(session, "join2", choices= c("OR" = "|","AND" = "&"))
  })
  
  # update the fifth selectInput based on the column names
  selectVariable_function("var5")
  
  # update the fourth selectInput based on the third input
  observeEvent(input$var5, {
    choicesvar3=unique(mydata()[[input$var5]])
    req(choicesvar3)
    updateSelectInput(session, "var6",label="Third Filter", choices = choicesvar3)
  })
  
  
  
  filteredData <- reactive({
    req(input$var1, input$var2, input$join1, input$var3, input$var4, input$join2, input$var5, input$var6)
    f <- mydata()
    
    
    if (input$join1 == "AND" & input$join2 == "AND") {
      filter1 <- ((f[[input$var1]] == input$var2) & (f[[input$var3]] == input$var4) & (f[[input$var5]] == input$var6))
      
    } else if (input$join1 == "AND" & input$join2 == "OR") {
      filter1 <- ((f[[input$var1]] == input$var2) & (f[[input$var3]] == input$var4))  | (f[[input$var5]] == input$var6)
      
    } else if (input$join1 == "OR" & input$join2 == "AND") {
      filter1 <- (f[[input$var1]] == input$var2)   |   ((f[[input$var5]] == input$var6) & (f[[input$var3]] == input$var4)) 
    } else {
      filter1 <- ((f[[input$var1]] == input$var2) | (f[[input$var3]] == input$var4) | (f[[input$var5]] == input$var6))
    }
    
    ff <- f[filter1, ]
    ff
    
  })
  
  output$advanced_table <- renderDT({                      
    datatable(filteredData())
    
  })
  
  ###############################################################
  
  mydata2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    readxl::read_excel(inFile$datapath)
  })
  
  
  
}

######################################## Run the application 
shinyApp(ui = ui, server = server)





