library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(shiny)
library(DT)
library(rdrop2)

source("etl.R")

ui <- dashboardPage(
  dashboardHeader(title = "Nephele",
                  dropdownMenuOutput("message")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("RI Calculator Form",tabName = "calculatornew",icon = icon("calculator")),
      menuItem("Search",tabName = "search")
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "calculatornew",
              fluidPage(
                tags$br(),
                dropdown(
                  textInput("ri","Enter the RI number"),
                  textInput("account","Enter the account"),
                  selectInput("Region","Select The Region",
                              choices = unique(Price$Region),
                              selected = "us-west-2"),
                  selectInput("Oldec2","Select The Instance to exchange",
                              choices = unique(Price$Type),
                              selected = "c5.large"),
                  selectInput("Oldos","Select The Original OS",
                              choices = unique(Price$OS)),
                  selectInput("term","Select The Term",
                              choices = unique(Price$Term)),
                  numericInput("number","Enter Number of Instances",1,min = 1,max = 10000),
                  selectInput("Newec2","Select the Candidate",
                              choices = unique(Price$Type),
                              selected = "c5.large"),
                  selectInput("Newos","Select The destination OS",
                              choices = unique(Price$OS)),
                  actionButton("submit", "Submit"),
                  actionButton("edit", "Edit"),
                  
                  style = "unite", 
                  icon = icon("plus"),
                  status = "danger", 
                  #width = "300px",
                  size = "m",
                  label = "Add new Record",
                  tooltip = TRUE,
                  animate = animateOptions(
                    enter = animations$fading_entrances$fadeInLeftBig,
                    exit = animations$fading_exits$fadeOutRightBig
                  )
                  
                ),
                tags$hr(),
                downloadButton("downloadData", "Download"),
                actionButton("deleteRow", "Delete Row"),
                actionButton("save","Save"),
                tags$hr(),
                column(width = 12, DT::dataTableOutput("responses", width = '100%')))
      ),
      tabItem(tabName = "search",
              fluidPage(DT::dataTableOutput("searchtable")))
    )))

######################################################
####################Server############################
######################################################
server <- function(input,output,session){
  ###################### Exchange form############################# 
  df <- data.frame(Date = character(0),
                   Reservation_ID = character(0),
                   Account = character(0),
                   Region = character(0),
                   Oldec2 = character(0),
                   Oldos = character(0),
                   Term = character(0),
                   number = character(0),
                   Newec2 = character(0),
                   Newos = character(0),
                   Amount = character(0),
                   Savings = character(0))
  
  
  
  observeEvent(input$submit,{
    data = data.frame(Date = Sys.Date(),
                      Reservation_ID = input$ri,
                      Account = input$account,
                      Region = input$Region,
                      Oldec2 = input$Oldec2,
                      Oldos = input$Oldos,
                      Term = input$term,
                      number = input$number,
                      Newec2 = input$Newec2,
                      Newos = input$Newos,
                      Amount = ceiling((Price %>% filter(Type == input$Oldec2 & Region == input$Region & 
                                                    OS == input$Oldos & Term == input$term) %>% select(Convertible) %>%
                                  slice(1) %>% pull()*input$number)/
                        (Price %>% filter(Type == input$Newec2 &Region == input$Region & 
                                             OS == input$Newos & Term == input$term) %>%
                           select(Convertible) %>%
                           slice(1) %>% pull())),
                      Savings = input$number*(Price %>% filter(Type == input$Oldec2 & Region == input$Region & 
                                                                  OS == input$Oldos & Term == input$term) %>% select(Convertible) %>%
                                                slice(1) %>% pull()*730))
    
    df <<-  rbind(df,data)
  })
  
  observeEvent(input$deleteRow,{
    
    df <<- df%>%
      filter(row_number() < nrow(.))
  })
  
  output$responses <- DT::renderDataTable({
    input$submit
    input$deleteRow
    
    datatable(df)
    #return(df)
  })
  

  observe({
    input$submit
    input$deleteRow
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Form", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })

}

shinyApp(ui,server)
