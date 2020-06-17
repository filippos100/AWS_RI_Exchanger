library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(shiny)
library(DT)

Price_1year <- read.csv('Prices1yearsnoupfront.csv') %>%
  mutate(Term = "1year") %>%
  rename(OS = System,Convertible = Price, On_Demand = On.demand,Offering_Class = Offering.class)

Price_3year <- read.csv('Prices3yearsnoupfront.csv') %>%
  mutate(Term = "3years") %>%
  rename(OS = System,Convertible = Price, On_Demand = On.demand,Offering_Class = Offering.class)

Price <- merge(Price_1year,Price_3year,by=c("Region","OS","Type","Convertible","On_Demand","Offering_Class","Term"),all = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Nephele",
                  dropdownMenuOutput("message")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("RI Calculator Form",tabName = "calculatornew",icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "calculatornew",
              fluidPage(
                tags$br(),
                dropdown(
                  textInput("ri","Enter the RI number"),
                  selectInput("Region","Select The Region",
                              choices = unique(Price$Region),
                              selected = "us-west-2"),
                  selectInput("Oldec2","Select The Instance to exchange",
                              choices = unique(Price$Type),
                              selected = "c5.large"),
                  selectInput("Oldos","Select The Original OS",
                              choices = unique(Price$OS)),
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
                tags$hr(),
                column(width = 12, DT::dataTableOutput("responses", width = '100%')))
      )
    )))

######################################################
####################Server############################
######################################################
server <- function(input,output){
  ###################### Exchanger form############################# 
  df <- data.frame(Reservation_ID = character(0),
                   Region = character(0),
                   Oldec2 = character(0),
                   Oldos = character(0),
                   number = character(0),
                   Newec2 = character(0),
                   Newos = character(0),
                   Amount = character(0),
                   Savings = character(0))
  
  
  
  observeEvent(input$submit,{
    data = data.frame(Reservation_ID = input$ri,
                      Region = input$Region,
                      Oldec2 = input$Oldec2,
                      Oldos = input$Oldos,
                      number = input$number,
                      Newec2 = input$Newec2,
                      Newos = input$Newos,
                      Amount = (Price %>% filter(Type == input$Oldec2 & Region == input$Region & 
                                                    OS == input$Oldos) %>% select(Convertible) %>%
                                  slice(1) %>% pull()*input$number)/
                        (Price %>% filter(Type == input$Newec2 &Region == input$Region & 
                                             OS == input$Newos) %>%
                           select(Convertible) %>%
                           slice(1) %>% pull()),
                      Savings = input$number*(Price %>% filter(Type == input$Oldec2 & Region == input$Region & 
                                                                  OS == input$Oldos) %>% select(Convertible) %>%
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
