#First steps

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
# require() gives a warning message and returns FALSE if the requested package is not found
#load al the necessary libraries with pacman
pacman::p_load(rstudioapi,
               shiny,
               shinydashboard,
               DT,
               dplyr,
               highcharter)


#### Set wd ####
# getActiveDocumentContext() provides information on where is the user's cursor
getActiveDocumentContext()
current_path = getActiveDocumentContext()$path
# set working directory where the script is
setwd(dirname(current_path)) # dirname() function gets the directory name of a file.
setwd("..") #.. to move locally up a folder from current directory
rm(current_path)
#### Import data #### read.csv("C:/Respaldo FR/UBIQUM/MODEL 2 TASK3/PRODUCTATTRIBUTESID.csv")
data_bydays<-read.csv("C:/Respaldo FR/UBIQUM/iot2/Shinny/datasets/data_bydays.csv")
data_byweeks<-read.csv("C:/Respaldo FR/UBIQUM/iot2/Shinny/datasets/data_byweeks.csv")
data_bymonths<-read.csv("C:/Respaldo FR/UBIQUM/iot2/Shinny/datasets/data_bymonths.csv")

# set col skipping the first one
cols = names(data_bydays)[c(2: length(names(data_bydays)))]


#### 1. A first basic example ####
#ui - functions that assemble an HTML user interface for the app
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title="Energy consumption for home",
    titleWidth=550
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon =icon("fal fa-table")),
      menuItem("Graphs", tabName = "graphs", icon =icon("bar-chart-o")),
      menuItem("Text", tabName = "text", icon = icon("line-chart")),
      menuItem("BLABLA", tabName = "blabla", icon = icon("fas fa-star")),
      menuItem("SOLVVVVE", tabName = "blabla", icon = icon("fas fa-star") )
      
      #to choose the icons:
      
      #- http://fontawesome.io/icons/
      #- http://getbootstrap.com/components/#glyphicons
    )
  ),
  dashboardBody()
)
#server - instructions on how to build and rebuild the R objects that are displayed in the UI
server <- function(input, output) { }
shinyApp(ui, server)





#### 4. Add another widget to select a specific variable ####
#ui - functions that assemble an HTML user interface for the app
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title="Energy consumption",
    titleWidth=250
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon =icon("fasfa-address-book")),
      menuItem("Graphs", tabName = "graphs", icon =icon("bar-chart-o")),
      #select among granularity choices
      selectInput(inputId = "Granularity", label = "Select a granularity",
                  choices=c("Day","Week","Month")),
      #select variable
      selectInput(inputId = "Variable", label = "Select a variable",
                  choices=cols)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets", box(DT::dataTableOutput("datasetTable"),width=10)),
      tabItem(tabName = "graphs", box()),
      tabItem(tabName = "Variable", box()),
      tabItem(tabName = "text", box())
    )
  )
)
#server - instructions on how to build and rebuild the R objects that are displayed in the UI
server <- function(input, output) {
  # Get the name of the selected dataset
  get_granularity <- reactive({
    switch(input$Granularity,
           "Day" = data_bydays,
           "Week" = data_byweeks,
           "Month" = data_bymonths
    )
  })
  # Prepare the dataset
  filteredData<-reactive({
    # dplyr select varaible X and the "dynamic" one
    get_granularity() %>% select(X, Variable=input$Variable)
  })
  # Print the table
  output$datasetTable <- renderDataTable ({
    final_data<-filteredData()
  })
}
shinyApp(ui, server)



#Print the plot
output$plot <- renderHighchart ({
  data_plot<-filteredData()
  hchart(data_plot, "line",hcaes(x=X, y=Variable))
})
shinyApp(ui, server)








#### 7. Add infobox ####
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title="Energy consumption",
    titleWidth=250
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon =icon("fasfa-address-book")),
      menuItem("Graphs", tabName = "graphs", icon =icon("bar-chart-o")),
      menuItem("Text", tabName = "text", icon = icon("line-chart")),
      selectInput(inputId = "Granularity", label = "Select a granularity",
                  choices=c("Day","Week","Month")),
      selectInput(inputId = "Variable", label = "Select a variable",
                  choices=c("ActiveEnergy","ReactiveEnergy","Kitchen","Laundry","EWAC"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets", box(DT::dataTableOutput("datasetTable"),width=10)),
      tabItem(tabName = "graphs", box(highchartOutput("plot"))),
      tabItem(tabName = "text",
              # First row
              fluidRow(
                box(textOutput("messages"))),
              # Second row
              fluidRow(
                box(infoBoxOutput(width = 6, "box")))
      )
    )
  )
)
server <- function(input, output) {
  # Extract the name of the selected dataset
  get.granularity <- reactive({
    switch(input$Granularity,
           "Day" = data_bydays,
           "Week" = data_byweeks,
           "Month" = data_bymonths)
  })
  # Prepare the dataset
  filteredData<-reactive({
    get.granularity() %>% select(Variable=input$Variable, X)
  })
  # Print the table
  output$datasetTable <- renderDataTable ({
    final_data<-filteredData()
  })
  # Print the plot
  output$plot <- renderHighchart ({
    data_plot<-filteredData()
    hchart(data_plot, "line",hcaes(x=X, y=Variable))
  })
  # Print the messages
  output$messages<- renderText({
    data_text<-filteredData()
    print(paste0("You have consumed a mean of ", round(mean(data_text$Variable)), " watts"))
  })
  # Infobox
  output$box <- renderInfoBox({
    data_text<-filteredData()
    infoBox(
      "Information", paste0(round(mean(data_text$Variable)*0.00014), " euros"), icon = icon("list"),
      color = "red", fill = TRUE)
  })
}
shinyApp(ui, server)





#df_month_SMA3 <- SMA(df_month_s ,n=3)
#plot.ts(df_month_SMA3)



