# ui.R

library(shiny)
library(RHRV)

shinyUI(pageWithSidebar(
  headerPanel("RHRV Project"),
  sidebarPanel(
    h3("Loading files"),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    wellPanel(
      selectInput(inputId = "type",
                  label = "Select file type:",
                  choices = c("Ascii","RR","WFDB","Polar","Suunto","EDF+"),
                  selected = "Acii"
      ),
      
      uiOutput("options")
    ),
    numericInput("timeScale", "Time scale:", 1),
    #selectInput("timeScale",
    #            label= "Time scale:", 
    #            choices = c("Seconds","Tenths of a second","Hundredths of a second","Milisecond"), selected = 1),
    helpText("Note: Seconds(1), Tenths of a second(0.1), Hundredths of a second(0.01), Milisecond(0.001)"),
    #dateInput("date", "Date:", value = "2012-04-30", format = "dd/mm/yyyy"),
    textInput("datyId", "Day:", "30"),
    textInput("monthId", "Month:", "04"),
    textInput("yearId", "Year:", "2014"),
    textInput("hourId", "Hour:", "12"),
    textInput("minuteId", "Minute:", "00"),
    textInput("secondId", "Second:", "00"),
    
    submitButton("Update View")



    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Graphic", plotOutput("contents")), 
      tabPanel("Console", verbatimTextOutput("summary")), 
      tabPanel("Documentation",  h3("Documentation" )),
      tabPanel("Filtering",  verbatimTextOutput("filtering"), plotOutput("filteringP"), verbatimTextOutput("filteringM"), plotOutput("filteringmM"), 
               verbatimTextOutput("interpolate"), plotOutput("interpolateP")),
      tabPanel("Time analysis", tableOutput("timeanalysis")),
      tabPanel("Frequency analysis",  verbatimTextOutput("fourierT"),plotOutput("fourier"), plotOutput("wavelet"))
      
    )
    
            )
))