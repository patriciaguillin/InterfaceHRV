# ui.R

library(shiny)
library(RHRV)

shinyUI(navbarPage("RHRV Project",
  tabPanel("Loading File",      
    pageWithSidebar(
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
                dateInput("date", "Date:", value = "2012-04-30", format = "dd/mm/yyyy"),
                #textInput("datyId", "Day:", "30"),
                #textInput("monthId", "Month:", "04"),
                #textInput("yearId", "Year:", "2014"),
                textInput("hourId", "Hour:", "12"),
                textInput("minuteId", "Minute:", "00"),
                textInput("secondId", "Second:", "00"),
                
                submitButton("Update View")
            
            
            
                ),
              mainPanel(
                #plotOutput("contents")
                tabsetPanel(
                    tabPanel("Graphic", plotOutput("contents")),
                    tabPanel("Console", verbatimTextOutput("summary")),
                    tabPanel("Documentation", h3("Documentation" ))
                  )
        
                
                )
            
      )
    ) ,     
  tabPanel("Filtering",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Filtering")
               
             ),
             mainPanel(
               #plotOutput("contents")
               tabsetPanel(
                 tabPanel("Authomatic", verbatimTextOutput("filtering"), plotOutput("filteringP")),
                 tabPanel("Manual", verbatimTextOutput("filteringM"), plotOutput("filteringmM")),
                 tabPanel("Documentation", h3("Documentation" ))
               )
        
               )
             
           )
  ) ,
  tabPanel("Analysis",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Analysis"),
               textInput("sizeId", "Size:", ""),
               textInput("NumofbinsId", "Numofbins:", ""),
               textInput("intervalId", "Interval:", "")
     
               
             ),
             mainPanel(
               #plotOutput("contents")
               tabsetPanel(
                 tabPanel("Time", verbatimTextOutput("timeanalysis")),
                 tabPanel("Frequency", verbatimTextOutput("fourierT"),plotOutput("fourier"), plotOutput("wavelet")),
                 tabPanel("Non Linear", h3("Documentation" ))
               )
        
             )
             
           )
  ) ,
  tabPanel("Summary",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Summary")
            
             ),
             mainPanel(
               #plotOutput("contents")
               h3("Summary")
               
             )
             
           )
  )
  )
)
