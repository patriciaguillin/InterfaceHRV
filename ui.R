# ui.R

library(shiny)
library(RHRV)

shinyUI(fluidPage(
  includeCSS("www/bootstrap.css"),
  tags$head(
    tags$style(
      HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))
  ),
  navbarPage("RHRV Project",
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
          selectInput("timeScale",
            label= "Time scale:",
            choices = c("Seconds"=1,"Tenths of a second"=0.1,"Hundredths of a second"=0.01,"Milisecond"=0.001), selected = 1),
          #helpText("Note: Seconds(1), Tenths of a second(0.1), Hundredths of a second(0.01), Milisecond(0.001)"),
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
                    tabPanel("Documentation", verbatimTextOutput("documentationLoading"))
                  )
        
                
                )
            
      )
    ) ,     
  tabPanel("Filtering",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Filtering"),
               numericInput("longF","Long",50),
               numericInput("lastF","Last",13),
               numericInput("minbpmF","Minbpm",25),
               numericInput("maxbpmF","Maxbpm",200),
               submitButton("Update View")
             ),
             # tabPanel("Authomatic", verbatimTextOutput("filtering"), plotOutput("filteringP")
             #,tabsetPanel()
             
           #),
             mainPanel(
               #plotOutput("contents")
               tabsetPanel( 
                 tabPanel("Authomatic",
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("filteringpP"),plotOutput("filteringP")),tabPanel("Console", verbatimTextOutput("filtering")),tabPanel("Documentation"))
                          
                          ),
                 tabPanel("Manual", 
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("fiteringmmM"),plotOutput("filteringmM")),tabPanel("Console", verbatimTextOutput("filteringM")),tabPanel("Documentation"))        
                          ),
                 tabPanel("Documentation", h5("What is the interpolation?"))
               )
        
               )
             
           )
  ) ,
  tabPanel("Interpolating",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Interpolating"),
               numericInput("freqHR", "Freq_HR:", 4),
               helpText("Freq_HR: Sampling frequency used in the interpolation. (Default: 4HZ)"),
               selectInput(inputId = "methodInterpolation",
                           label = "Time scale:",
                           choices = c("spline","linear"),
                           selected = "spline"
               ),
               submitButton("Update View")
               
             ),
             mainPanel(
               
               
               tabsetPanel(
                 tabPanel("Graphic", plotOutput("interpolateGraphic")),
                 tabPanel("Console", verbatimTextOutput("interpolate")),
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
               numericInput("sizeId", "Size:", 300),
               textInput("NumofbinsId", "Numofbins:", ""),
               textInput("intervalId", "Interval:", ""),
               submitButton("Update View")
     
               
             ),
             mainPanel(
               #plotOutput("contents")
               tabsetPanel(
                 tabPanel("Time", verbatimTextOutput("timeanalysisV"),tableOutput("timeanalysis")),
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
))
