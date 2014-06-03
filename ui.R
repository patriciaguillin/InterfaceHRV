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
          fileInput("file1", "Choose file:", multiple=TRUE),
          tags$hr(),
          selectInput("dist","File type:",
                      list("Ascii"="ascii","RR"="rr","WFDB"="wfdb","Polar"="polar","Suunto"="suunto","EDF+"="edf"),
                      selected = "ascii"),
          #,"Chi-square"="chisq","Log-normal"="lnorm","Beta"="beta"
          uiOutput("dist1"),
          uiOutput("dist2"),
          uiOutput("dist3"),
          uiOutput("dist4"),
          uiOutput("dist5"),
          uiOutput("dist6"),
          uiOutput("dist7"),
          uiOutput("dist8")
      
          #wellPanel(
           # selectInput(inputId = "type",
            #            label = "Select file type:",
             #           choices = c("Ascii","RR","WFDB","Polar","Suunto","EDF+"),
              #          selected = "Acii"
            #),
            
            #uiOutput("options")
          #),
          #selectInput("timeScale",
           # label= "Time scale:",
            #choices = c("Seconds"=1,"Tenths of a second"=0.1,"Hundredths of a second"=0.01,"Milisecond"=0.001), selected = 1),
          #helpText("Note: Seconds(1), Tenths of a second(0.1), Hundredths of a second(0.01), Milisecond(0.001)"),
          #dateInput("date", "Date:", value = "2012-04-30", format = "dd/mm/yyyy"),
          #textInput("datyId", "Day:", "30"),
          #textInput("monthId", "Month:", "04"),
          #textInput("yearId", "Year:", "2014"),
          #textInput("hourId", "Hour:", "12"),
          #textInput("minuteId", "Minute:", "00"),
          #textInput("secondId", "Second:", "00"),
          
          #submitButton("Update View")
          
        ),
            
            
              mainPanel(
                #plotOutput("contents")
                tabsetPanel(
                    tabPanel("Graphic", plotOutput("contents")),
                    tabPanel("Console", verbatimTextOutput("summary")),
                    tabPanel("Documentation", tabsetPanel(tabPanel("CreateHRVData", uiOutput("documentationLoading1")),tabPanel("LoadBeatAscii", uiOutput("documentationLoading2")), tabPanel("BuildNIHR", uiOutput("documentationLoading3")), tabPanel("PlotNIHR", uiOutput("documentationLoading4")))
                             )
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
               numericInput("maxbpmF","Maxbpm",200)
              # submitButton("Update View")
             ),
             # tabPanel("Authomatic", verbatimTextOutput("filtering"), plotOutput("filteringP")
             #,tabsetPanel()
             
           #),
             mainPanel(
               #plotOutput("contents")
               tabsetPanel( 
                 tabPanel("Authomatic",
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("filteringpP"),plotOutput("filteringP")),tabPanel("Console", verbatimTextOutput("filtering")),tabPanel("Documentation",uiOutput("documentationFiltering1"))
                          
                          )),
                 tabPanel("Manual", 
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("fiteringmmM"),plotOutput("filteringmM")),tabPanel("Console", verbatimTextOutput("filteringM")),tabPanel("Documentation",uiOutput("documentationFiltering2"))        
                          )
                  )
        
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
               )
               #submitButton("Update View")
               
             ),
             mainPanel(
               
               
               tabsetPanel(
                 tabPanel("Graphic", plotOutput("interpolateGraphic")),
                 tabPanel("Console", verbatimTextOutput("interpolate")),
                 tabPanel("Documentation",uiOutput("documentationInterpolating"))
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
               textInput("intervalId", "Interval:", "")
              # submitButton("Update View")
     
               
             ),
             mainPanel(
               tabsetPanel( 
                 tabPanel("Time",
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("timeanalysisvV"), tableOutput("timeanalysis")),tabPanel("Console", verbatimTextOutput("timeanalysisV")),tabPanel("Documentation",uiOutput("documentationTimeanalysis"))
                                      
                          )
                  ),
                 tabPanel("Frequency", 
                          tabsetPanel(tabPanel("Graphic",plotOutput("fourier"), plotOutput("wavelet")),tabPanel("Console", verbatimTextOutput("fourierT")),tabPanel("Documentation",uiOutput("documentationFrequencyanalysis"))        
                          )
                 ),
                 tabPanel("Non Linear", 
                          tabsetPanel(tabPanel("Graphic"),tabPanel("Console"),tabPanel("Documentation")        
                          )
                 )
                 
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
