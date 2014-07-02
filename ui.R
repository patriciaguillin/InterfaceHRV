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
        
        selectInput("dist","File type:",
                    list("Ascii"="ascii","RR"="rr","WFDB"="wfdb","Polar"="polar","Suunto"="suunto","EDF+"="edf"),
                    selected = "ascii"),
        #,"Chi-square"="chisq","Log-normal"="lnorm","Beta"="beta"
        tags$hr(),
        uiOutput("distFileAscii"),
        uiOutput("distFileRR"),
        uiOutput("distFileWFDB"),
        uiOutput("distFilePolar"),
        uiOutput("distFileSuunto"),
        uiOutput("distFileEDF"),
        uiOutput("dist1"),
        uiOutput("dist2"),
        uiOutput("dist3"),
        uiOutput("dist4"),
        uiOutput("dist5"),
        uiOutput("dist6"),
        uiOutput("dist7"),
        uiOutput("dist8")
        
      ),
      
      
      mainPanel(
        #plotOutput("contents")
        
        tabsetPanel(
          tabPanel("Graphic", plotOutput("contents"), value="loadingGraphicTab"),
          tabPanel("Console", verbatimTextOutput("summary"), value="loagingConsoleTab"),
          tabPanel("Documentation", uiOutput("documentationLoading1"), value="loadingDocumentationTab"),
          id = "loadingTab"
          
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
                          tabsetPanel(tabPanel("Graphic",plotOutput("filteringP")),tabPanel("Console", verbatimTextOutput("filtering")),tabPanel("Documentation",uiOutput("documentationFiltering1"))
                          
                          ), value="filteringAuthoTab"),
                 tabPanel("Manual", 
                          tabsetPanel(tabPanel("Graphic", verbatimTextOutput("fiteringmmM"),plotOutput("filteringmM")),tabPanel("Console", verbatimTextOutput("filteringM")),tabPanel("Documentation",uiOutput("documentationFiltering2"))        
                          ),value="filteringManualTab"),
                 id = "filteringTab"
        
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
                           label = "Method:",
                           choices = c("spline","linear"),
                           selected = "spline"
               )
               
             ),
             mainPanel(
               
               
               tabsetPanel(
                 tabPanel("Graphic", plotOutput("interpolateGraphic"), value = "interpolatingGraphicTab"),
                 tabPanel("Console", verbatimTextOutput("interpolate"), value = "interpolatingConsoleTab"),
                 tabPanel("Documentation",uiOutput("documentationInterpolating"), value = "interpolatingDocumentationTab"),
                 id = "interpolatingTab"
               )
               
             )
             
           )
  ) ,
  tabPanel("Time Analysis",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Time Analysis"),
               numericInput("sizeId", "Size:", 300),
               selectInput("analysisTFunction","",
                           list("Interval"="intervalId","Numofbins"="numofbinsId"),
                           selected = "intervalId"),
               numericInput("valueTime", "", 7.8125)
              # submitButton("Update View")
     
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Graphic",  br(), br(), br(),tableOutput("timeanalysis"), value = "timeAnalysisGraphicTab"),
                 tabPanel("Console", verbatimTextOutput("timeanalysisV"), value = "timeAnalysisConsoleTab"),
                 tabPanel("Documentation",uiOutput("documentationTimeanalysis"), value = "timeAnalysisDocumentationTab"),
                 id = "timeAnalysisTab"
                 )     
              
             )
           
        
             )
             
           
  ) ,
  tabPanel("Frequency Analysis",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Frequency Analysis"),
               
              #SPECTROGRAM OPTIONS
               conditionalPanel(
                   condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                   numericInput("sizeSpectrogram", "Size:", 300)
                 ) , 
              conditionalPanel(
                   condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                   numericInput("shiftSpectrogram", "Shift:", 10)
                 ),
              conditionalPanel(
                   condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                   numericInput("sizespSpectrogram", "Sizesp:", 1024)
                 ),
              conditionalPanel(
                   condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                   textInput("scaleSpectrogram", "Scale:", "linear")
                 ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                numericInput("freqRangeMinSpectogram", "FreqRangeMin:", 0)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencySpectrogramTab'",
                numericInput("freqRangeMaxSpectogram", "FreqRangeMax:", 2)
              ),
              
               #FOURIER OPTIONS
              
              conditionalPanel(
                 condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                 numericInput("sizeFourier", "Size:", 300)
               ) , 
              conditionalPanel(
                 condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                 numericInput("shiftFourier", "Shift:", 10)
               ),
              conditionalPanel(
                 condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                 numericInput("sizespFourier", "Sizesp:", 1024)
               ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                numericInput("ulfmin","ULFmin:",0)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("ulfmax","ULFmax:",0.03)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("vlfmin","VLFmin:",0.03)
              ),
              conditionalPanel(
                  condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("vlfmax","VLFmax:",0.05)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("lfmin","LFmim:",0.05)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("lfmax","LFmax:",0.15)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("hfmin","HFmin:",0.15)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                  numericInput("hfmax","HFmax:",0.4)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                h5("PlotPowerBand options:")
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                numericInput("ymax","Ymax:",200)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyFourierTab'",
                numericInput("ymaxratio","Ymaxratio:",1.7)
              ),
              
              #WAVELET OPTIONS
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
              selectInput("waveletW","Wavelet:",
                          list("Haar"="haar","d4"="d4","d6"="d6","d8"="d8","d16"="d16","la8"="la8","la16"="la16","la20"="la20","bl14"="bl14","bl20"="bl20"),
                          selected = "d4")
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("bandtoleranceW","Bandtolerance:",0.01)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                selectInput("relativeW","Relative:",
                            list("Relative"="TRUE","Absolute"="FALSE"),
                            selected = "FALSE")
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("ulfminW","ULFmin:",0)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("ulfmaxW","ULFmax:",0.03)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("vlfminW","VLFmin:",0.03)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("vlfmaxW","VLFmax:",0.05)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("lfminW","LFmim:",0.05)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("lfmaxW","LFmax:",0.15)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("hfminW","HFmin:",0.15)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("hfmaxW","HFmax:",0.4)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                h5("PlotPowerBand options:")
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("ymaxWavelet","Ymax:",700)
              ),
              conditionalPanel(
                condition = "input.frequencyAnalysisTab == 'frequencyWaveletTab'",
                numericInput("ymaxratioWavelet","Ymaxratio:",50)
              )
              #submitButton("Update View")

               
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Spectrogram",tabsetPanel(tabPanel("Graphic", plotOutput("spectrogramP")),tabPanel("Console", verbatimTextOutput("spectrogram")),tabPanel("Documentation",uiOutput("documentationSpectrogram")),id="spectrogramTab")    , value="frequencySpectrogramTab"                             
                 ),
                 tabPanel("Fourier",tabsetPanel(tabPanel("Graphic", plotOutput("fourierP")),tabPanel("Console", verbatimTextOutput("fourierC")),tabPanel("Documentation",uiOutput("documentationFourier"),id="fourierTab"))    , value="frequencyFourierTab"                                                         
                 ),
                 tabPanel("Wavelet",tabsetPanel(tabPanel("Graphic", plotOutput("waveletP")),tabPanel("Console", verbatimTextOutput("waveletC")),tabPanel("Documentation",uiOutput("documentationWavelet"),id="waveletTab")), value="frequencyWaveletTab"                     
                 ),  
                 id = "frequencyAnalysisTab"
                 )
               
            #   tabsetPanel(tabPanel("Graphic",plotOutput("fourier"), plotOutput("wavelet")),
             #              tabPanel("Console", verbatimTextOutput("fourierT")),
              #             tabPanel("Documentation",uiOutput("documentationFrequencyanalysis"))        
               # )

             )
           )
           
  ) ,
  
  tabPanel("Summary",      
           pageWithSidebar(
             headerPanel("RHRV Project"),
             sidebarPanel(
               h3("Summary"),
               downloadButton("downloadPDF", "Download shiny PDF report")
             ),
             mainPanel(
               h3("Summary"),
               plotOutput("contentsSummary"),
               h3("Time Analysis"),
               tableOutput("timeAnalysisSummary"),
               h3("Fourier Transform"),
               plotOutput("fourierSummary"),
               h3("Wavelet Transform"),
               plotOutput("waveletSummary")
               
             )
             
           )
  )
  )
))
