# server.R

library(shiny)
library(RHRV)

shinyServer(function(input, output) {
  
  
  datasetInput <- reactive({
    
  })
  #datasetInput <- reactive({
  #  switch(input$type,
  #         "Acii" = t_ascii,
  #         "RR" = t_rr,
  #         "WFDB" = t_wfdb,
  #         "Polar" = t_polar,
  #         "Suunto" = t_suunto,
  #         "EDF+" = t_edf)
  #})
  
  #datasetInput <- reactive({
  #  switch(input$timeScale,
  #         "1" = 1,
  #         "2" = 01,
  #         "Hundredths of a second" = 0.01,
  #         "Milisecond" = 0.001
  #         )
  #})
  
  
  output$summary <- renderPrint({
   
    #hrv.data = CreateHRVData()
    #hrv.data = SetVerbose(hrv.data, TRUE)
    #hrv.data = LoadBeatAscii(hrv.data, "example.beats")
    #hrv.data = BuildNIHR(hrv.data)
  
    
  })
  output$contents <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    print(inFile)
    if (is.null(inFile))
      return(NULL)
    x <- list(x = cars[,1], y = cars[,2])
    
    data <- read.csv(file=inFile$datapath,header=F)$V1
    print(head(data))
    hrv.data = CreateHRVData()
    hrv.data = SetVerbose(hrv.data, TRUE)
    Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
    date.in <- as.character(input$date)
    date.split <- strsplit(date.in,split="-" )
    Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
    dateTTime <- paste(Date, Time, sep = ' ')
    
    hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = input$timeScale, datetime = dateTTime)
    
    hrv.data = BuildNIHR(hrv.data)
    PlotNIHR(hrv.data)
        
  })
  
  output$options <- renderUI({
    
  #if ascii... if RR.... if WFDB...
    
  })
  
  output$filtering <- renderPrint({
    hrv.data = FilterNIHR(hrv.data)
    
  })
  
  output$filteringP <- renderPlot({
    PlotNIHR(hrv.data)
  })
  
  output$interpolate <- renderPrint({
    hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)
    
  })
  
  output$interpolateP <- renderPlot({
    
  })
  
  output$filteringM <- renderPrint({
    hrv.data = EditNIHR(hrv.data)
    #hrv.dataa = hrv.data
    output$filteringmM <- renderPlot({
      PlotNIHR(hrv.data)
    })
  })
  

  
  
  
  
  output$timeanalysis <- renderPrint({
    CreateTimeAnalysis(hrv.data)
    #factor(1:3)
  })
  
  output$fourierT <- renderPrint({
    spectrogram = CalculateSpectrogram(hrv.data, size = 300, shift = 30, sizesp = 2048) 
  })
  output$fourier <- renderPlot({
    spectrogram = PlotSpectrogram(HRVData = hrv.data, size = 300, shift = 60, sizesp = 2048)
  })
  
  output$wavelet <- renderPlot({
    spectrogram = PlotSpectrogram(HRVData = hrv.data, size = 300, shift = 60, sizesp = 2048,
                                  freqRange = c(0, 0.2))
  })
  
 

  
})


