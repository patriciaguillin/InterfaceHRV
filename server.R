# server.R

library(shiny)
library(RHRV)

shinyServer(function(input, output) {
  
  
  datasetInput <- reactive({
    
  })

  
  #LOADING FILE -> GRAPHIC
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
    
    hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
    
    hrv.data = BuildNIHR(hrv.data)
    PlotNIHR(hrv.data)
    
    #LOADING FILE -> CONSOLE
    output$summary <- renderPrint({
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    })
    
    #LOADING FILE -> DOCUMENTATION
    #(nada que mostrar por ahora)
    output$documentationLoading <- renderPrint({
      print(?CreateHRVData)
      print(?LoadBeatAscii)
      print(?BuildNIHR)
      print(?PlotNIHR)
      
    })
    
    
    #FILTERING -> AUTHOMATIC
    
    output$filtering <- renderPrint({
      hrv.data = FilterNIHR(hrv.data)
      output$filteringP <- renderPlot({
        PlotNIHR(hrv.data)
      })
    })
    
    
    
    
    #FILTERING -> MANUAL
    output$filteringM <- renderPrint({
      hrv.data = EditNIHR(hrv.data)
      #hrv.dataa = hrv.data
      output$filteringmM <- renderPlot({
        PlotNIHR(hrv.data)
      })
    })
   
 
    
    #INTERPOLATING
    output$interpolate <- renderPrint({
      hrv.data = InterpolateNIHR (hrv.data, freqhr = input$freqHR, method = input$methodInterpolation)
      output$interpolateGraphic <- renderPlot({
        hrv.data = PlotNIHR(hrv.data)
      })
    })
        
    
    #ANALYSIS -> TIME
   
    output$timeanalysisV <- renderPrint({
      hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=NULL, interval=7.8125, verbose=NULL)
      print("Size of window:") 
      print(hr$TimeAnalysis[[1]]$size)
      
      output$timeanalysis <- renderTable({
        
        hr$TimeAnalysis[[1]]
        hr$TimeAnalysis[[1]]$HRVi 
        smoke <- matrix(c(hr$TimeAnalysis[[1]]$SDNN,hr$TimeAnalysis[[1]]$SDANN,hr$TimeAnalysis[[1]]$SDNNIDX,
                          hr$TimeAnalysis[[1]]$pNN50,hr$TimeAnalysis[[1]]$SDSD,hr$TimeAnalysis[[1]]$rMSSD,
                          hr$TimeAnalysis[[1]]$IRRR,hr$TimeAnalysis[[1]]$MADRR,hr$TimeAnalysis[[1]]$TINN,
                          hr$TimeAnalysis[[1]]$HRVi),ncol=1,byrow=TRUE)
        colnames(smoke) <- c("")
        rownames(smoke) <- c("SDNN","SDANN","SDNNIDX","pNN50","SDSD","r-MSSD","IRRR","MADRR","TINN","HRV index")
        smoke
      })
    })
    
    
    #ANALYSIS -> FREQUENCY
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
    
    
    #ANALYSIS -> NON LINEAR
    #(nada que mostrar por ahora)
    
    
    
  })
  
  output$options <- renderUI({
    
  #if ascii... if RR.... if WFDB...
    
  })
  
  
  
  
  
  

  
})


