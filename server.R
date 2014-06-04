# server.R
hrv.data <- 1
library(shiny)
library(RHRV)


static_help = function(pkg, links = tools::findHTMLlinks()) {
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    tools::Rd2HTML(pkgRdDB[[p]], paste(p, 'html', sep = '.'),
                   package = pkg, Links = links, no_links = is.null(links))
  }
}
shinyServer(function(input, output) {
  
  #TYPE FILE
  output$dist1 <- renderUI({
    lab <- switch(input$dist,
                  ascii="Time scale:", rr="Time scale:")
    ini <- switch(input$dist,
                  ascii="1", rr="1")
    if(any(input$dist==c("ascii","rr"))) {
      selectInput(inputId = "timeScale",
                  label = lab,
                  choices = c("Seconds"=1,"Tenths of a second"=0.1,"Hundredths of a second"=0.01,"Milisecond"=0.001), 
                  selected = 1)
                  
      
    }
  })
  
  output$dist2 <- renderUI({
    if(any(input$dist==c("ascii","rr"))) {
      helpText("Note: Seconds(1), Tenths of a second(0.1), Hundredths of a second(0.01), Milisecond(0.001)")
    }
  })
  
  output$dist3 <- renderUI({
    lab <- switch(input$dist,
                  ascii="Date:", rr="Date:")
    ini <- switch(input$dist,
                  ascii="2012-04-30", rr="2012-04-30")
    if(any(input$dist==c("ascii","rr"))) {
      dateInput("date", lab, value = ini, format = "dd/mm/yyyy")
      
    }
  })
  
  output$dist4 <- renderUI({
    lab <- switch(input$dist,
                  ascii="Hour:", rr="Hour:")
    ini <- switch(input$dist,
                  ascii="12", rr="12")
    if(any(input$dist==c("ascii","rr"))) {
      textInput("hourId", lab, ini) 
    }
  })
  
  output$dist5 <- renderUI({
    lab <- switch(input$dist,
                  ascii="Minute:", rr="Minute:")
    ini <- switch(input$dist,
                  ascii="00", rr="00")
    if(any(input$dist==c("ascii","rr"))) {
      textInput("minuteId", lab, ini)
    }
  })
  
  output$dist6 <- renderUI({
    lab <- switch(input$dist,
                  ascii="Second:", rr="Second:")
    ini <- switch(input$dist,
                  ascii="00", rr="00")
    if(any(input$dist==c("ascii","rr"))) {
      textInput("secondId", lab, ini) 
    }
  })
  
  output$dist7 <- renderUI({
    lab <- switch(input$dist,
                  wfdb="WFDBAnnotator:")
    ini <- switch(input$dist,
                  wfdb="atr")
    if(any(input$dist=="wfdb")) {
      textInput("wfdbAnnotator", lab, ini) 
    }
  })
  
  output$dist8 <- renderUI({
    lab <- switch(input$dist,
                  edf="AnotationType:")
    ini <- switch(input$dist,
                  edf="QRS")
    if(any(input$dist=="edf")) {
      textInput("edfAnotation", lab, ini) 
    }
  })
  
  #LOADING FILE -> GRAPHIC
  output$contents <- renderPlot({
    filename <- 'LoadBeatAscii.html'
    if(file.exists(filename) == FALSE){
      static_help("RHRV")}
    else{}
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
    
    if(any(input$dist=="ascii")) {  
      Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
      date.in <- as.character(input$date)
      date.split <- strsplit(date.in,split="-" )
      Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
      dateTTime <- paste(Date, Time, sep = ' ')
      hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="rr")) {  
      Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
      date.in <- as.character(input$date)
      date.split <- strsplit(date.in,split="-" )
      Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
      dateTTime <- paste(Date, Time, sep = ' ')
      hrv.data = LoadBeatRR(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="wfdb")) { 
      hrv.data = LoadBeatWFDB(hrv.data, RecordName= inFile$datapath, annotator = input$wfdbAnnotator)       
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="polar")) { 
      hrv.data = LoadBeatPolar(hrv.data, RecordName= inFile$datapath)        
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="suunto")) {  
      hrv.data = LoadBeatSuunto(hrv.data, RecordName= inFile$datapath)        
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="edf")) {
      hrv.data = LoadBeatEDFPlus(hrv.data, RecordName= inFile$datapath, annotationType="QRS")        
      hrv.data = BuildNIHR(hrv.data)
      PlotNIHR(hrv.data)
    }
    
    #LOADING FILE -> CONSOLE
    output$summary <- renderPrint({
      cat(">$CreateHRVData() \n")
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      if(any(input$dist=="ascii")) {  
        cat("\n>$LoadBeatAscii(hrv.data,",inFile$datapath,",",as.numeric(input$timeScale),",",dateTTime,")\n")
        input$timeScale    
        hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="rr")) {  
        cat(">$LoadBeatRR(hrva.data,",inFile$datapath,",",as.numeric(input$timeScale),",",dateTTime,")\n")
        hrv.data = LoadBeatRR(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)       
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="wfdb")) {  
        cat(">$LoadBeatWFDB(hrv.data,",inFile$datapath,",",input$wfdbAnnotator,")")
        hrv.data = LoadBeatWFDB(hrv.data, RecordName= inFile$datapath, annotator = input$wfdbAnnotator)       
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="polar")) {  
        cat(">$LoadBeatPolar(hrv.data",inFile$datapath,")")
        input$timeScale
        hrv.data = LoadBeatPolar(hrv.data, RecordName= inFile$datapath)        
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="suunto")) {  
        cat(">$LoadBeatSuunto(hrv.data",inFile$datapath,")")
        input$timeScale
        hrv.data = LoadBeatSuunto(hrv.data, RecordName= inFile$datapath)        
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="edf")) {  
        cat(">$LoadBeatEDFPlus(hrv.data",inFile$datapath,",",input$edfAnotation,")")
        input$timeScale
        hrv.data = LoadBeatEDFPlus(hrv.data, RecordName= inFile$datapath, annotationType=input$edfAnotation)        
        cat("\n>$BuildNIHR(hrv.data)\n")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>$PlotNIHR(hrv.data)\n")
        PlotNIHR(hrv.data)
      }
    })
    
    #LOADING FILE -> DOCUMENTATION
    #(nada que mostrar por ahora)
    output$documentationLoading1 <- renderUI({
     
      
      if(any(input$dist=="ascii")) {  
        includeHTML("LoadBeatAscii.html")
      }
      else if(any(input$dist=="rr")) {  
        includeHTML("LoadBeatRR.html")
      }
      else if(any(input$dist=="wfdb")) { 
        includeHTML("LoadBeatWFDB.html")
      }
      else if(any(input$dist=="polar")) { 
        includeHTML("LoadBeatPolar.html")
      }
      else if(any(input$dist=="suunto")) {  
        includeHTML("LoadBeatSuunto.html")
      }
      else if(any(input$dist=="edf")) {
        includeHTML("LoadBeatEDFPlus.html")
      }
    })
    
    #FILTERING -> AUTHOMATIC
    
    output$filteringpP <- renderPrint({
      file.name <- tempfile("filteringAuthomatic")
      sink(file=file.name)
      #long=input$longF ,last=input$lastF ,minbpm=input$minbpmF ,maxbpm=input$maxbpmF)
      hrv.data <- FilterNIHR(hrv.data, long=input$longF ,last=input$lastF ,minbpm=input$minbpmF ,maxbpm=input$maxbpmF)
      sink()
      
      
      output$filteringP <- renderPlot({
      PlotNIHR(hrv.data)
        output$filtering <- renderPrint({
          cat(">$FilterNIHR( hrv.data,",input$longF,",",input$lastF,",",input$minbpmF,",",input$maxbpmF,")")
          out.filtering.authomatic <- readLines(file.name, n = 10)
          for (i in 1:length(out.filtering.authomatic ) ){
            cat( out.filtering.authomatic[[i]], "\n")
          }
          success <- file.remove(file.name)
        })
      })
    
    
    output$documentationFiltering1 <- renderUI({
      
      includeHTML("FilterNIHR.html")
      
    })
    
    
    
    
    #FILTERING -> MANUAL
    output$fiteringmmM <- renderPrint({
      file.name <- tempfile("filteringManual")
      sink(file=file.name)
      hrv.data <- EditNIHR(hrv.data)
      sink()
      output$filteringmM <- renderPlot({
        PlotNIHR(hrv.data)
          output$filteringM <- renderPrint({
            cat(">$EditNIHR(hrv.data)")
            out.filtering.manual <- readLines(file.name, n = 100)
            for (i in 1:length(out.filtering.manual ) ){
              cat( out.filtering.manual[[i]], "\n")
            }
            success <- file.remove(file.name)
         })
      })

    })
    
    output$documentationFiltering2 <- renderUI({
      
      includeHTML("EditNIHR.html")
      
    })
    
    
   
 
    
    #INTERPOLATING
    output$interpolateGraphic <- renderPlot({
      file.name <- tempfile("interpolating")
      sink(file=file.name)
      hrv.data <- InterpolateNIHR (hrv.data, freqhr = input$freqHR, method = input$methodInterpolation)
      sink()
        PlotHR(hrv.data)
      output$interpolate <- renderPrint({
          cat(">$InterpolateNIHR( hrv.data,",input$freqHR,",",input$methodInterpolation,")\n")
          out.interpolating <- readLines(file.name, n = 100)
          for (i in 1:length(out.interpolating ) ){
            cat( out.interpolating[[i]], "\n")
          }
          success <- file.remove(file.name)
      
      })
      
    
    
    output$documentationInterpolating <- renderUI({
      
      includeHTML("InterpolateNIHR.html")
      
    })

        
    
    #ANALYSIS -> TIME
   
    output$timeanalysisvV <- renderPrint({
      file.name <- tempfile("timeanalysis")
      sink(file=file.name)
      #long=input$longF ,last=input$lastF ,minbpm=input$minbpmF ,maxbpm=input$maxbpmF)
      
      if(any(input$analysisTFunction=="intervalId")) {  
        hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=NULL, interval = as.numeric(input$valueTime), verbose=NULL)
       
      }
      else if(any(input$analysisTFunction=="numofbinsId")) {  
        hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=input$valueTime, interval = NULL, verbose=NULL)
       
      }
      
      sink()
      
      
      output$timeanalysis <- renderTable({
        
        hr$TimeAnalysis[[1]]
        hr$TimeAnalysis[[1]]$HRVi 
        smoke <- matrix(c(hr$TimeAnalysis[[1]]$SDNN,hr$TimeAnalysis[[1]]$SDANN,hr$TimeAnalysis[[1]]$SDNNIDX,
                          hr$TimeAnalysis[[1]]$pNN50,hr$TimeAnalysis[[1]]$SDSD,hr$TimeAnalysis[[1]]$rMSSD,
                          hr$TimeAnalysis[[1]]$IRRR,hr$TimeAnalysis[[1]]$MADRR,hr$TimeAnalysis[[1]]$TINN,
                          hr$TimeAnalysis[[1]]$HRVi),ncol=1,byrow=TRUE)
        colnames(smoke) <- c("Outcome")
        rownames(smoke) <- c("SDNN","SDANN","SDNNIDX","pNN50","SDSD","r-MSSD","IRRR","MADRR","TINN","HRV index")
        smoke
        
        
      })
      output$timeanalysisV <- renderPrint({
        if(any(input$analysisTFunction=="intervalId")) { 
          cat(">$CreateTimeAnalysis(hrv.data,",input$sizeId,",numofbins=NULL, interval=",input$valueTime,", verbose=NULL)\n")
        }
        else if(any(input$analysisTFunction=="numofbinsId")) {
          cat(">$CreateTimeAnalysis(hrv.data,",input$sizeId,",numofbins=",input$valueTime,", interval=NULL, verbose=NULL)\n")  
          hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=input$valueTime, interval = NULL, verbose=NULL)
          sink()
        }
        
        out.timeanalysis <- readLines(file.name, n = 100)
        for (i in 1:length(out.timeanalysis ) ){
          cat( out.timeanalysis[[i]], "\n")
        }
        success <- file.remove(file.name)
        cat("\n**Size of window: ") 
        cat(hr$TimeAnalysis[[1]]$size)
      })
    })
    
    output$documentationTimeanalysis <- renderUI({
      
      includeHTML("CreateTimeAnalysis.html")
      
    })
    
    
    #ANALYSIS -> FREQUENCY
    output$fourierT <- renderPrint({
      cat(">$CalculateSpectogram(hrv.data, size = 300, shift = 30, sizesp = 2048)\n")
      spectrogram = CalculateSpectrogram(hrv.data, size = 300, shift = 30, sizesp = 2048) 
    })
    output$fourier <- renderPlot({
      spectrogram = PlotSpectrogram(HRVData = hrv.data, size = 300, shift = 60, sizesp = 2048)
    })
    
    output$wavelet <- renderPlot({
      spectrogram = PlotSpectrogram(HRVData = hrv.data, size = 300, shift = 60, sizesp = 2048,
                                    freqRange = c(0, 0.2))
    })
    
    output$documentationFrequencyanalysis <- renderUI({
      
      includeHTML("CalculateSpectrogram.html")
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(
          '
          data-
          '
          , Sys.Date(),
          '
          .csv
          '
          , sep=
            ''
        )
      },
      content = function(con) {
        write.csv(data, con)
      }
    )

    #ANALYSIS -> NON LINEAR
    #(nada que mostrar por ahora)
    
    
    
    })
    })
  })
  

  
})


