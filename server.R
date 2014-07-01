# server.R

library(shiny)
library(RHRV)
library(tools)
library(knitr)


static_help = function(pkg, links = tools::findHTMLlinks()) {
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    tools::Rd2HTML(pkgRdDB[[p]], paste(p, 'html', sep = '.'),
                   package = pkg, Links = links, no_links = is.null(links))
  }
}
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  #TYPE FILE
  output$distFileAscii <- renderUI({
    if(any(input$dist=="ascii")) {
      fileInput("fileAscii", "Choose file:", multiple=TRUE)
    }
  })
  output$distFileRR <- renderUI({
    if(any(input$dist=="rr")) {
      fileInput("fileRR", "Choose file:", multiple=TRUE)
    }
  })
  output$distFileWFDB <- renderUI({
    if(any(input$dist=="wfdb")) {
      fileInput("fileWFDB", "Choose file:", multiple=TRUE)
    }
  })
  output$distFilePolar <- renderUI({
    if(any(input$dist=="polar")) {
      fileInput("filePolar", "Choose file:", multiple=TRUE)
    }
  })
  output$distFileSuunto <- renderUI({
    if(any(input$dist=="suunto")) {
      fileInput("fileSuunto", "Choose file:", multiple=TRUE)
    }
  })
  output$distFileEDF <- renderUI({
    if(any(input$dist=="edf")) {
      fileInput("fileEDF", "Choose file:", multiple=TRUE)
    }
  })
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
  
    
    if(any(input$dist=="ascii")) {
      inFile <- input$fileAscii
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
      
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
      data2 = hrv.data
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="rr")) {
      inFile <- input$fileRR
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
      
      data <- read.csv(file=inFile$datapath,header=F)$V1
      print(head(data))
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      
      Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
      date.in <- as.character(input$date)
      date.split <- strsplit(date.in,split="-" )
      Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
      dateTTime1 <- paste(Date, Time, sep = ' ')
      hrv.data = LoadBeatRR(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime1)
      hrv.data = BuildNIHR(hrv.data)
      data2 = hrv.data
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="wfdb")) { 
      
      inFile <- input$fileWFDB
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
 
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      #inFile1 = file_path_sans_ext(inFile$datapath)
      for(i in 1:nrow(inFile)){
        aux <- paste(head(strsplit(as.character(inFile$datapath[i]),"/")[[1]],-1),collapse="/")
        nameFile <- paste(aux,inFile$name[i], sep="/")
        file.rename(inFile$datapath[i], nameFile)
      }
      hrv.data = LoadBeatWFDB(hrv.data, RecordName= file_path_sans_ext(nameFile), annotator = input$wfdbAnnotator)       
      hrv.data = BuildNIHR(hrv.data)
      data2 = hrv.data
      PlotNIHR(hrv.data)
   
    }
    else if(any(input$dist=="polar")) { 
      inFile <- input$filePolar
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
      
      data <- read.csv(file=inFile$datapath,header=F)$V1
      print(head(data))
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      
      hrv.data = LoadBeatPolar(hrv.data, RecordName= inFile$datapath)        
      hrv.data = BuildNIHR(hrv.data)
      data2 = hrv.data
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="suunto")) {  
      inFile <- input$fileSuunto
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
      
      data <- read.csv(file=inFile$datapath,header=F)$V1
      print(head(data))
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      
      hrv.data = LoadBeatSuunto(hrv.data, RecordName= inFile$datapath)        
      hrv.data = BuildNIHR(hrv.data)
      data2 = hrv.data
      PlotNIHR(hrv.data)
    }
    else if(any(input$dist=="edf")) {
      inFile <- input$fileEDF
      print(inFile)
      if (is.null(inFile))
        return(NULL)
      #x <- list(x = cars[,1], y = cars[,2])
      
      data <- read.csv(file=inFile$datapath,header=F)$V1
      print(head(data))
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      
      hrv.data = LoadBeatEDFPlus(hrv.data, RecordName= inFile$datapath, annotationType=input$edfAnotation)        
      hrv.data = BuildNIHR(hrv.data)
      data2 = hrv.data
      PlotNIHR(hrv.data)
    }
  
    
    #LOADING FILE -> CONSOLE
    output$summary <- renderPrint({
      
      cat(">hrv.data = CreateHRVData() \n")
      hrv.data = CreateHRVData()
      hrv.data = SetVerbose(hrv.data, TRUE)
      if(any(input$dist=="ascii")) { 
        Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
        date.in <- as.character(input$date)
        date.split <- strsplit(date.in,split="-" )
        Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
        dateTTime <- paste(Date, Time, sep = ' ')
        cat("\n>hrv.data = LoadBeatAscii(hrv.data, RecordName = \"",inFile$datapath,"\", scale = ",as.numeric(input$timeScale),", datetime = ",dateTTime,")\n", sep="")   
        hrv.data = LoadBeatAscii(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime)
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="rr")) {  
        Time <- paste(input$hourId,':',input$minuteId,':',input$secondId,sep = '')
        date.in <- as.character(input$date)
        date.split <- strsplit(date.in,split="-" )
        Date <- paste(date.split[[1]][3],date.split[[1]][2],date.split[[1]][1],sep="/")
        dateTTime1 <- paste(Date, Time, sep = ' ')
        cat(">hrv.data = LoadBeatRR(hrva.data, RecordName = \"",inFile$name,"\", scale = ",as.numeric(input$timeScale),", datetime = ",dateTTime1,")\n", sep="")
        hrv.data = LoadBeatRR(hrv.data, RecordName= inFile$datapath, scale = as.numeric(input$timeScale), datetime = dateTTime1)       
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="wfdb")) {  
        cat(">hrv.data = LoadBeatWFDB(hrv.data, RecordName = \"",file_path_sans_ext(nameFile),"\", annotator = \"",input$wfdbAnnotator,"\")\n", sep="")
        hrv.data = LoadBeatWFDB(hrv.data, RecordName= file_path_sans_ext(nameFile), annotator = input$wfdbAnnotator)       
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="polar")) {  
        cat(">hrv.data = LoadBeatPolar(hrv.data, RecordName = \"",inFile$name,"\")\n", sep="")
        hrv.data = LoadBeatPolar(hrv.data, RecordName= inFile$datapath)        
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="suunto")) {  
        cat(">hrv.data = LoadBeatSuunto(hrv.data, RecordName = \"",inFile$name,"\")\n", sep="")
        hrv.data = LoadBeatSuunto(hrv.data, RecordName= inFile$datapath)        
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      }
      else if(any(input$dist=="edf")) {  
        cat(">hrv.data = LoadBeatEDFPlus(hrv.data, RecordName = \"",inFile$name,"\", annotationType = ",input$edfAnotation,")\n", sep="")
        hrv.data = LoadBeatEDFPlus(hrv.data, RecordName= inFile$datapath, annotationType=input$edfAnotation)        
        cat("\n>hrv.data = BuildNIHR(hrv.data)\n", sep="")
        hrv.data = BuildNIHR(hrv.data)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
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
    
    
    output$filteringP <- renderPlot({
      hrv.data <- FilterNIHR(hrv.data, long=input$longF ,last=input$lastF ,minbpm=input$minbpmF ,maxbpm=input$maxbpmF)
      
      PlotNIHR(hrv.data)
      
      output$filtering <- renderPrint({
        cat(">hrv.data = FilterNIHR( hrv.data, long = ",input$longF,", last = ",input$lastF,", minbpm = ",input$minbpmF,", maxbpm = ",input$maxbpmF,")\n", sep="")
        hrv.data <- FilterNIHR(hrv.data, long=input$longF ,last=input$lastF ,minbpm=input$minbpmF ,maxbpm=input$maxbpmF)
        cat("\n>PlotNIHR(hrv.data)\n", sep="")
        PlotNIHR(hrv.data)
      })
    
    output$documentationFiltering1 <- renderUI({
      
      includeHTML("FilterNIHR.html")
      
    })
    
    
    
    
    #FILTERING -> MANUAL
    output$fiteringmmM <- renderPrint({
      file.name <- tempfile("filteringManual")
      sink(file=file.name)
      hrv.data <<- EditNIHR(hrv.data)
      sink()
      output$filteringmM <- renderPlot({
        PlotNIHR(hrv.data)
          output$filteringM <- renderPrint({
            cat(">hrv.data = EditNIHR(hrv.data)\n", sep="")
            out.filtering.manual <- readLines(file.name, n = 100)
            for (i in 1:length(out.filtering.manual ) ){
              cat( out.filtering.manual[[i]], "\n", sep="")
            }
            success <- file.remove(file.name)
            cat("\n>PlotNIHR(hrv.data)\n", sep="")
            PlotNIHR(hrv.data)
         })
        }) 
      })
    

    
    
    output$documentationFiltering2 <- renderUI({
      
      includeHTML("EditNIHR.html")
      
    })
    
    
   
 
    
    #INTERPOLATING
    output$interpolateGraphic <- renderPlot({
      hrv.data <- InterpolateNIHR (hrv.data, freqhr = input$freqHR, method = input$methodInterpolation)
      PlotHR(hrv.data)
      output$interpolate <- renderPrint({
          cat(">hrv.data = InterpolateNIHR( hrv.data, freqhr = ",input$freqHR,", method = \"",input$methodInterpolation,"\")\n", sep="")
          hrv.data <<- InterpolateNIHR (hrv.data, freqhr = input$freqHR, method = input$methodInterpolation)
          cat("\n>PlotNIHR(hrv.data)\n", sep="")
          PlotNIHR(hrv.data)
      
      })
      
    
    
    output$documentationInterpolating <- renderUI({
      
      includeHTML("InterpolateNIHR.html")
      
    })

        
    
    #ANALYSIS -> TIME

      output$timeanalysis <- renderTable({
        if(any(input$analysisTFunction=="intervalId")) {  
          hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=NULL, interval = as.numeric(input$valueTime), verbose=NULL)
          
        }
        else if(any(input$analysisTFunction=="numofbinsId")) {  
          hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=input$valueTime, interval = NULL, verbose=NULL)
          
        }
        hr$TimeAnalysis[[1]]
        hr$TimeAnalysis[[1]]$HRVi 
        smoke <<- matrix(c(hr$TimeAnalysis[[1]]$SDNN,hr$TimeAnalysis[[1]]$SDANN,hr$TimeAnalysis[[1]]$SDNNIDX,
                          hr$TimeAnalysis[[1]]$pNN50,hr$TimeAnalysis[[1]]$SDSD,hr$TimeAnalysis[[1]]$rMSSD,
                          hr$TimeAnalysis[[1]]$IRRR,hr$TimeAnalysis[[1]]$MADRR,hr$TimeAnalysis[[1]]$TINN,
                          hr$TimeAnalysis[[1]]$HRVi),ncol=10,byrow=TRUE)
        colnames(smoke) <- c("SDNN","SDANN","SDNNIDX","pNN50","SDSD","r-MSSD","IRRR","MADRR","TINN","HRV index")
        smoke
        
        
      },include.rownames=FALSE)
      output$timeanalysisV <- renderPrint({
        if(any(input$analysisTFunction=="intervalId")) { 
          cat(">hrv.data = CreateTimeAnalysis(hrv.data, size = ",input$sizeId,", numofbins = NULL, interval = ",input$valueTime,", verbose = NULL)\n", sep="")
          hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins = NULL, interval = input$valueTime, verbose=NULL)
        }
        else if(any(input$analysisTFunction=="numofbinsId")) {
          cat(">hrv.data = CreateTimeAnalysis(hrv.data,",input$sizeId,",numofbins = ",input$valueTime,", interval = NULL, verbose = NULL)\n", sep="")  
          hr <- CreateTimeAnalysis(hrv.data, size = input$sizeId, numofbins=input$valueTime, interval = NULL, verbose=NULL)
        }
        cat("\n**Size of window: ", sep="") 
        cat(hr$TimeAnalysis[[1]]$size, sep="")
      })
    #})
    
    output$documentationTimeanalysis <- renderUI({
      
      includeHTML("CreateTimeAnalysis.html")
      
    })
    
    
    #ANALYSIS -> FREQUENCY

    #Spectrogram
    output$spectrogramP <- renderPlot({
      if (values$starting)
        return(NULL)
      hrv.data = CreateFreqAnalysis(hrv.data)
      PlotSpectrogram(hrv.data, size = input$sizeSpectrogram, shift = input$shiftSpectrogram, sizesp = input$sizespSpectrogram, scale = input$scaleSpectrogram, freqRange = c(input$freqRangeMinSpectogram,input$freqRangeMaxSpectogram))
    })
    output$spectrogram <- renderPrint({
      cat(">hrv.data = CreateFreqAnalysis(hrv.data)\n", sep="")
      hrv.data = CreateFreqAnalysis(hrv.data)
      cat("\n>PlotSpectrogram(hrv.data, size = ",input$sizeSpectrogram,", shift = ",input$shiftSpectrogram,", sizesp = ",input$sizespSpectrogram,",scale = ",input$scaleSpectrogram,"freqRange = c(",input$freqRangeMinSpectogram,",",input$freqRangeMaxSpectogram,"))\n", sep="")
      spectogram = PlotSpectrogram(hrv.data, size = input$sizeSpectrogram, shift = input$shiftSpectrogram, sizesp = input$sizespSpectrogram, scale = input$scaleSpectrogram,freqRange = c(input$freqRangeMinSpectogram,input$freqRangeMaxSpectogram)) 
    })
    output$documentationSpectrogram <- renderUI({
      includeHTML("PlotSpectrogram.html")
    })
    
    #Fourier
    output$fourierP <- renderPlot({
      hrv.data = CreateFreqAnalysis(hrv.data)
      hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size = input$sizeFourier, shift = input$shiftFourier, sizesp = input$sizespFourier, 
                         type="fourier", ULFmin = input$ulfmin, ULFmax = input$ulfmax, VLFmin = input$vlfmin, VLFmax = input$vlfmax,
                         LFmin = input$lfmin, LFmax = input$lfmax, HFmin = input$hfmin, HFmax = input$hfmax)
      data3 = hrv.data
      PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = input$ymax, ymaxratio = input$ymaxratio)
    })
    output$fourierC <- renderPrint({
      cat(">hrv.data = CreateFreqAnalysis(hrv.data)\n", sep="")
      hrv.data = CreateFreqAnalysis(hrv.data)
      cat("\n>hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size = ",input$sizeFourier,", shift = ",input$shiftFourier,", sizesp = ",input$sizespFourier,", type= \"fourier\", ULFmin = ",input$ulfmin,", ULFmax = ",input$ulfmax,", VLFmin = ",input$vlfmin,", VLFmax = ",input$vlfmax,", LFmin = ",input$lfmin,", LFmax = ",input$lfmax,", HFmin = ",input$hfmin,", HFmax = ",input$hfmax,")\n", sep="")
      hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size = input$sizeFourier, shift = input$shiftFourier, sizesp = input$sizespFourier, 
                                    type="fourier", ULFmin = input$ulfmin, ULFmax = input$ulfmax, VLFmin = input$vlfmin, VLFmax = input$vlfmax,
                                    LFmin = input$lfmin, LFmax = input$lfmax, HFmin = input$hfmin, HFmax = input$hfmax)
      cat("\n>PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = ",input$ymax,", ymaxratio = ",input$ymaxratio,")\n", sep="")
      fourierPlot = PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = input$ymax, ymaxratio = input$ymaxratio)
    })
    output$documentationFourier <- renderUI({
      includeHTML("CalculatePowerBand.html")
    })
    
    #Wavelet
    output$waveletP <- renderPlot({
      hrv.data = CreateFreqAnalysis(hrv.data)
      hrv.data  = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, type="wavelet", 
                                     wavelet=input$waveletW, bandtolerance=input$bandtoleranceW, relative = input$relativeW,
                                     ULFmin = input$ulfminW, ULFmax = input$ulfmaxW, VLFmin = input$vlfminW, VLFmax = input$vlfmaxW,
                                     LFmin = input$lfminW, LFmax = input$lfmaxW, HFmin = input$hfminW, HFmax = input$hfmaxW)
      
      PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = input$ymaxWavelet, ymaxratio = input$ymaxratioWavelet)
    })
    output$waveletC <- renderPrint({
      cat(">hrv.data = CreateFreqAnalysis(hrv.data)\n", sep="")
      hrv.data = CreateFreqAnalysis(hrv.data)
      cat("\n>hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, type= \"wavelet\", wavelet = \"",input$waveletW,"\", bandtolerance = ",input$bandtoleranceW,", relative = ",input$relativeW,", ULFmin = ",input$ulfminW,", ULFmax = ",input$ulfmaxW,", VLFmin = ",input$vlfminW,", VLFmax = ",input$vlfmaxW,", LFmin = ",input$lfminW,", LFmax = ",input$lfmaxW,", HFmin = ",input$hfminW,", HFmax = ",input$hfmaxW,")\n", sep="")
      hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, type="wavelet", 
                                    wavelet=input$waveletW, bandtolerance=input$bandtoleranceW, relative = input$relativeW, ULFmin = input$ulfminW, 
                                    ULFmax = input$ulfmaxW, VLFmin = input$vlfminW, VLFmax = input$vlfmaxW,
                                    LFmin = input$lfminW, LFmax = input$lfmaxW, HFmin = input$hfminW, HFmax = input$hfmaxW)
      cat("\n>PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = ",input$ymaxWavelet,", ymaxratio = ",input$ymaxratioWavelet,")\n", sep="")
      PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = input$ymaxWavelet, ymaxratio = input$ymaxratioWavelet)
    })
    output$documentationWavelet <- renderUI({
      includeHTML("CalculatePowerBand.html")
    })
    
    
    #Summary
    output$contentsSummary <- renderPlot({
      PlotNIHR(hrv.data)
    })
    
    output$timeAnalysisSummary <- renderTable({

      rownames(smoke) <- c("Outcome")
      colnames(smoke) <- c("SDNN","SDANN","SDNNIDX","pNN50","SDSD","r-MSSD","IRRR","MADRR","TINN","HRV index")
      smoke
    },include.rownames=FALSE)
    
    output$fourierSummary <- renderPlot({
      hrv.Fourier <<- CreateFreqAnalysis(hrv.data)
      hrv.Fourier <<- CalculatePowerBand(hrv.Fourier, indexFreqAnalysis = 1, size = input$sizeFourier, shift = input$shiftFourier, sizesp = input$sizespFourier, 
                                    type="fourier", ULFmin = input$ulfmin, ULFmax = input$ulfmax, VLFmin = input$vlfmin, VLFmax = input$vlfmax,
                                    LFmin = input$lfmin, LFmax = input$lfmax, HFmin = input$hfmin, HFmax = input$hfmax)
      
      PlotPowerBand(hrv.Fourier, indexFreqAnalysis = 1, ymax = input$ymax, ymaxratio = input$ymaxratio)
    })
    
    output$waveletSummary <- renderPlot({
      hrv.Wavelet <<- CreateFreqAnalysis(hrv.data)
      hrv.Wavelet  <<- CalculatePowerBand(hrv.Wavelet, indexFreqAnalysis = 1, type="wavelet", 
                                     wavelet=input$waveletW, bandtolerance=input$bandtoleranceW, relative = input$relativeW,
                                     ULFmin = input$ulfminW, ULFmax = input$ulfmaxW, VLFmin = input$vlfminW, VLFmax = input$vlfmaxW,
                                     LFmin = input$lfminW, LFmax = input$lfmaxW, HFmin = input$hfminW, HFmax = input$hfmaxW)
      
      PlotPowerBand(hrv.Wavelet, indexFreqAnalysis = 1, ymax = input$ymaxWavelet, ymaxratio = input$ymaxratioWavelet)
     })
    #Download
    output$downloadPDF <-
      downloadHandler(filename = "summary.pdf",
                      content = function(file){
                        knit2pdf("report.Rnw")
                        file.copy("report.pdf", file)
                        file.remove("report.pdf", "report.tex",
                                    "report.aux", "report.log")
                        unlink("figure", recursive = TRUE)
                      },
                      contentType = "application/pdf"
      )

    
    })
    })
  })


  
})


