
options(shiny.maxRequestSize = 600*1024^2)


server <- function(input, output, session) {
  
  observe({
    
    shinyjs::hide(selector = "a[data-value=\"qcFilterTab\"]")
    shinyjs::hide(selector = "a[data-value=\"vlnplot\"]")
    shinyjs::hide(selector = "a[data-value=\"filterNormSelectTab\"]")
    shinyjs::hide(selector = "a[data-value=\"dispersionPlot\"]")
    shinyjs::hide(selector = "a[data-value=\"runPcaTab\"]")
    shinyjs::hide(selector = "a[data-value=\"vizPcaPlot\"]")
    shinyjs::hide(selector = "a[data-value=\"pcaPlot\"]")
    shinyjs::hide(selector = "a[data-value=\"heatmapPlot\"]")
    shinyjs::hide(selector = "a[data-value=\"jackStrawPlot\"]")
    shinyjs::hide(selector = "a[data-value=\"clusterCells\"]")
    shinyjs::hide(selector = "a[data-value=\"tsneTab\"]")
    shinyjs::hide(selector = "a[data-value=\"finishTab\"]")
    shinyjs::hide(selector = "a[data-value=\"findMarkersTab\"]")
    
    # Check if example selected, or if not then ask to upload a file.
    shiny:: validate(
      need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))), 
           message = "Please select a file")
    )
    inFile <- input$datafile
    
  })
  

  inputDataReactive <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    print("inputting data")
    
    # Check if example selected, or if not then ask to upload a file.
    shiny:: validate(
      need((input$data_file_type=="examplecounts")|(!is.null(input$datafile)), 
           message = "Please select a file")
    )
    
    
    inFile <- input$datafile
    
    if(!is.null(inFile) & input$data_file_type=="upload10x")
    {
      js$addStatusIcon()
      shiny:: validate(
        need(dim(inFile)[1] == 3, 
             message = "need 3 files, 1 .mtx and 2 .tsv")
      )
      
      filesdir = dirname(inFile[1,4])
      
      file.rename(inFile$datapath[1],paste0(filesdir,'/',inFile$name[1]))
      file.rename(inFile$datapath[2],paste0(filesdir,'/',inFile$name[2]))
      file.rename(inFile$datapath[3],paste0(filesdir,'/',inFile$name[3]))
      
      pbmc.data <- Read10X(data.dir = filesdir)
      
      return(list('data'=pbmc.data))
    }
    else if(!is.null(inFile) & input$data_file_type=="uploadNonUmi")
    {
      js$addStatusIcon("datainput","loading")
      
      # shiny:: validate(
      #   need(tools::file_ext(sub("\\?.+", "", inFile$datapath)) %in% c(".csv",".CSV") , 
      #        message = "need .csv file")
      # )
      
      pbmc.data <- read.csv(inFile$datapath[1], header=TRUE, sep=",", row.names = 1)
      
      js$addStatusIcon("datainput","done")
      return(list('data'=pbmc.data))
    }
    else if (!is.null(inFile)) {
      #js$addStatusIcon("datainput","loading")
      seqdata <- read.csv(inFile$datapath, header=TRUE, sep=",", row.names = 1)
      print('uploaded seqdata')
      if(ncol(seqdata)==1) { # if file appears not to work as csv try tsv
        seqdata <- read.tsv(inFile$datapath, header=TRUE, row.names = 1)
        print('changed to tsv, uploaded seqdata')
      }
      shiny::validate(need(ncol(seqdata)>1,
                           message="File appears to be one column. Check that it is a comma-separated (.csv) file."))
      return(list('data'=seqdata))
    }
    else{
      if(input$data_file_type=="examplecounts")
      {
        js$addStatusIcon("datainput","loading")
        pbmc.data <- Read10X(data.dir = "www/hg19/")
        
        js$addStatusIcon("datainput","done")
        return(list('data'=pbmc.data))
      }
      return(NULL)
    }
  })
  
  output$example_counts_file <- downloadHandler(filename="examplecounts_short.csv",
                                                content=function(file){
                                                  file.copy("www/examplecounts_short.csv",file)
                                                })
  
  
  output$countdataDT <- renderDataTable({
    tmp <- inputDataReactive()
    
    if(!is.null(tmp))
      as.matrix(tmp$data[,1:4])
  })
  
  output$inputInfo <- renderText({ 
    
    tmp <- inputDataReactive()
    
    if(!is.null(tmp))
    {
      outStr = paste0(
      paste("dense size: ", object.size(x = as.matrix(x = tmp$data))),
      '\n',
      paste("sparse size: ", object.size(x = tmp$data)))
      
    }
    

  })
  
  
  # check if a file has been uploaded and create output variable to report this
  output$fileUploaded <- reactive({
    
    return(!is.null(inputDataReactive()))
    
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  observe({
    initSeuratObjReactive()
  })
  
  initSeuratObjReactive <- 
    eventReactive(input$upload_data, 
                  ignoreNULL = FALSE,
                  {
                    withProgress(message = "Initializing Seurat Object, please wait",{
                      
                      
                      updateCollapse(session,id =  "input_collapse_panel", open="analysis_panel",
                                     style = list("analysis_panel" = "success",
                                                  "data_panel"="primary"))
                      
                      rawData = inputDataReactive()$data
                      js$addStatusIcon("datainput","loading")
                      
                      pbmc <- CreateSeuratObject(raw.data = rawData, min.cells = input$mincells, min.genes = input$mingenes, 
                                                 project = input$projectname)
                      
                      
                      shiny::setProgress(value = 0.8, detail = "Done.")
                      js$addStatusIcon("datainput","done")
                      shinyjs::show(selector = "a[data-value=\"qcFilterTab\"]")
                      
                      return(list('pbmc'=pbmc))
                    })})
  
  
  
  output$filteredGenesFound <- renderUI({
    myValues$exprOut
    
  })
  
  myValues <- reactiveValues()
  
  observe({
    if(input$addExpr > 0){
      
      validate(
        need( isolate(!(input$regexFilter %in% myValues$exprList)) ,
             message = "Regex already exists"),
        need( isolate(!(input$regexFilterLabel %in% names(myValues$exprList))) ,
              message = "Regex already exists")
      )
      
      test = isolate( c(input$regexFilter) )
      test = isolate( setNames(test,input$regexFilterLabel) )
      isolate({
        myValues$exprList <- c(myValues$exprList, test)
        updateSelectizeInput(session,'filterExpressions',
                             choices=myValues$exprList, selected=myValues$exprList)
      })
      
      
      updateTextInput(session, "regexFilter", value = "")
      updateTextInput(session, "regexFilterLabel", value = "")
      
      myValues$exprOut = ""
      
    }
  })
  
  observeEvent(input$filterExpressions,ignoreNULL=FALSE, {
    
    if (length(input$filterExpressions) < length(myValues$exprList))
    {
      myValues$exprList = myValues$exprList[ myValues$exprList %in% input$filterExpressions]
      updateSelectizeInput(session,'filterExpressions',
                           choices=myValues$exprList, selected=myValues$exprList)
    }
      
  })
  
  
  observe({
    
    pbmc = initSeuratObjReactive()$pbmc
    
    updateSelectizeInput(session,'filterSpecGenes',
                         choices=rownames(x = pbmc@data), server = TRUE)
  })
  
  observe({
    if(input$testExpr > 0 & isolate(input$regexFilter != "")){
      
      pbmc = initSeuratObjReactive()$pbmc
      
      test = grep(pattern = isolate(input$regexFilter), x = rownames(x = pbmc@data), value = TRUE)
      
      
      genes = unlist(lapply(test, function(x){ paste("<div class='col-sm-3'>", x, "</div>")}))
      
      genes = c("<h4>num of genes: ",length(genes),"</h4>",genes)
      myValues$exprOut <- HTML(genes)
      
    }
  })

  
  observe({
    analyzeDataReactive()
  })
  
  analyzeDataReactive <- 
    eventReactive(input$submit_data,
                  ignoreNULL = FALSE, {
                    withProgress(message = "Analyzing Single Cell data, please wait",{
                      print("analysisCountDataReactive")
                      
                      #rawData = inputDataReactive()$data
                      pbmc <- initSeuratObjReactive()$pbmc
                      
                      js$addStatusIcon("qcFilterTab","loading")
                      
                      shiny::setProgress(value = 0.3, detail = " Applying Filters ...")
                      
                      #pbmc.data <- read.csv("~/Downloads/seuratScript/Filipe_merged_Raw counts_symbols.csv", header=TRUE, sep=",", row.names = 1)
                      
                      
                      
                      
                      #######
                      
                      if(length(myValues$exprList) > 0)
                      {
                        
                        #myValues$patternGenes = myValues$exprList
                        
                        for (i in 1:length(myValues$exprList)) {
                          
                          exprPattern = myValues$exprList[i]
                          exprName = names(myValues$exprList[i])
                          
                          pattern.genes <- grep(pattern = exprPattern, x = rownames(x = pbmc@data), value = TRUE)
                          percent.pattern <- Matrix::colSums(pbmc@raw.data[pattern.genes, ])/Matrix::colSums(pbmc@raw.data)
                          #browser()
                          pbmc <- AddMetaData(object = pbmc, metadata = percent.pattern, col.name = paste("percent.",exprName, sep = ""))
                          
                        }
                      }
                      
                      if(length(input$filterSpecGenes) > 0)
                      {
                        
                          percent.pattern <- Matrix::colSums(pbmc@raw.data[input$filterSpecGenes, ])/Matrix::colSums(pbmc@raw.data)
                          
                          pbmc <- AddMetaData(object = pbmc, metadata = percent.pattern, col.name = paste0("percent.",input$customGenesLabel))
                          
                      }
                      
                      #######
                      
                      shiny::setProgress(value = 0.9, detail = "Done.")
                      
                      shinyjs::show(selector = "a[data-value=\"vlnplot\"]")
                      
                      ###update subsetNames
                      subsetNames = c("nGene")
                      
                      if(length(myValues$exprList) > 0)
                        subsetNames = c(paste("percent.",names(myValues$exprList), sep = ""), subsetNames)
                      if(length(input$filterSpecGenes) > 0)
                        subsetNames = c(paste0("percent.",input$customGenesLabel), subsetNames)
                        
                      updateSelectizeInput(session,'subsetNames',
                                           choices=subsetNames, selected=subsetNames)
                      #####
                      
                      
                      js$addStatusIcon("qcFilterTab","done")
                      
                      #updateTabItems(session, "tabs", "vlnplot")
                      js$addStatusIcon("vlnplot","next")
                      
                      return(list('pbmc'=pbmc))
                    })}
    )
  
  
  output$vlnPlotsUI <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$subsetNames))
      return()
    
    vlnsToPlot = c(input$subsetNames, "nUMI")
    pbmc = analyzeDataReactive()$pbmc
    
    outputUI = lapply(seq(length(vlnsToPlot)), function(i) {
      
      minThreshA = round(min(pbmc@meta.data[vlnsToPlot[i]]),4)
      maxThreshA = round(max(pbmc@meta.data[vlnsToPlot[i]]),4)
      
      minThresh = minThreshA - 0.05*(maxThreshA - minThreshA)
      maxThresh = maxThreshA + 0.05*(maxThreshA - minThreshA)
      
      output[[paste0("VlnPlot",i)]] <- renderPlot({
        
        lowthreshinput = input[[paste0("thresh",i)]][1]
        
        if(vlnsToPlot[i] != "nUMI"){
          VlnPlot(object = pbmc, features.plot = vlnsToPlot[i], nCol = 1)  %>%
          + geom_hline(yintercept = input[[paste0("thresh",i)]][1],color = 'red',linetype = "dashed", size = 1) %>%
          + geom_text(data=data.frame(x=1,y=input[[paste0("thresh",i)]][1]), aes(x, y), label="low.threshold", vjust=2, hjust=0,color = "red",size = 5,fontface = "bold") %>%
          + geom_hline(yintercept = input[[paste0("thresh",i)]][2],color = 'blue',linetype = "dashed", size = 1) %>%
          + geom_text(data=data.frame(x=1,y=input[[paste0("thresh",i)]][2]), aes(x, y), label="high.threshold", vjust=-1, hjust=0, color = "blue",size = 5,fontface = "bold") %>%
          + scale_y_continuous(limits=c(minThresh - 0.05*(maxThresh - minThresh),maxThresh + 0.05*(maxThresh - minThresh)))
        }
        else
          VlnPlot(object = pbmc, features.plot = vlnsToPlot[i], nCol = 1)
        
      })
      
      output[[paste0("numGenesWithinThresh",i)]] <- renderText({
        dataColumn = round(pbmc@meta.data[vlnsToPlot[i]],4)
        
        numGenes = length(dataColumn[dataColumn >= input[[paste0("thresh",i)]][1] & dataColumn <= input[[paste0("thresh",i)]][2]])
        
        HTML(paste("Number of cells within gene detection thresholds<strong>",numGenes,"</strong>out of<strong>",NROW(dataColumn),"</strong>"))
      })
      
      
      column(4,
             
             h4( strong(vlnsToPlot[i]) ),
             withSpinner(plotOutput(outputId = paste0("VlnPlot",i))),
             if(vlnsToPlot[i] != "nUMI")
             {
               wellPanel(
                 sliderInput( paste0("thresh",i), "Threshold:",
                              min = minThresh, max = maxThresh,
                              value = c(minThresh, maxThresh)),
                 htmlOutput(paste0("numGenesWithinThresh",i)   )
               )
             }
      )
      
      
    })
    
    outputUI
    
  })
  
  

  
  output$VlnPlot <- renderPlot({
    
    pbmc <- analyzeDataReactive()$pbmc
    
    featurePlots = c("nGene", "nUMI")
    if(length(myValues$exprList) > 0)
      featurePlots = c(featurePlots, paste("percent.",names(myValues$exprList), sep = ""))
    
    if(length(input$filterSpecGenes) > 0)
      featurePlots = c(featurePlots,paste0("percent.",input$customGenesLabel))
    
    VlnPlot(object = pbmc, features.plot = featurePlots, nCol = 3)
  })
  
  output$GenePlot <- renderPlot({
    
    pbmc <- analyzeDataReactive()$pbmc
    
    geneListNames = c("nGene")
    
    if(length(myValues$exprList) > 0)
      geneListNames = c(paste("percent.",names(myValues$exprList), sep = ""), geneListNames)
    if(length(input$filterSpecGenes) > 0)
      geneListNames = c(paste0("percent.",input$customGenesLabel),geneListNames)
      
    par(mfrow = c(1, length(geneListNames)))
    
    #GenePlot(object = pbmc, gene1 = "nUMI", gene2 = "percent.mito")
    for (i in 1:length(geneListNames)) {
      GenePlot(object = pbmc, gene1 = "nUMI", gene2 = geneListNames[i])
    }
  })
  
  
  
  output$highLowThresholdsUI <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$subsetNames))
      return()
    
    outputUI = lapply(seq(length(input$subsetNames)), function(i) {
      
      column(4,
             tags$div(class = "BoxArea",
                      h4( strong(input$subsetNames[i]) ),
                      textInput( paste0("lowThresh",i), "Low Threshold", value = "-Inf"),
                      textInput(paste0("highThresh",i), "High Threshold", value = "Inf"))
      )
    })
    
    outputUI
    
  })
  
  
  observe({
    filterCellsReactive()
  })
  
  filterCellsReactive <-
    eventReactive(input$filterCells,{
      withProgress(message = "Processing , please wait",{
        
        
        pbmc <- analyzeDataReactive()$pbmc
        
        js$addStatusIcon("vlnplot","loading")
        
        shiny::setProgress(value = 0.3, detail = "Filtering Cells ...")
        
        
        lowThresh = lapply(seq(length(input$subsetNames)), function(i){
            input[[paste0("thresh",i)]][1]
          })
        
        
        highThresh = lapply(seq(length(input$subsetNames)), function(i){
          input[[paste0("thresh",i)]][2]
        })
        
        lowThresh = unlist(lowThresh)
        highThresh = unlist(highThresh)
        shiny::setProgress(value = 0.6, detail = "Filtering Cells ...")
        
        pbmc <- FilterCells(object = pbmc, subset.names = input$subsetNames, 
                            low.thresholds = lowThresh, high.thresholds = highThresh)
        
        shiny::setProgress(value = 0.9, detail = "Done.")
        
        
        js$addStatusIcon("vlnplot","done")
        
        
        js$addStatusIcon("filterNormSelectTab","next")
        shinyjs::show(selector = "a[data-value=\"filterNormSelectTab\"]")
        
        return(list('pbmc'=pbmc))
        
      })
    })
  
  
  observe({
    findVariableGenesReactive()
  })
  
  findVariableGenesReactive <- 
    eventReactive(input$findVariableGenes, {
                    withProgress(message = "Processing , please wait",{
                      print("analysisCountDataReactive")
                      
                      pbmc <- filterCellsReactive()$pbmc
                      
                      
                      js$addStatusIcon("filterNormSelectTab","loading")
                      
                      
                      shiny::setProgress(value = 0.4, detail = "Normalizing Data ...")
                      
                      pbmc <- NormalizeData(object = pbmc, normalization.method = input$normMethod, 
                                            scale.factor = input$scaleFactor)
                      
                      
                      shiny::setProgress(value = 0.8, detail = "Finding Variable Genes ...")
                      
                      pbmc <- FindVariableGenes(object = pbmc, mean.function = ExpMean, dispersion.function = LogVMR, 
                                                x.low.cutoff = input$xlowcutoff, x.high.cutoff = input$xhighcutoff, 
                                                y.cutoff = input$ycutoff, do.plot = FALSE)
                      
                      print(paste("number of genes found: ", length(x = pbmc@var.genes)))
                      
                      
                      
                      shinyjs::show(selector = "a[data-value=\"runPcaTab\"]")
                      shinyjs::show(selector = "a[data-value=\"dispersionPlot\"]")
                      
                      
                      
                      varsToRegressSelect = c("nGene", "nUMI")
                      if(length(myValues$exprList) > 0)
                        varsToRegressSelect = c(varsToRegressSelect, paste("percent.",names(myValues$exprList), sep = ""))
                      
                      if(length(input$filterSpecGenes) > 0)
                        varsToRegressSelect = c(varsToRegressSelect,paste0("percent.",input$customGenesLabel))
                      
                      
                      updateSelectizeInput(session,'varsToRegress',
                                           choices=varsToRegressSelect, selected= varsToRegressSelect[varsToRegressSelect != "nGene"])
                      
                      js$addStatusIcon("dispersionPlot","loading")
                      js$addStatusIcon("filterNormSelectTab","done")
                      js$addStatusIcon("runPcaTab","next")
                      
                      
                      
                      return(list('pbmc'=pbmc))
                    })}
    )
  
  
  output$dispersionPlot <- renderPlot({
    
    pbmc <- findVariableGenesReactive()$pbmc
    
    #dispersionPlot = FindVariableGenes(object = pbmc, mean.function = ExpMean, dispersion.function = LogVMR, 
     #                                  x.low.cutoff = input$xlowcutoff, x.high.cutoff = input$xhighcutoff, 
      #                                 y.cutoff = input$ycutoff)
    varGenes = VariableGenePlot(pbmc)
    js$addStatusIcon("dispersionPlot","done")
    
    varGenes
  })
  
  output$numVarGenesFound <- renderText({ 
    
    pbmc <- findVariableGenesReactive()$pbmc
    #print(paste("number of variable genes found: ", length(x = pbmc@var.genes)))
    paste("Number of variable genes found: ", length(x = pbmc@var.genes))
  })
  
  
  observe({
    runPcaReactive()
  })
  
  runPcaReactive <- 
    eventReactive(input$runPca, {
      withProgress(message = "Processing , please wait",{
        print("Running PCA")
        
        pbmc <- findVariableGenesReactive()$pbmc
        
        js$addStatusIcon("runPcaTab","loading")
        
        shiny::setProgress(value = 0.3, detail = "Scaling Data (this might take a while)...")
        
        pbmc <- ScaleData(object = pbmc, vars.to.regress = input$varsToRegress)
        
        shiny::setProgress(value = 0.6, detail = "Performing PCA ...")
        
        pbmc <- RunPCA(object = pbmc, pc.genes = pbmc@var.genes)
        
        
        shinyjs::show(selector = "a[data-value=\"vizPcaPlot\"]")
        shinyjs::show(selector = "a[data-value=\"pcaPlot\"]")
        shinyjs::show(selector = "a[data-value=\"heatmapPlot\"]")
        shinyjs::show(selector = "a[data-value=\"jackStrawPlot\"]")
        shinyjs::show(selector = "a[data-value=\"runPcaTab\"]")
        
        js$addStatusIcon("runPcaTab","done")
        js$addStatusIcon("vizPcaPlot","done")
        js$addStatusIcon("pcaPlot","done")
        js$addStatusIcon("heatmapPlot","done")
        js$addStatusIcon("jackStrawPlot","next")
        
        updateTabItems(session, "tabs", "runPcaTab")
        
        # showTab(inputId = "inTabset", target = "vizPcaPlot")
        # showTab(inputId = "inTabset", target = "pcaPlot")
        # showTab(inputId = "inTabset", target = "heatmapPlot")
        
        # showTab(inputId = "inTabset" , target = "runPcaTab")
        # 
        # updateTabsetPanel(session, "inTabset",
        #                   selected = "dispersionPlot"
        # )
        return(list('pbmc'=pbmc))
      })}
    )
  
  
  output$pcsPrintAvailable <- reactive({
    if(is.null(runPcaReactive()$pbmc))
      return(FALSE)
    return(TRUE)
  })
  outputOptions(output, 'pcsPrintAvailable', suspendWhenHidden=FALSE)
  
  output$pcsPrint <- renderText({ 
    
    pbmc <- runPcaReactive()$pbmc
    #print(paste("number of variable genes found: ", length(x = pbmc@var.genes)))
    printStr = capture.output(PrintPCA(object = pbmc, pcs.print = 1:input$numPCs, genes.print = input$numGenes, use.full = FALSE))
    
    printStr = gsub("\\[1\\]","",printStr)
    printStr = paste(printStr, collapse = "<br>")
    
    HTML(printStr)
  })
  
  observeEvent(input$vizPca, {
    
    updateTabItems(session, "tabs", "vizPcaPlot")
  })
  
  output$vizPcaPlot <- renderPlot({
    
    pbmc <- runPcaReactive()$pbmc
    
    VizPCA(object = pbmc, pcs.use = input$pcsToPlotStart:input$pcsToPlotEnd)
  })
  
  output$pcaPlot <- renderPlot({
    
    pbmc <- runPcaReactive()$pbmc
    
    PCAPlot(object = pbmc, dim.1 = input$dim1, dim.2 = input$dim2)
  })
  
  output$heatmapPlot <- renderPlot({
    
    pbmc <- runPcaReactive()$pbmc
    
    PCHeatmap(object = pbmc, pc.use = input$pcsToUse1:input$pcsToUse2, cells.use = input$cellsToUse, do.balanced = TRUE, 
              label.columns = FALSE, use.full = FALSE)
  })
  
  
  observe({
    jackStrawReactive()
  })
  
  jackStrawReactive <- 
    eventReactive(input$jackStraw, {
      withProgress(message = "Processing , please wait",{
        print("Running JackStraw")
        
        js$addStatusIcon("jackStrawPlot","loading")
        #updateTabItems(session, "tabs", "jackStrawPlot")
        
        pbmc <- runPcaReactive()$pbmc
        
        shiny::setProgress(value = 0.3, detail = "Running JackStraw Procedure (this might take a while)...")
        
        pbmc <- JackStraw(object = pbmc, num.replicate = input$numReplicates, do.print = FALSE)
        
        shiny::setProgress(value = 0.9, detail = "Done.")
        
        
        shinyjs::show(selector = "a[data-value=\"clusterCells\"]")
        shinyjs::show(selector = "a[data-value=\"jackStrawPlot\"]")
        
        js$addStatusIcon("jackStrawPlot","done")
        js$addStatusIcon("clusterCells","next")
        
        return(list('pbmc'=pbmc))
      })}
    )
  
  
  output$pcElbowPlot <- renderPlot({
    
    pbmc <- jackStrawReactive()$pbmc
    
    PCElbowPlot(object = pbmc)
  })
  
  output$jackStrawPlot <- renderPlot({
    
    pbmc <- jackStrawReactive()$pbmc
    
    JackStrawPlot(object = pbmc, PCs = input$jsPcsToPlot1:input$jsPcsToPlot2)
  })
  
  output$jackStrawPlotAvailable <- reactive({
    if(is.null(jackStrawReactive()$pbmc))
      return(FALSE)
    return(TRUE)
  })
  outputOptions(output, 'jackStrawPlotAvailable', suspendWhenHidden=FALSE)
  

  
  observe({
    clusterCellsReactive()
  })
  
  clusterCellsReactive <- 
    eventReactive(input$clusterCells, {
      withProgress(message = "Processing , please wait",{
        print("Finding Clusters")
        
        pbmc <- jackStrawReactive()$pbmc
        
        js$addStatusIcon("clusterCells","loading")
        
        shiny::setProgress(value = 0.4, detail = "Finding Cell Clusters ...")
        
        pbmc <- FindClusters(object = pbmc, reduction.type = input$reducType, dims.use = input$clustPCDim1:input$clustPCDim2, 
                             resolution = input$clustResolution, print.output = 0, save.SNN = TRUE)
        
        shinyjs::show(selector = "a[data-value=\"tsneTab\"]")
        shinyjs::show(selector = "a[data-value=\"clusterCells\"]")
        
        js$addStatusIcon("clusterCells","done")
        js$addStatusIcon("tsneTab","next")

        return(list('pbmc'=pbmc))
      })}
    )
  
  
  output$clustParamsAvailable <- reactive({
    if(is.null(clusterCellsReactive()$pbmc))
      return(FALSE)
    return(TRUE)
  })
  outputOptions(output, 'clustParamsAvailable', suspendWhenHidden=FALSE)
  
  output$clustParamsPrint <- renderText({ 
    
    pbmc <- clusterCellsReactive()$pbmc
    
    printStr = capture.output(PrintFindClustersParams(object = pbmc))
    
    printStr = gsub("\\[1\\]","",printStr)
    printStr = paste(printStr, collapse = "<br>")
    
    HTML(printStr)
  })
  
  
  
  observe({
    tsneReactive()
  })
  
  tsneReactive <- 
    eventReactive(input$runTSNE, {
      withProgress(message = "Processing , please wait",{
        print("Running TSNE")
        
        js$addStatusIcon("tsneTab","loading")
        
        pbmc <- clusterCellsReactive()$pbmc
        
        shiny::setProgress(value = 0.4, detail = "Running TSNE Reduction ...")
        
        pbmc <- RunTSNE(object = pbmc, dims.use = input$tsnePCDim1:input$tsnePCDim2, do.fast = TRUE)
        
        shiny::setProgress(value = 0.9, detail = "Done.")
        
        
        shinyjs::show(selector = "a[data-value=\"finishTab\"]")
        shinyjs::show(selector = "a[data-value=\"findMarkersTab\"]")
        shinyjs::show(selector = "a[data-value=\"tsneTab\"]")
        
        
        js$addStatusIcon("tsneTab","done")
        js$addStatusIcon("finishTab","download")
        js$addStatusIcon("findMarkersTab","next")
        
        return(list('pbmc'=pbmc))
      })}
    )
  
  
  output$tsnePlot <- renderPlot({
    
    pbmc <- tsneReactive()$pbmc
    
    TSNEPlot(object = pbmc)
  })
  
  output$tsnePlotAvailable <- reactive({
    return(!is.null(tsneReactive()$pbmc))
  })
  outputOptions(output, 'tsnePlotAvailable', suspendWhenHidden=FALSE)
  
  
  output$downloadRObj <- downloadHandler(
    filename = function() {
      
      paste(input$projectname,"-", Sys.Date(), '.Robj', sep='')
    },
    content = function(file) {
      pbmc = tsneReactive()$pbmc
      
      js$addStatusIcon("finishTab","done")
      save(pbmc, file = file)
    }
  )
  
  source("server-findMarkers.R",local = TRUE)
  
  
  
}