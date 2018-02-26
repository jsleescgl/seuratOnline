
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
