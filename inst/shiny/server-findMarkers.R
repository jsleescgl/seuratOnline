
observe({
  
  if(!is.null(tsneReactive()))
  {
    pbmc = tsneReactive()$pbmc
    updateSelectizeInput(session,'clusterNum',
                         choices=levels(pbmc@ident), selected=NULL)
    updateSelectizeInput(session,'clusterNumVS1',
                         choices=levels(pbmc@ident), selected=NULL)
    updateSelectizeInput(session,'clusterNumVS2',
                         choices=levels(pbmc@ident), selected=NULL)
  }
  
})


observe({
  findClusterMarkersReactive()
})

findClusterMarkersReactive <- eventReactive(input$findClusterMarkers, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    
    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNum, min.pct = input$minPct, test.use = input$testuse, only.pos = input$onlypos)
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    return(list("clustername" = paste0("cluster",input$clusterNum),"clustermarkers"=cluster.markers))
  })
})

output$clusterMarkers <- renderDataTable({
  tmp <- findClusterMarkersReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersCSV <- downloadHandler(
  filename = function()  {paste0(findClusterMarkersReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersReactive()$clustermarkers, file, row.names=TRUE)}
)

output$clusterMarkersAvailable <-
  reactive({
    return(!is.null(findClusterMarkersReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAvailable', suspendWhenHidden=FALSE)


observe({
  findClusterMarkersVSReactive()
})

findClusterMarkersVSReactive <- eventReactive(input$findClusterMarkersVS, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc
    
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNumVS1, ident.2 = input$clusterNumVS2, min.pctvs = input$minPct, test.use = input$testuseVS, only.pos = input$onlyposVS)
    
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    
    return(list("clustername" = paste0("cluster",input$clusterNumVS1,"_vs_clusters_",paste(input$clusterNumVS2, collapse = "_")),"clustermarkers"=cluster.markers))
    
  })
})

output$clusterMarkersVS <- renderDataTable({
  tmp <- findClusterMarkersVSReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersVSCSV <- downloadHandler(
  filename = function()  {paste0(findClusterMarkersVSReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersVSReactive()$clustermarkers, file, row.names=TRUE)
    }
)

output$clusterMarkersVSAvailable <-
  reactive({
    return(!is.null(findClusterMarkersVSReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersVSAvailable', suspendWhenHidden=FALSE)


# ALL MARKERS


observe({
  findClusterMarkersAllReactive()
})

findClusterMarkersAllReactive <- eventReactive(input$findClusterMarkersAll, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc
    
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    
    cluster.markers <- FindAllMarkers(object = pbmc, min.pctvs = input$minPctAll, test.use = input$testuseAll, only.pos = input$onlyposAll, logfc.threshold = input$threshAll)
    
    if(input$numGenesPerCluster > 0)
      cluster.markers %>% group_by(cluster) %>% top_n(input$numGenesPerCluster, avg_logFC)
    
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    return(list("clustername" = paste0("allClusterMarkers"),"clustermarkers"=cluster.markers))
    
  })
})

output$clusterMarkersAll <- renderDataTable({
  tmp <- findClusterMarkersAllReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersAllCSV <- downloadHandler(
  filename =  function() {paste0(findClusterMarkersAllReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersAllReactive()$clustermarkers, file, row.names=TRUE)}
)

output$clusterMarkersAllAvailable <-
  reactive({
    return(!is.null(findClusterMarkersAllReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAllAvailable', suspendWhenHidden=FALSE)
