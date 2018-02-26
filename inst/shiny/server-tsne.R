
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
