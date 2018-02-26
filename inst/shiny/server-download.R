
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
