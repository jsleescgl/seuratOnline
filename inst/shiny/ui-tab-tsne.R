tabItem(tabName = "tsneTab", 
        
        fluidRow(
          
          column(12,
                 h3(strong("Run Non-linear dimensional reduction (tSNE)")),
                 hr(),
                 
                 wellPanel(
                  
                 column(12,
                        tags$div(class = "BoxArea2",
                                 h4(strong("Parameters")),
                                 p("Seurat continues to use tSNE as a powerful tool to visualize and explore these datasets. While we no longer advise clustering directly on tSNE components, cells within the graph-based clusters determined above should co-localize on the tSNE plot. This is because the tSNE aims to place cells with similar local neighborhoods in high-dimensional space together in low-dimensional space. As input to the tSNE, we suggest using the same PCs as input to the clustering analysis, although computing the tSNE based on scaled gene expression is also supported using the genes.use argument."),
                                 
                                 column(6,numericInput("tsnePCDim1", "Dimensions(PC) To Use (1):", value = 1)),
                                 column(6,numericInput("tsnePCDim2", "Dimensions(PC) To Use (2):", value = 10)),
                                 tags$div(class = "clearBoth")
                                 )
                        
                 ),
                 hr(),
                 column(12,
                        conditionalPanel("output.tsnePlotAvailable",
                                         h4(strong("TSNE Plot")),
                                         withSpinner(plotOutput(outputId = "tsnePlot"))
                                         
                        )
                        
                 )
                 ,
                 column(12,
                        actionButton("runTSNE","Run TSNE Reduction",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%"))
                 ,
                 tags$div(class = "clearBoth")
                 
                 
                        )
        )
          )
)