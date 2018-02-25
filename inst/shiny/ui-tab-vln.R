tabItem(tabName = "vlnplot", 
         fluidRow(
          #column(12,
           column(12,
                  h3(strong("Vln Plot (Filter Cells)")),
                  p("Note that low.thresholds and high.thresholds are used to define a 'gate' -Inf and Inf should be used if you don't want a lower or upper
                               threshold."),
                  p("Select thresholds to filter cells"),
                  column(12,selectizeInput("subsetNames", label="Subset Names",
                                           choices=NULL,
                                           multiple=TRUE)
                  ),
                  uiOutput("vlnPlotsUI"),
                  hr(),
                  actionButton("filterCells","Filter Cells (within thesholds)",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%")
                  ),

           column(12,
                  h3(strong("Gene Plot")),
                  p("GenePlot is typically used to visualize gene-gene relationships, but can be used for anything calculated by the object, i.e. columns in
                    object@meta.data, PC scores etc.  Since there is a rare subset of cells
                    with an outlier level of high mitochondrial percentage and also low UMI
                    content, we filter these as well"),
                  withSpinner(plotOutput(outputId = "GenePlot"))
                  )
         #)
         )

)