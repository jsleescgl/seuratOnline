tabItem(tabName = "finishTab",
        fluidRow(
          column(12,
                 h3(strong("Download R Object")),
                 p("You can save the object at this point so that it can easily be loaded back in R for further analysis & exploration without having to rerun the computationally intensive steps performed above, or easily shared with collaborators."),


                 column(12,
                        column(6,
                               actionButton('generateSeuratFile','Generate Seurat Robj',class = "button button-3d button-block button-pill button-royal button-large")
                        ),
                        column(6,
                               conditionalPanel("output.seuratFileExists",
                                                downloadButton('downloadRObj','Download Seurat Obj',class = "button button-3d button-block button-pill button-action button-large"))
                        )
                 )
                 )
        )

)
