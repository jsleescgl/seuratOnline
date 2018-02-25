tabItem(tabName = "qcFilterTab", 
        
        fluidRow(
          
          column(12,
                 h3(strong("QC & Filter (Preprocessing)")),
                 hr(),
                 
                 wellPanel(
                   
                   column(12,
                          tags$div(class = "",
                                   h4(strong("Filter Cells")),
                                   p("Seurat allows you to easily explore QC metrics and filter cells based on any user-defined criteria. In the example below, we visualize gene and molecule counts, plot their relationship, and exclude cells with a clear outlier number of genes detected as potential multiplets. Of course this is not a guaranteed method to exclude cell doublets, but we include this as an example of filtering user-defined outlier cells. We also filter cells based on the percentage of mitochondrial genes present."),
                                   hr(),
                                   tags$div(class = "BoxArea2",
                                            h4(strong("By Regular Expression:")),
                                            column(7,
                                                   wellPanel(
                                                     column(12,
                                                            column(6,
                                                                   textInput("regexFilter", "By Regex Expression", 
                                                                             placeholder = "Eg. ^MT- for genes that start with 'MT-'")
                                                            ),
                                                            column(6,
                                                                   textInput("regexFilterLabel", "Label (no spaces)", 
                                                                             placeholder = "Eg. mito")
                                                            )
                                                     )
                                                     ,
                                                     column(12,
                                                            column(6,
                                                                   actionButton("testExpr","Test Regex", class = "button-primary button-pill",style = "width: 100%;")
                                                            ),
                                                            column(6,
                                                                   conditionalPanel("input.regexFilter.length > 0 && input.regexFilterLabel.length > 0",
                                                                                    actionButton("addExpr","Add Filter", style = "width: 100%;", class = "button-action button-pill", icon("plus")))
                                                            )
                                                     )
                                                     ,
                                                     tags$div(class = "clearBoth")
                                                     ,
                                                     hr(),
                                                     wellPanel(h4(strong("Genes that match Regex")),
                                                               htmlOutput("filteredGenesFound")
                                                               ,
                                                               tags$div(class = "clearBoth")
                                                     ),
                                                     tags$div(class = "clearBoth")
                                                   )
                                            ),
                                            column(5,
                                                   wellPanel(style = "background-color:lightblue;",
                                                     h4(strong("Filter Expressions")),
                                                     selectizeInput("filterExpressions", label="",
                                                                    choices=NULL,
                                                                    multiple=TRUE)
                                                   )
                                                   
                                            ),
                                            tags$div(class = "clearBoth")
                                   ),
                                   tags$div(class = "BoxArea2",
                                            h4(strong("By Specific Genes:")),
                                            wellPanel(style = "background-color:lightblue;",
                                                      textInput("customGenesLabel", "Label (no spaces)", 
                                                                placeholder = "Eg. mito.genes"),       
                                              selectizeInput("filterSpecGenes", label="Select Genes",
                                                             choices=NULL,
                                                             multiple=TRUE,
                                                             options = list(
                                                               placeholder = 
                                                                 'Start typing gene name'
                                                             ))
                                            )
                                   )
                                   ,
                                   tags$div(class = "clearBoth")
                          )
                   )
                   ,
                   column(12,
                          actionButton("submit_data","Submit Data",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%"))
                   ,
                   tags$div(class = "clearBoth")
                   
                   
                 )
          )
        )
)