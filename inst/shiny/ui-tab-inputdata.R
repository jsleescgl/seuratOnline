tabItem(tabName = "datainput",
         hr(),
         fluidRow(column(4,wellPanel(
           #downloadLink("instructionspdf",label="Download Instructions (pdf)"),
           radioButtons('data_file_type','Use example file or upload your own data',
                        c('Upload Data (10X)'="upload10x",
                          #'Upload Data (Dropseq)'="uploadDropseq",
                          'Upload Data (nonUMI)'="uploadNonUmi",
                          'Example Data'="examplecounts"
                        ),selected = "upload"),
           conditionalPanel(condition="input.data_file_type=='upload10x'",
                            p("10X Data, 1 .mtx file, and 2 .tsv files")
                            #fileInput('datafile', 'Choose File Containing Data (.mtx, .tsv)', multiple = TRUE)
           ),
           
           conditionalPanel(condition="input.data_file_type=='uploadNonUmi'",
                            p("CSV counts file")
                            
           ),
           conditionalPanel(condition = "input.data_file_type=='uploadNonUmi' || input.data_file_type=='upload10x'",
                            fileInput('datafile', 'Choose File Containing Data (.CSV)', multiple = TRUE)
                            ),
           
           conditionalPanel("output.fileUploaded",
                            hr(),
                            textInput("projectname", 
                                      value = "Project1", label = "Project Name"),
                            numericInput("mincells",
                                         label="Minimum number of cells per gene",min=1,max=200,value=3),
                            numericInput("mingenes",
                                         label="Minimum number of genes per cell",min=1,max=Inf,value=200),
                            actionButton("upload_data","Next Step: QC & Filter Cells", class = "button button-3d button-block button-pill button-caution", style = "width: 100%"))
         )
         ),#column
         column(8,
                bsCollapse(id="input_collapse_panel",open="data_panel",multiple = FALSE,
                           bsCollapsePanel(title="Data Contents Table:",value="data_panel",
                                           textOutput("inputInfo"),
                                           withSpinner(dataTableOutput('countdataDT'))                       
                           )
                )#bscollapse
         )#column
         )#fluidrow
)#tabpanel
