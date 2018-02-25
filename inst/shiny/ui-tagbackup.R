library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(plotly)

ui <- function(title,windowTitle=title){
  tagList(
    tags$head(
      tags$title(windowTitle)
    )
  )
}

# collects all of the tab UIs
#shinyUI(
#

tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
    tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
    tags$style(".BoxArea2 { padding:19px; margin: 5px; border: 3px solid; border-color:#6d9cce; border-radius:10px;}"),
    tags$style(".BoxArea { padding:19px; margin: 5px; border: 3px solid; border-color:rgba(24, 188, 156, 1); border-radius:10px; background-color:#fdfdfd; opacity: 0.90;}"),
    tags$style(".clearBoth{ clear:both; }"),
    tags$style("a > .highlight { background-color:#fdfdfd;}")
  ),
  navbarPage(
    useShinyjs(),
    theme = "https://bootswatch.com/3/spacelab/bootstrap.min.css",
    inverse = TRUE,
    #United theme from http://bootswatch.com/
    #customHeaderPanel(title="START: RNAseq Analysis and Visualization Resource"),#img(src="KCardio_CMYK_4C_pos_small.jpg",height=50,width= 92,align="right")	,
    title = "Seurat Online",
    #tabPanel("Start", mainPanel(
    tabsetPanel(id = "inTabset",
                source("ui-tab-inputdata.R",local=TRUE)$value,
                source("ui-tab-vln.R",local = TRUE)$value,
                source("ui-tab-filterNormSelect.R",local = TRUE)$value,
                source("ui-tab-dispersionPlot.R",local = TRUE)$value,
                source("ui-tab-runPca.R",local = TRUE)$value,
                source("ui-tab-vizPcaPlot.R",local = TRUE)$value,
                source("ui-tab-pcaPlot.R",local = TRUE)$value,
                source("ui-tab-pcHeatmapPlot.R",local = TRUE)$value,
                source("ui-tab-jackStrawPlot.R",local = TRUE)$value
                
    ),
    #)),
    ## =========================================================================== ##
    ## DOWNLOAD DATA TABS
    ## =========================================================================== ##
    #source("ui-tab-inputdata.R",local=TRUE)$value,
    ## =========================================================================== ##
    ## Visualization TABS
    ## =========================================================================== ##
    #source("ui-tab-samplegroupplots.R",local=TRUE)$value,
    
    #end definitions of tabs, now footer
    ## ============================================================================ ##
    ## INFO TAB
    ## ============================================================================ ##   
    
    ## ==================================================================================== ##
    ## FOOTER
    ## ==================================================================================== ##              
    footer=p(hr(),p("ShinyApp created by ", strong("Ayman Yousif")," of ",align="center",width=4),
             p(("Center for Genomics and Systems Biology, NYU Abu Dhabi"),align="center",width=4),
             p(("Copyright (C) 2017, code licensed under GPLv3"),align="center",width=4)
    )
    
    ## ==================================================================================== ##
    ## end
    ## ==================================================================================== ## 
    #tags$head(includeScript("google-analytics.js"))
    ,tags$head(includeScript("www/custom.js"))
  ) #end navbarpage
) #end taglist


