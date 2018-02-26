# seuratOnline: R Shiny interface for Seurat single-cell analysis library


## Install:

devtools::install_github("aymanm/seuratOnline")

## Run:

```
library(seuratOnline)
seuratOnline() #this will run on port 1234 by default
```

To run on specific port:

```
portNumber = 5555
seuratOnline(portNumber)
```
