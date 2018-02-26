# seuratOnline: R Shiny interface for Seurat single-cell analysis library


## Install:

devtools::install_github("aymanm/seuratOnline")

## Run:

```
library(seuratOnline)
seuratOnline()
```
This will run on port http://0.0.0.0:1234/ by default

***

To run on specific ip/port:

```
ip = '127.0.0.1'
portNumber = 5555
seuratOnline(ip,portNumber)
```
This will run on http://127.0.0.1:5555/
