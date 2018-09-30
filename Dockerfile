FROM continuumio/miniconda3

RUN conda config --add channels conda-forge && \
    conda config --add channels bioconda

RUN conda install -y r-devtools r-shiny r-seurat  r-roxygen2 r-rcpp
COPY . /home/docker/app
RUN chmod 777 /home/docker/app/install-seurat.R
RUN chmod 777 /home/docker/app/run-seurat.R
#RUN  /home/docker/app/install-seurat.R

WORKDIR /home/docker/app

#CMD /home/docker/app/run-seurat.R
