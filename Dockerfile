FROM continuumio/miniconda3

RUN conda install -y r-devtools
COPY . /home/docker/app
RUN chmod 777 /home/docker/app/install-seurat.R
RUN chmod 777 /home/docker/app/run-seurat.R
RUN  /home/docker/app/install-seurat.R

WORKDIR /home/docker/app

CMD /home/docker/app/run-seurat.R
