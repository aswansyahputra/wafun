FROM rocker/binder:3.6.3
LABEL maintainer='Muhammad Aswan Syahputra'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/aswansyahputra/wafun/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-07-21/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
