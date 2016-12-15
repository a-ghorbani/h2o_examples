FROM quantumobject/docker-rstudio
#FROM rocker/rstudio

MAINTAINER "Asghar Ghorbani" ghorbani59@gmail.com

ENV cran_repo "http://cran.wu.ac.at/"


## Install prerequisite of h2o 
# partially taken from https://github.com/h2oai/h2o-3/blob/master/Dockerfile

RUN \
  echo 'DPkg::Post-Invoke {"/bin/rm -f /var/cache/apt/archives/*.deb || true";};' | tee /etc/apt/apt.conf.d/no-cache && \
  apt-get update -q -y && \
  apt-get dist-upgrade -y && \
  apt-get clean  && \
  rm -rf /var/cache/apt/* 

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y  \
 libxml2-dev \
 python-matplotlib \
 python-numpy \
 python-pip \
 python-pandas \
 python-sklearn \
 python-software-properties \ 
 software-properties-common \
 unzip \
 wget && \
 apt-get clean

# Install Oracle Java 7
RUN add-apt-repository -y ppa:webupd8team/java && \
    apt-get update -q && \
    echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y oracle-java7-installer && \
    apt-get clean 

# Install packages that H2O depends on.
RUN Rscript -e "if (! ('methods'  %in% rownames(installed.packages()))) { install.packages('methods',  repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('statmod'  %in% rownames(installed.packages()))) { install.packages('statmod',  repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('stats'    %in% rownames(installed.packages()))) { install.packages('stats',    repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('graphics' %in% rownames(installed.packages()))) { install.packages('graphics', repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('RCurl'    %in% rownames(installed.packages()))) { install.packages('RCurl',    repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('jsonlite' %in% rownames(installed.packages()))) { install.packages('jsonlite', repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('tools'    %in% rownames(installed.packages()))) { install.packages('tools',    repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('tools'    %in% rownames(installed.packages()))) { install.packages('ggplot2',  repos='$cran_repo' ) }" && \
    Rscript -e "if (! ('utils'    %in% rownames(installed.packages()))) { install.packages('utils',    repos='$cran_repo' ) }"

# Fetch h2o latest_stable
RUN \
  cd /tmp && \
  # wget http://h2o-release.s3.amazonaws.com/h2o/latest_stable -O latest && \
  # wget --no-check-certificate -i latest -O h2o.zip && \
  wget --no-check-certificate http://h2o-release.s3.amazonaws.com/h2o/master/3717/h2o-3.11.0.3717.zip -O h2o.zip && \
  unzip -d /tmp /tmp/h2o.zip && \
  rm /tmp/h2o.zip && \
  Rscript -e "install.packages('`find /tmp/ -name h2o*tar.gz`', repos=NULL, type = 'source')"


# Prepair R to run the examples 
RUN apt-get install -y libgtk2.0-dev && apt-get clean 

RUN Rscript -e "install.packages(c('base64enc'   , 'tibble'  , 'rmarkdown', 'markdown' , 'caTools'),   repos='$cran_repo') " && \
    Rscript -e "install.packages(c('evaluate'    , 'digest'  , 'formatR'  , 'highr'    , 'knitr'  ),   repos='$cran_repo') " && \
    Rscript -e "install.packages(c('stringr'     , 'yaml'    , 'Rcpp'     , 'htmltools', 'rattle' ),   repos='$cran_repo') " && \
    Rscript -e "install.packages(c('rpart.plot'  , 'partykit', 'caret'    , 'party'    , 'pROC'   ),   repos='$cran_repo') " && \
    Rscript -e "install.packages(c('RColorBrewer', 'rBayesianOptimization' ),   repos='$cran_repo') " 

USER guest
RUN cd /home/guest && \
    wget https://codeload.github.com/a-ghorbani/h2o_examples/zip/master -O h2o_examples.zip  && \
    unzip h2o_examples.zip && \
    rm h2o_examples.zip
USER root 

EXPOSE 8787
EXPOSE 54321
EXPOSE 8080

