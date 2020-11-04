FROM rocker/rstudio:4.0.3

LABEL maintainer_email="vrukavina7@gmail.com"
LABEL maintainer_name="Vedran Rukavina"

# install needed system packages
RUN apt-get -y install xml2 openssl

# create an R user
ENV USER rstudio

## Copy requirements.txt
COPY requirements.txt /tmp/requirements.txt
## Copy pkg_install_or_exit.R
COPY pkg_install_or_exit.R /tmp/pkg_install_or_exit.R

## install required packages in container
RUN Rscript --no-save /tmp/pkg_install_or_exit.R /tmp/requirements.txt
