FROM rocker/shiny:latest

LABEL maintainer_email="vrukavina7@gmail.com"
LABEL maintainer_name="Vedran Rukavina"

# install needed system packages
RUN apt-get update && apt-get -y install xml2 openssl gsl-bin libgsl0-dev

# create an R user
ENV USER rstudio

## Copy requirements.txt
COPY requirements.txt /tmp/requirements.txt
## Copy pkg_install_or_exit.R
COPY pkg_install_or_exit.R /tmp/pkg_install_or_exit.R

## install required packages in container
RUN Rscript --no-save /tmp/pkg_install_or_exit.R /tmp/requirements.txt

# copy necessary files
COPY . ./app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
