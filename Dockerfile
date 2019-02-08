FROM rocker/shiny-verse:3.5.1
MAINTAINER "Noam Ross" ross@ecohealthalliance.org

RUN install2.r --error --skipinstalled \
  shinydashboard \
  optimx \
  plotly \
  shinyWidgets \
  future \
  promises

RUN installGithub.r nik01010/dashboardthemes@v1.0.2 cran/htmlwidgets@1.3
  
COPY . /srv/shiny-server/