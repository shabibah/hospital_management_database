FROM rocker/shiny:latest

# ───────────────────────────────────────────────
# Instal dependensi OS
# ───────────────────────────────────────────────
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpq-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# ───────────────────────────────────────────────
# Instal dependensi R
# ───────────────────────────────────────────────
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'plotly', 'dplyr', 'lubridate', 'DT', 'shinyWidgets', 'DBI', 'RPostgres'), repos='https://cloud.r-project.org')"

# ───────────────────────────────────────────────
# Salin file app.R langsung ke root Shiny app dir
# ───────────────────────────────────────────────
COPY app.R /srv/shiny-server/app.R

# ───────────────────────────────────────────────
# Set permission untuk Shiny user
# ───────────────────────────────────────────────
RUN chown -R shiny:shiny /srv/shiny-server

# ───────────────────────────────────────────────
# Buka port Shiny Server
# ───────────────────────────────────────────────
EXPOSE 3838

# ───────────────────────────────────────────────
# Jalankan Shiny Server
# ───────────────────────────────────────────────
CMD ["/usr/bin/shiny-server"]
