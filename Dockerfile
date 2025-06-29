# Gunakan base image Shiny dari rocker
FROM rocker/shiny:latest

# Install dependensi sistem tambahan
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

# Install paket-paket R yang dibutuhkan
RUN R -e "install.packages(c( \
    'shiny', 'shinydashboard', 'plotly', 'dplyr', 'lubridate', \
    'DT', 'shinyWidgets', 'DBI', 'RPostgres' \
), repos = 'https://cloud.r-project.org')"

# Salin seluruh isi project ke direktori kerja container
COPY . /srv/app

# Pastikan file dimiliki oleh user shiny
RUN chown -R shiny:shiny /srv/app

# Set direktori kerja
WORKDIR /srv/app

# Railway menyuntikkan PORT sebagai environment variable
ENV PORT=${PORT:-3838}

# Jalankan aplikasi Shiny
CMD ["R", "-e", "print(Sys.getenv()); shiny::runApp('/srv/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]
