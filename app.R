# ------------------------------------------- #
# ----- SANVIA MEDIKA DASHBOARD (SHINY) ----- #
# ------------------------------------------- #

# ============================
# Packages
# ============================
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(shinyWidgets)
library(DBI)
library(RPostgres)


# ============================
# Load dan Preproses Data
# ============================
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "railway",
  host     = "yamanote.proxy.rlwy.net",
  port     = 15672,
  user     = "postgres",
  password = "fwNMQdBokMQnqlgfXfNZRjZwwWHFgRsw"
)

data <- dbGetQuery(con, "SELECT * FROM hospital_management_dataset") %>%
  mutate(
    full_name = trimws(paste(patient_first_name, patient_last_name)),
    age = 2023 - year(patient_dob),
    age_group = cut(
      age,
      breaks = c(0, 18, 35, 50, 65, Inf),
      labels = c("<18", "18-35", "36-50", "51-65", ">65"),
      right = FALSE
    ),
    appointment_date = as.Date(appointment_date),
    bill_date = as.Date(bill_date),
    registration_date = as.Date(registration_date)
  ) %>%
  filter(!is.na(full_name), !is.na(age))


dbDisconnect(con)


# ============================
# UI
# ============================

ui <- dashboardPage(
  
  dashboardHeader(
    title = tagList(
      icon("hospital"),
      "Sanvia Medika"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Pasien",    tabName = "pasien",    icon = icon("user-md")),
      menuItem("Dokter",    tabName = "dokter",    icon = icon("user-nurse")),
      menuItem("Janji Temu",tabName = "janji",     icon = icon("calendar-check")),
      menuItem("Status Tagihan", tabName = "tagihan", icon = icon("file-invoice-dollar"))
    ),
    hr(),
    
    pickerInput(
      inputId = "sidebar_filter_tahun",
      label   = "Filter Tahun:",
      choices = sort(unique(year(data$appointment_date))),
      multiple = TRUE,
      options = list(
        `actions-box`     = TRUE,
        `selectAllText`   = "Pilih Semua",
        `deselectAllText` = "Batal Pilih Semua"
      )
    ),
    
    pickerInput(
      inputId = "sidebar_filter_bulan",
      label   = "Filter Bulan:",
      choices = month.name,
      multiple = TRUE,
      options = list(
        `actions-box`     = TRUE,
        `selectAllText`   = "Pilih Semua",
        `deselectAllText` = "Batal Pilih Semua"
      )
    ),
    
    pickerInput(
      inputId = "sidebar_filter_spesialis",
      label   = "Spesialisasi Dokter:",
      choices = unique(data$doctor_specialization),
      multiple = TRUE,
      options = list(
        `actions-box`     = TRUE,
        `selectAllText`   = "Pilih Semua",
        `deselectAllText` = "Batal Pilih Semua"
      )
    ),
    
    pickerInput(
      inputId = "sidebar_filter_metode",
      label   = "Metode Pembayaran:",
      choices = unique(data$payment_method),
      multiple = TRUE,
      options = list(
        `actions-box`     = TRUE,
        `selectAllText`   = "Pilih Semua",
        `deselectAllText` = "Batal Pilih Semua"
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(
        ".dropdown-menu {
           max-height: 300px !important;
           overflow-y: auto !important;
           z-index: 9999 !important;
         }"
      ))
    ),
    
    tabItems(
      # ===============================
      # Tab: Dashboard
      # ===============================
      tabItem(
        tabName = "dashboard",
        tabsetPanel(
          tabPanel("Statistik Pasien",
                   fluidRow(
                     valueBoxOutput("totalPasienBox"),
                     valueBoxOutput("totalIncomeBox"),
                     valueBoxOutput("totalJanjiBox")
                   ),
                   fluidRow(
                     box(title = "Distribusi Gender", width = 6, plotlyOutput("genderDist")),
                     box(title = "Kelompok Usia", width = 6, plotlyOutput("ageGroup"))
                   ),
                   fluidRow(
                     box(title = "Provider Asuransi", width = 12, plotlyOutput("insuranceProvider"))
                   )
          ),
          tabPanel("Statistik Dokter",
                   fluidRow(
                     box(title = "Distribusi Spesialisasi", width = 6, plotlyOutput("doctorSpecialization")),
                     box(title = "Pengalaman Dokter", width = 6, plotlyOutput("doctorExperience"))
                   ),
                   fluidRow(
                     box(title = "Sebaran Cabang", width = 12, plotlyOutput("hospitalBranch"))
                   )
          ),
          tabPanel("Analisis Janji Temu",
                   fluidRow(
                     box(title = "Janji Temu per Bulan", width = 6, plotlyOutput("apptPerMonth")),
                     box(title = "Status Janji Temu", width = 6, plotlyOutput("apptStatus"))
                   ),
                   fluidRow(
                     box(title = "Spesialisasi Dokter", width = 6, plotlyOutput("apptDoctor2")),
                     box(title = "Alasan Kunjungan", width = 6, plotlyOutput("visitReason"))
                   )
          ),
          tabPanel("Analisis Pembayaran",
                   fluidRow(
                     box(title = "Pemasukan Bulanan", width = 6, plotlyOutput("monthlyIncome2")),
                     box(title = "Rata-rata Tagihan", width = 6, plotlyOutput("avgBilling"))
                   ),
                   fluidRow(
                     box(title = "Metode Pembayaran", width = 6, plotlyOutput("paymentMethod")),
                     box(title = "Status Pembayaran", width = 6, plotlyOutput("paymentStatus"))
                   )
          ),
          tabPanel("Performa Layanan",
                   fluidRow(
                     box(title = "Perawatan Populer", width = 6, plotlyOutput("topTreatment")),
                     box(title = "Perawatan vs Pembayaran", width = 6, plotlyOutput("treatPayment"))
                   ),
                   fluidRow(
                     box(title = "Biaya per Spesialisasi", width = 12, plotlyOutput("costSpecialist"))
                   )
          ),
          tabPanel("KPI Monitoring",
                   fluidRow(
                     box(title = "Aktif vs Tidak Aktif", width = 6, plotlyOutput("activePatients")),
                     box(title = "Tagihan Gagal/Tertunda", width = 6, plotlyOutput("failedBills"))
                   ),
                   fluidRow(
                     box(title = "Rata-rata Lead Time", width = 12, plotlyOutput("leadTime"))
                   )
          )
        )
      ),
      
      # ===============================
      # Tab: Pasien
      # ===============================
      tabItem(
        tabName = "pasien",
        fluidRow(
          box(
            width = 6,
            pickerInput(
              inputId = "selected_patient",
              label = "Pilih Nama Pasien:",
              choices = unique(data$full_name),
              selected = NULL,
              multiple = FALSE,
              options = list(
                `live-search`      = TRUE,
                `noneSelectedText` = "Tidak ada yang dipilih",
                `actions-box`      = TRUE,
                `selectAllText`    = "Pilih Semua",
                `deselectAllText`  = "Batal Pilih Semua"
              )
            )
          ),
          box(
            width = 6,
            pickerInput(
              inputId = "selected_patient_id",
              label = "Pilih ID Pasien:",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = list(
                `live-search`      = TRUE,
                `noneSelectedText` = "Pilih nama terlebih dahulu",
                `actions-box`      = TRUE,
                `selectAllText`    = "Pilih Semua",
                `deselectAllText`  = "Batal Pilih Semua"
              )
            )
          )
        ),
        fluidRow(
          box(title = "Informasi Pasien", width = 12, DTOutput("pasienInfoTable"))
        ),
        fluidRow(
          box(title = "Riwayat Kunjungan", width = 12, DTOutput("riwayatKunjungan"))
        )
      ),
      
      # ===============================
      # Tab: Dokter
      # ===============================
      tabItem(
        tabName = "dokter",
        fluidRow(
          box(
            width = 12,
            pickerInput(
              inputId = "selected_doctor",
              label = "Pilih Dokter:",
              choices = unique(paste(data$doctor_first_name, data$doctor_last_name)),
              selected = NULL,  
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua",
                `live-search`     = TRUE
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Informasi Dokter",
            width = 12,
            uiOutput("dokterInfoCard")
          )
        )
      ),
      
      # ===============================
      # Tab: Janji Temu
      # ===============================
      tabItem(
        tabName = "janji",
        fluidRow(
          box(
            width = 4,
            pickerInput(
              inputId = "filter_janji_tgl",
              label   = "Tanggal:",
              choices = 1:31,
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua"
              )
            )
          ),
          box(
            width = 4,
            pickerInput(
              inputId = "filter_janji_bulan",
              label   = "Bulan:",
              choices = month.name,
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua"
              )
            )
          ),
          box(
            width = 4,
            pickerInput(
              inputId = "filter_janji_tahun",
              label   = "Tahun:",
              choices = sort(unique(year(data$appointment_date))),
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            checkboxGroupButtons(
              inputId = "dokterFilter",
              label = "Spesialisasi Dokter:",
              choices = unique(data$doctor_specialization),
              status = "primary",
              size = "sm",
              justified = TRUE,
              width = "100%"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            DTOutput("filteredAppointments")
          )
        )
      ),
      
      # ===============================
      # Tab: Status Tagihan
      # ===============================
      tabItem(
        tabName = "tagihan",
        fluidRow(
          box(
            width = 6,
            pickerInput(
              inputId = "namaPasien",
              label   = "Nama Pasien:",
              choices = unique(data$full_name),
              selected = NULL,
              multiple = TRUE,
              options = list(
                `live-search`       = TRUE,
                `noneSelectedText`  = "Semua Pasien",
                `actions-box`       = TRUE,
                `selectAllText`     = "Pilih Semua",
                `deselectAllText`   = "Batal Pilih Semua"
              )
            )
          ),
          box(
            width = 6,
            pickerInput(
              inputId = "patientId",
              label   = "ID Pasien:",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = list(
                `live-search`       = TRUE,
                `noneSelectedText`  = "Pilih Nama Terlebih Dahulu",
                `actions-box`       = TRUE
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            pickerInput(
              inputId = "filter_tagihan_bulan",
              label   = "Bulan:",
              choices = month.name,
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua"
              )
            )
          ),
          box(
            width = 6,
            pickerInput(
              inputId = "filter_tagihan_tahun",
              label   = "Tahun:",
              choices = sort(unique(year(data$bill_date))),
              multiple = TRUE,
              options = list(
                `actions-box`     = TRUE,
                `selectAllText`   = "Pilih Semua",
                `deselectAllText` = "Batal Pilih Semua"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            box(
              title = "Detail Tagihan",
              width = 12,
              style = "overflow-x: auto;",
              DTOutput("tagihanTable")
            )
          ),
          column(
            width = 4,
            box(
              title = "Distribusi Status Pembayaran",
              width = 12,
              plotlyOutput("tagihanStatusChart", height = "250px")
            ),
            box(
              title = "Metode Pembayaran",
              width = 12,
              plotlyOutput("paymentMethodChart", height = "250px")
            )
          )
        )
      )
    )
  )
)


# ============================
# Server
# ============================

server <- function(input, output, session) {
  
  # ===============================
  # Data Reaktif (Filter Sidebar)
  # ===============================
  filtered_data <- reactive({
    df <- data
    
    if (!is.null(input$sidebar_filter_bulan)) {
      df <- df %>%
        filter(month.name[month(appointment_date)] %in% input$sidebar_filter_bulan)
    }
    
    if (!is.null(input$sidebar_filter_tahun)) {
      df <- df %>%
        filter(year(appointment_date) %in% input$sidebar_filter_tahun)
    }
    
    if (!is.null(input$sidebar_filter_spesialis)) {
      df <- df %>%
        filter(doctor_specialization %in% input$sidebar_filter_spesialis)
    }
    
    if (!is.null(input$sidebar_filter_metode)) {
      df <- df %>%
        filter(payment_method %in% input$sidebar_filter_metode)
    }
    
    return(df)
  })
  
  # ===============================
  # Value Boxes (Dashboard)
  # ===============================
  output$totalPasienBox <- renderValueBox({
    valueBox(
      value    = length(unique(filtered_data()$patient_id)),
      subtitle = "Total Pasien",
      icon     = icon("users"),
      color    = "aqua"
    )
  })
  
  output$totalIncomeBox <- renderValueBox({
    valueBox(
      value = paste0(
        "$",
        format(
          round(
            sum(
              filtered_data()$treatment_amount[
                filtered_data()$payment_status == "Paid"
              ],
              na.rm = TRUE
            ),
            0
          ),
          big.mark = "."
        )
      ),
      subtitle = "Total Pemasukan (Paid)",
      icon     = icon("money-bill"),
      color    = "green"
    )
  })
  
  output$totalJanjiBox <- renderValueBox({
    valueBox(
      value    = nrow(filtered_data()),
      subtitle = "Total Janji Temu",
      icon     = icon("calendar"),
      color    = "orange"
    )
  })
  
  # ===============================
  # Statistik Pasien
  # ===============================
  output$genderDist <- renderPlotly({
    df <- filtered_data() %>%
      distinct(patient_id, patient_gender) %>%
      count(patient_gender)
    
    plot_ly(
      df,
      labels   = ~patient_gender,
      values   = ~n,
      type     = 'pie',
      textinfo = 'percent',
      textfont = list(size = 16, color = 'white'),
      marker   = list(
        colors = c("#FF69B4", "#1F77B4"),
        line   = list(color = 'white', width = 2)
      )
    )
  })
  
  output$ageGroup <- renderPlotly({
    df <- filtered_data() %>%
      distinct(patient_id, age_group) %>%
      count(age_group)
    
    plot_ly(
      df,
      x               = ~age_group,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = "#8E44AD")
    )
  })
  
  output$insuranceProvider <- renderPlotly({
    df <- filtered_data() %>%
      distinct(patient_id, insurance_provider) %>%
      count(insurance_provider) %>%
      arrange(desc(n)) %>%
      mutate(insurance_provider = factor(
        insurance_provider, levels = rev(insurance_provider)
      ))
    
    plot_ly(
      df,
      y               = ~insurance_provider,
      x               = ~n,
      type            = 'bar',
      orientation     = 'h',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = c("#C0392B", "#1E8449", "#1F77B4", "#F39C12"))
    )
  })
  
  # ===============================
  # Statistik Dokter
  # ===============================
  output$doctorSpecialization <- renderPlotly({
    df <- filtered_data() %>%
      count(doctor_specialization)
    
    plot_ly(
      df,
      x               = ~doctor_specialization,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = c("#C0392B", "#1E8449", "#8E44AD", "#1F77B4"))
    )
  })
  
  output$doctorExperience <- renderPlotly({
    df <- filtered_data() %>%
      count(doctor_experience_years)
    
    plot_ly(
      df,
      x               = ~doctor_experience_years,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = "#F39C12")
    )
  })
  
  output$hospitalBranch <- renderPlotly({
    df <- filtered_data() %>%
      count(doctor_hospital_branch)
    
    plot_ly(
      df,
      x               = ~doctor_hospital_branch,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = "#D63384")
    )
  })
  
  # ===============================
  # Analisis Janji Temu
  # ===============================
  output$apptPerMonth <- renderPlotly({
    df <- filtered_data() %>%
      mutate(month = floor_date(appointment_date, "month")) %>%
      count(month)
    
    plot_ly(
      data   = df,
      x      = ~month,
      y      = ~n,
      type   = 'scatter',
      mode   = 'lines+markers',
      line   = list(shape = "spline", color = "#1F77B4", width = 3),
      fill   = "tozeroy",
      marker = list(size = 6, color = "#1F77B4"),
      hovertemplate = paste(
        "<b>Bulan:</b> %{x|%b %Y}<br>",
        "<b>Janji Temu:</b> %{y}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Jumlah Janji Temu"),
        hovermode = "x unified"
      )
  })
  
  output$apptStatus <- renderPlotly({
    df <- filtered_data() %>%
      count(appointment_status)
    
    plot_ly(
      df,
      labels   = ~appointment_status,
      values   = ~n,
      type     = 'pie',
      hole     = 0.5,
      textinfo = 'percent',
      textfont = list(size = 16, color = 'white'),
      marker   = list(
        colors = c("#8E44AD", "#C0392B", "#1E8449", "#F39C12"),
        line   = list(color = 'white', width = 2)
      )
    )
  })
  
  output$apptDoctor2 <- renderPlotly({
    df <- filtered_data() %>%
      count(doctor_specialization)
    
    plot_ly(
      df,
      x               = ~doctor_specialization,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = c("#C0392B", "#1E8449", "#8E44AD", "#1F77B4"))
    )
  })
  
  output$visitReason <- renderPlotly({
    df <- filtered_data() %>%
      count(visit_reason) %>%
      arrange(desc(n)) %>%
      mutate(visit_reason = factor(visit_reason, levels = rev(visit_reason)))
    
    plot_ly(
      df,
      y               = ~visit_reason,
      x               = ~n,
      type            = 'bar',
      orientation     = 'h',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(
        color = c("#1E8449", "#8E44AD", "#F39C12", "#C0392B", "#D63384")
      )
    )
  })
  
  # ===============================
  # Analisis Pembayaran
  # ===============================
  output$monthlyIncome2 <- renderPlotly({
    df <- filtered_data() %>%
      filter(payment_status == "Paid") %>%
      mutate(month = floor_date(bill_date, "month")) %>%
      group_by(month) %>%
      summarise(total = sum(treatment_amount, na.rm = TRUE))
    
    plot_ly(
      df,
      x = ~month,
      y = ~total,
      type = 'bar',
      marker = list(color = "#F39C12"),
      hovertemplate = paste(
        "<b>Bulan:</b> %{x|%b %Y}<br>",
        "<b>Total:</b> $%{y:,.2f}<extra></extra>"
      )
    )
  })
  
  output$avgBilling <- renderPlotly({
    df <- filtered_data() %>%
      group_by(appointment_id) %>%
      summarise(avg_amount = mean(treatment_amount, na.rm = TRUE))
    
    plot_ly(
      df,
      x = "Average Billing",
      y = ~avg_amount,
      type = 'box',
      fillcolor = "#A569BD",
      line = list(color = "#4A235A"),
      opacity = 0.6,
      boxpoints = FALSE
    ) %>%
      layout(
        yaxis = list(
          title = "Average Amount ($)",
          tickprefix = "$",
          separatethousands = TRUE
        ),
        xaxis = list(title = "")
      )
  })
  
  output$paymentMethod <- renderPlotly({
    df <- filtered_data() %>%
      count(payment_method)
    
    plot_ly(
      df,
      labels = ~payment_method,
      values = ~n,
      type   = 'pie',
      textinfo = 'percent',
      textfont = list(size = 16, color = 'white'),
      marker = list(
        colors = c("#1E8449", "#D63384", "#1F77B4"),
        line = list(color = 'white', width = 2)
      )
    )
  })
  
  output$paymentStatus <- renderPlotly({
    df <- filtered_data() %>%
      count(payment_status)
    
    colors <- c("Failed" = "#C0392B", "Paid" = "#1E8449", "Pending" = "#F39C12")
    
    plot_ly(
      df,
      x     = ~payment_status,
      y     = ~n,
      type  = 'bar',
      text  = ~n,
      textposition = 'inside',
      insidetextanchor = 'middle',
      cliponaxis = FALSE,
      textfont = list(size = 16, color = 'white'),
      marker  = list(color = ~colors[payment_status])
    )
  })
  
  # ===============================
  # Performa Layanan
  # ===============================
  output$topTreatment <- renderPlotly({
    df <- filtered_data() %>%
      count(treatment_type) %>%
      top_n(5, n)
    
    plot_ly(
      df,
      x = ~treatment_type,
      y = ~n,
      type = 'bar',
      text = ~n,
      textposition = 'inside',
      insidetextanchor = 'middle',
      cliponaxis = FALSE,
      textfont = list(size = 16, color = 'white'),
      marker = list(color = "#C0392B")
    )
  })
  
  output$treatPayment <- renderPlotly({
    df <- filtered_data() %>%
      count(treatment_type, payment_method)
    
    plot_ly(
      df,
      x = ~treatment_type,
      y = ~n,
      color = ~payment_method,
      type = 'bar',
      barmode = 'stack',
      text = ~n,
      textposition = 'outside',
      cliponaxis = FALSE,
      textangle = 0,
      textfont = list(size = 10, color = 'black'),
      colors = c("#1E8449", "#D63384", "#1F77B4")
    ) %>%
      layout(uniformtext = list(minsize = 10, mode = 'show'),
             xaxis = list(tickfont = list(size = 10))
      )
  })
  
  output$costSpecialist <- renderPlotly({
    df <- filtered_data() %>%
      group_by(doctor_specialization) %>%
      summarise(mean_cost = mean(treatment_amount, na.rm = TRUE))
    
    plot_ly(
      df,
      x = ~doctor_specialization,
      y = ~mean_cost,
      type = 'bar',
      marker = list(color = "#F39C12"),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Mean Cost: $%{y:,.2f}<extra></extra>"
      )
    ) %>%
      layout(
        yaxis = list(
          title = "Mean Cost",
          tickprefix = "$",
          separatethousands = TRUE
        ),
        xaxis = list(title = "Doctor Specialization")
      )
  })
  
  # ===============================
  # KPI Monitoring
  # ===============================
  output$activePatients <- renderPlotly({
    latest <- data %>%
      group_by(patient_id) %>%
      summarise(last_visit = max(appointment_date, na.rm = TRUE))
    
    last_data_date <- max(data$appointment_date, na.rm = TRUE)
    
    df <- latest %>%
      mutate(status = if_else(
        last_visit >= last_data_date - months(6),
        "Aktif", "Tidak Aktif"
      )) %>%
      count(status) %>%
      arrange(status)
    
    plot_ly(
      df,
      labels   = ~status,
      values   = ~n,
      type     = 'pie',
      textinfo = 'percent',
      textfont = list(size = 16, color = 'white'),
      marker   = list(
        colors = c("#196F3D", "#C0392B"),
        line   = list(color = 'white', width = 2)
      )
    )
  })
  
  output$failedBills <- renderPlotly({
    df <- filtered_data() %>%
      filter(payment_status %in% c("Failed", "Pending")) %>%
      count(payment_status)
    
    colors <- c("Failed" = "#C0392B", "Pending" = "#F39C12")
    
    plot_ly(
      df,
      x               = ~payment_status,
      y               = ~n,
      type            = 'bar',
      text            = ~n,
      textposition    = 'inside',
      insidetextanchor = 'middle',
      cliponaxis      = FALSE,
      textfont        = list(size = 16, color = 'white'),
      marker          = list(color = ~colors[payment_status])
    )
  })
  
  output$leadTime <- renderPlotly({
    df <- filtered_data() %>%
      group_by(patient_id) %>%
      summarise(
        lead = as.numeric(min(appointment_date) - min(registration_date))
      )
    
    plot_ly(
      data  = df,
      x     = "Lead Time",
      y     = ~lead,
      type  = "box",
      fillcolor = "rgba(54, 162, 235, 0.5)",
      line = list(color = "rgba(54, 162, 235, 1)"),
      name = "",
      hovertemplate = "Lead Time: %{y} days<extra></extra>"
    ) %>%
      layout(
        yaxis = list(title = "Lead Time (days)"),
        xaxis = list(title = "")
      )
  })
  
  # ===============================
  # Tab: Pasien
  # ===============================
  observeEvent(input$selected_patient, {
    req(input$selected_patient)
    
    ids <- data %>%
      filter(full_name == input$selected_patient) %>%
      pull(patient_id) %>%
      unique()
    
    updatePickerInput(
      session,
      inputId = "selected_patient_id",
      choices = ids,
      selected = ids[1]
    )
  })
  
  output$pasienInfoTable <- renderDT({
    req(input$selected_patient, input$selected_patient_id)
    
    df <- data %>%
      filter(
        full_name  == input$selected_patient,
        patient_id == input$selected_patient_id
      )
    
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Pesan = "Data tidak ditemukan"),
        options = list(dom = 't', scrollX = TRUE)
      ))
    }
    
    df <- df %>%
      select(
        `ID Pasien`         = patient_id,
        Nama                = full_name,
        Gender              = patient_gender,
        `No HP`             = patient_contact,
        Alamat              = patient_address,
        Email               = patient_email,
        `Provider Asuransi` = insurance_provider,
        `Nomor Asuransi`    = insurance_number
      ) %>%
      distinct()
    
    datatable(df, options = list(dom = 't', scrollX = TRUE))
  })
  
  output$riwayatKunjungan <- renderDT({
    req(input$selected_patient, input$selected_patient_id)
    
    df <- data %>%
      filter(
        full_name == input$selected_patient,
        patient_id == input$selected_patient_id
      ) %>%
      arrange(appointment_date) %>%
      select(
        `Tanggal Janji`       = appointment_date,
        `ID Dokter`           = doctor_id,
        `Spesialisasi Dokter` = doctor_specialization,
        `Alasan Kunjungan`    = visit_reason,
        `Status Janji`        = appointment_status,
        `Perawatan`           = treatment_type
      )
    
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Pesan = "Tidak ada riwayat kunjungan"),
        options = list(dom = 't', scrollX = TRUE)
      ))
    }
    
    datatable(df, options = list(scrollX = TRUE))
  })
  
  # ===============================
  # Tab: Dokter
  # ===============================
  observe({
    df <- data
    
    if (!is.null(input$sidebar_filter_spesialis) &&
        length(input$sidebar_filter_spesialis) > 0) {
      df <- df %>% filter(doctor_specialization %in% input$sidebar_filter_spesialis)
    }
    
    doctors <- unique(paste(df$doctor_first_name, df$doctor_last_name))
    
    updatePickerInput(
      session,
      inputId = "selected_doctor",
      choices = doctors,
      selected = isolate(input$selected_doctor)  
    )
  })
  
  output$dokterInfoCard <- renderUI({
    df <- data %>%
      mutate(doctor_full_name = paste(
        doctor_first_name, doctor_last_name
      ))
    
    if (!is.null(input$sidebar_filter_spesialis)) {
      df <- df %>%
        filter(doctor_specialization %in% input$sidebar_filter_spesialis)
    }
    
    if (!is.null(input$selected_doctor) && length(input$selected_doctor) > 0) {
      df <- df %>% filter(doctor_full_name %in% input$selected_doctor)
    }

    df <- df %>%
      distinct(
        doctor_id,
        doctor_full_name,
        doctor_specialization,
        doctor_phone,
        doctor_email,
        doctor_experience_years,
        doctor_hospital_branch
      ) %>%
      arrange(doctor_id)
    
    if (nrow(df) == 0) {
      return(HTML("<p>Tidak ada data dokter tersedia.</p>"))
    }
    
    dokter_cards <- lapply(seq_len(nrow(df)), function(i) {
      info <- df[i, ]
      
      box(
        width = 6, status = "primary", solidHeader = TRUE,
        title = info$doctor_full_name,
        tags$div(
          style = "display: flex;",
          tags$img(
            src   = "https://cdn-icons-png.flaticon.com/512/3774/3774299.png",
            width = "80px", height = "80px",
            style = "margin-right: 20px; border-radius: 10px;"
          ),
          tags$div(
            tags$p(HTML(paste0("<strong>ID Dokter:</strong> ", info$doctor_id))),
            tags$p(HTML(paste0("<strong>Spesialisasi:</strong> ", info$doctor_specialization))),
            tags$p(HTML(paste0("<strong>No HP:</strong> ", info$doctor_phone))),
            tags$p(HTML(paste0("<strong>Email:</strong> ", info$doctor_email))),
            tags$p(HTML(paste0("<strong>Cabang RS:</strong> ", info$doctor_hospital_branch))),
            tags$p(HTML(paste0("<strong>Tahun Pengalaman:</strong> ", info$doctor_experience_years, " tahun")))
          )
        )
      )
    })
    
    fluidRow(dokter_cards)
  })
  
  output$filteredAppointments <- renderDT({
    df <- filtered_data()
    
    if (!is.null(input$filter_janji_tgl)) {
      df <- df %>%
        filter(day(appointment_date) %in% input$filter_janji_tgl)
    }
    
    if (!is.null(input$filter_janji_bulan)) {
      df <- df %>%
        filter(month.name[month(appointment_date)] %in% input$filter_janji_bulan)
    }
    
    if (!is.null(input$filter_janji_tahun)) {
      df <- df %>%
        filter(year(appointment_date) %in% input$filter_janji_tahun)
    }
    
    if (!is.null(input$dokterFilter)) {
      df <- df %>%
        filter(doctor_specialization %in% input$dokterFilter)
    }
    
    df <- df %>%
      arrange(appointment_date) %>%
      select(
        `ID Pasien`           = patient_id,
        `Nama Pasien`         = full_name,
        `Tanggal Janji`       = appointment_date,
        `ID Dokter`           = doctor_id,
        `Spesialisasi Dokter` = doctor_specialization,
        `Alasan Kunjungan`    = visit_reason,
        `Status Janji`        = appointment_status
      )
    
    datatable(df, options = list(scrollX = TRUE))
  })
  
  # ===============================
  # Tab: Status Tagihan
  # ===============================
  observeEvent(input$namaPasien, {
    df_ids <- filtered_data()
    
    if (!is.null(input$namaPasien) && length(input$namaPasien) > 0) {
      df_ids <- df_ids %>%
        filter(full_name %in% input$namaPasien) %>%
        pull(patient_id) %>%
        unique()
    } else {
      df_ids <- NULL
    }
    
    updatePickerInput(
      session,
      inputId = "patientId",
      choices = df_ids,
      selected = NULL
    )
  })
  
  output$tagihanTable <- renderDT({
    df <- filtered_data()
    
    if (!is.null(input$namaPasien) && length(input$namaPasien) > 0) {
      df <- df %>% filter(full_name %in% input$namaPasien)
    }
    
    if (!is.null(input$patientId) && length(input$patientId) > 0) {
      df <- df %>% filter(patient_id %in% input$patientId)
    }
    
    if (!is.null(input$filter_tagihan_bulan)) {
      df <- df %>%
        filter(month.name[month(bill_date)] %in% input$filter_tagihan_bulan)
    }
    
    if (!is.null(input$filter_tagihan_tahun)) {
      df <- df %>%
        filter(year(bill_date) %in% input$filter_tagihan_tahun)
    }
    
    datatable(
      df %>%
        arrange(bill_date) %>%
        select(
          `ID Pasien`         = patient_id,
          `Nama Pasien`       = full_name,
          `ID Tagihan`        = bill_id,
          `Tanggal Tagihan`   = bill_date,
          `Metode Pembayaran` = payment_method,
          `Jumlah Tagihan`    = treatment_amount,
          `Status Pembayaran` = payment_status
        ),
      options  = list(scrollX = TRUE),
      class    = 'nowrap display',
      rownames = FALSE
    )
  })
  
  output$tagihanStatusChart <- renderPlotly({
    df <- filtered_data()
    
    if (!is.null(input$namaPasien) && length(input$namaPasien) > 0) {
      df <- df %>% filter(full_name %in% input$namaPasien)
    }
    
    if (!is.null(input$patientId) && length(input$patientId) > 0) {
      df <- df %>% filter(patient_id %in% input$patientId)
    }
    
    if (!is.null(input$filter_tagihan_bulan)) {
      df <- df %>%
        filter(month.name[month(bill_date)] %in% input$filter_tagihan_bulan)
    }
    
    if (!is.null(input$filter_tagihan_tahun)) {
      df <- df %>%
        filter(year(bill_date) %in% input$filter_tagihan_tahun)
    }
    
    df <- df %>% count(payment_status)
    
    plot_ly(
      df,
      labels   = ~payment_status,
      values   = ~n,
      type     = 'pie',
      textinfo = 'percent',
      textfont = list(size = 11, color = 'white'),
      marker   = list(line = list(color = '#FFFFFF', width = 1)),
      hole     = 0, 
      domain = list(x = c(0.15, 0.70), y = c(1, 1))
    ) %>% layout(
      showlegend = TRUE,
      legend = list(
        orientation = 'h',
        x = 0.38,
        xanchor = 'center',
        y = -0.1  
      ),
      margin = list(t = 20, b = 80)  
    )
  })
  
  output$paymentMethodChart <- renderPlotly({
    df <- filtered_data()
    
    if (!is.null(input$namaPasien) && length(input$namaPasien) > 0) {
      df <- df %>% filter(full_name %in% input$namaPasien)
    }
    
    if (!is.null(input$patientId) && length(input$patientId) > 0) {
      df <- df %>% filter(patient_id %in% input$patientId)
    }
    
    if (!is.null(input$filter_tagihan_bulan)) {
      df <- df %>%
        filter(month.name[month(bill_date)] %in% input$filter_tagihan_bulan)
    }
    
    if (!is.null(input$filter_tagihan_tahun)) {
      df <- df %>%
        filter(year(bill_date) %in% input$filter_tagihan_tahun)
    }
    
    df <- df %>% count(payment_method)
    
    plot_ly(
      df,
      x = ~payment_method,
      y = ~n,
      type = 'bar',
      text = ~n,
      textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(size = 12, color = 'white'),
      marker = list(color = "#1F77B4")
    ) %>% layout(
      xaxis = list(tickfont = list(size = 10))  
    )
  })
}


# ============================
# Jalankan Aplikasi
# ============================
shinyApp(ui, server)
