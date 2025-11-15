library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(broom)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)

# =======================================================
# FUNGSI GLOBAL: KONVERSI WAKTU
# =======================================================
# (Tidak berubah)
convert_to_seconds <- function(time_strings) {
  sapply(time_strings, function(time_string) {
    if (is.na(time_string)) return(NA)
    time_string <- gsub(",", ".", time_string)
    
    if (grepl(":", time_string)) {
      parts <- as.numeric(strsplit(time_string, ":")[[1]])
      if (length(parts) == 3) {
        total_seconds <- (parts[1] * 60) + parts[2] + (parts[3] / 1000)
      } else if (length(parts) == 2) {
        total_seconds <- (parts[1] * 60) + parts[2]
      } else { total_seconds <- NA }
    } else { total_seconds <- as.numeric(time_string) }
    return(total_seconds)
  })
}

# =======================================================
# FUNGSI GLOBAL BARU: KALKULATOR PACE "TIRE CLIFF"
# =======================================================
# Ini adalah otak baru Anda: menghitung pace per lap dengan uwzględnieniem "cliff"
get_pace_vector <- function(num_laps, deg_rate, base_pace, cliff_start, cliff_multiplier, start_tire_age = 0) {
  if (num_laps <= 0) return(numeric(0))
  
  pace_vector <- numeric(num_laps)
  
  for (i in 1:num_laps) {
    current_tire_age <- start_tire_age + i
    
    if (current_tire_age < cliff_start) {
      # Sebelum cliff: degradasi linear sederhana
      # Pace = Pace Dasar + (Biaya Degradasi * Umur Ban)
      pace_vector[i] <- base_pace + (deg_rate * current_tire_age)
    } else {
      # Setelah cliff:
      # 1. Biaya total untuk sampai ke lap sebelum cliff
      cost_before_cliff <- deg_rate * (cliff_start - 1)
      
      # 2. Berapa lap kita sudah lewati cliff
      laps_after_cliff <- current_tire_age - (cliff_start - 1)
      
      # 3. Biaya tambahan setelah cliff (dengan multiplier)
      cost_after_cliff <- (deg_rate * cliff_multiplier) * laps_after_cliff
      
      # Pace = Pace Dasar + (Biaya sebelum cliff) + (Biaya setelah cliff)
      pace_vector[i] <- base_pace + cost_before_cliff + cost_after_cliff
    }
  }
  return(pace_vector)
}


# =======================================================
# Bagian 1: UI (User Interface)
# =======================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "TGR STRATEGY | PITWALL v5.0", # <-- DIPERBARUI
    tags$li(class = "dropdown",
            tags$style(".navbar-custom-menu { margin-top: 10px; margin-right: 10px; }"),
            prettySwitch(
              inputId = "dark_mode_toggle",
              label = "Dark Mode",
              status = "warning",
              fill = TRUE,
              value = TRUE 
            )
    )
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Pitwall (Live)", tabName = "pitwall", icon = icon("tachometer-alt")),
      menuItem("Pre-Race Setup", tabName = "setup", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    
    useShinyjs(),
    
    # (CSS TGR Skin tidak berubah)
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Exo+2:wght@400;700&display=swap');
        body, h1, h2, h3, h4, .main-sidebar, .main-header, .box-title, .value-box, .info-box { 
          font-family: 'Exo 2', sans-serif !important; 
        }
        .box-title { 
          font-weight: 700 !important; 
          text-transform: uppercase; 
        }
        .content-wrapper {
          background-color: #1a1a1a !important; 
          background-image: url('https://www.transparenttextures.com/patterns/carbon-fibre.png') !important;
          background-repeat: repeat !important;
          color: #FFF !important;
        }
        .main-header .navbar, .main-header .logo, .main-sidebar { 
          background-color: #000000 !important; 
        }
        .main-header .logo { font-weight: 700; }
        .box { 
          background-color: transparent !important; 
          border-top: 3px solid #E60000 !important;
        }
        .box-header { 
          border-bottom: 1px solid #444 !important; 
        }
        .info-box, .value-box { 
          background: #2a2a2a !important; 
          color: #FFF !important; 
        }
        .info-box-icon { color: #FFF !important; }
        .info-box[class*='bg-'] { background-color: #2a2a2a !important; }
        .info-box.bg-red { border-top: 3px solid #E60000 !important; }
        .info-box.bg-black { border-top: 3px solid #555 !important; }
        .value-box.bg-red { background-color: #E60000 !important; }
        .value-box.bg-green, .value-box.bg-lime { background-color: #00b050 !important; }
        .value-box.bg-blue { background-color: #0070c0 !important; }
        .value-box.bg-orange { background-color: #ffc000 !important; color: #000 !important; }
        .value-box.bg-black { background-color: #000 !important; }
        
        /* Light Mode */
        .light-mode .content-wrapper { background: #f4f4f4 !important; color: #333 !important; }
        .light-mode .main-header .navbar, .light-mode .main-header .logo { background-color: #D32F2F !important; }
        .light-mode .main-sidebar { background-color: #222d32 !important; }
        .light-mode .box { background-color: #FFF !important; color: #333 !important; border-top-color: #D32F2F !important; }
        .light-mode .box-header { border-bottom: 1px solid #f4f4f4 !important; }
        .light-mode .info-box { background: #f9f9f9 !important; color: #333 !important; }
        .light-mode .info-box[class*='bg-'] { background-color: #f9f9f9 !important; }
        .light-mode .info-box.bg-red { border-left: 3px solid #E60000 !important; }
        .light-mode .info-box.bg-black { border-left: 3px solid #555 !important; }
        
        /* Sidebar Text Fix */
        .sidebar, .sidebar .box, .sidebar .shiny-input-container, .sidebar .checkbox { color: #FFF !important; }
        .light-mode .sidebar, .light-mode .sidebar .box, .light-mode .sidebar .shiny-input-container, .light-mode .sidebar .checkbox { color: #FFF !important; }
        .light-mode .sidebar .box { background-color: #222d32 !important; }
      "))
    ),
    
    tabItems(
      
      # =======================================================
      # TAB 1: PITWALL (LIVE)
      # =======================================================
      # (Tidak berubah)
      tabItem(tabName = "pitwall",
              fluidRow(
                h4("Live Status", style = "padding-left: 15px; text-transform: uppercase;"),
                infoBoxOutput("currentLapBox", width = 6),
                infoBoxOutput("currentTireAgeBox", width = 6)
              ),
              fluidRow(
                valueBoxOutput("decisionBox", width = 12)
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Pitwall Control", status = "success", solidHeader = TRUE,
                         width = 12,
                         actionButton("advance_lap", "ADVANCE LAP >>", icon = icon("arrow-right"), width = "100%", class = "btn-success", style="font-weight: bold; font-size: 16px;"),
                         actionButton("pit_stop_taken", "PIT STOP COMPLETE", icon = icon("wrench"), width = "100%", class = "btn-danger", style = "margin-top: 10px;"),
                         hr(), 
                         h5("Live Traffic (Optional)", style="text-align: center;"),
                         numericInput("gap_to_car_ahead", "Gap to Car Ahead (sec):", 5.0, min=0, step=0.1),
                         numericInput("gap_to_car_behind", "Gap to Car Behind (sec):", 8.0, min=0, step=0.1),
                         checkboxInput("caution_flag", "SAFETY CAR / FCY ACTIVE?", value = FALSE)
                       ),
                       infoBoxOutput("stayOutBox", width = 12),
                       infoBoxOutput("pitNowBox", width = 12)
                ),
                column(width = 6,
                       box(
                         title = "Live Crossover Plot",
                         width = 12, solidHeader = TRUE, status = "danger",
                         plotlyOutput("crossoverPlot")
                       )
                )
              )
      ), 
      
      # =======================================================
      # TAB 2: PRE-RACE SETUP
      # =======================================================
      tabItem(tabName = "setup",
              fluidRow(
                # --- Kolom Kiri: Input & Setup ---
                column(width = 6,
                       box(
                         title = "Race Data Input", status = "danger", solidHeader = TRUE,
                         width = 12, collapsible = TRUE,
                         div(id = "file_inputs_div", 
                             fileInput("file_endurance", "1. Upload Endurance File", accept = c(".csv")),
                             fileInput("file_weather", "2. Upload Weather File", accept = c(".csv")),
                             fileInput("file_baseline", "3. Upload Best 10 Laps File", accept = c(".csv"))
                         ), 
                         actionButton("run_analysis", "Analyze Race Data", icon = icon("cogs"), width = "100%", class = "btn-primary"),
                         actionButton("clear_files", "Clear/Reset Files", icon = icon("trash-alt"), width = "100%", class = "btn-danger", style = "margin-top: 5px;") 
                       ),
                       
                       # --- DIPERBARUI: Input "Tire Cliff" ditambahkan ---
                       box(
                         title = "Model Inputs (Template)", status = "info", solidHeader = TRUE,
                         width = 12, collapsible = TRUE,
                         numericInput("degradation", "Degradation Cost/Lap (sec):", value = 0.0, step = 0.01),
                         numericInput("pit_delta", "Pit Stop Delta (sec):", value = 50.0, step = 1),
                         numericInput("baseline_pace", "Baseline Pace (sec):", value = 90.0, step = 0.1),
                         hr(),
                         h5("Tire Cliff Assumption (Advanced)", style="text-align: center;"),
                         numericInput("cliff_start_lap", "Tire Cliff starts at Lap:", 18, min = 1),
                         numericInput("cliff_multiplier", "Degradation Multiplier:", 2.0, min = 1, step = 0.1)
                       ),
                       
                       box(
                         title = "Race Setup", status = "warning", solidHeader = TRUE,
                         width = 12, collapsible = TRUE,
                         numericInput("total_race_laps", "Total Race Laps:", 30, min = 1),
                         actionButton("reset_sim", "Load/Reset Simulation", icon = icon("refresh"), width = "100%")
                       )
                ),
                
                # --- Kolom Kanan: Hasil Analisis ---
                # (Tidak berubah)
                column(width = 6,
                       box(
                         title = "Race Analysis Summary",
                         status = "danger", solidHeader = TRUE, width = 12,
                         collapsible = TRUE,
                         
                         infoBoxOutput("avgTempBox", width = 6),
                         infoBoxOutput("avgPaceBox", width = 6),
                         infoBoxOutput("totalLapsBox", width = 6),
                         infoBoxOutput("pitDeltaBox", width = 6)
                       ),
                       
                       tabBox(
                         title = "Analysis Plots",
                         id = "analysis_plot_tabs", 
                         width = 12, 
                         
                         tabPanel("Pre-Race Planner", 
                                  infoBoxOutput("optimalStrategyBox", width = 12),
                                  box(
                                    title = "Total Race Time Comparison", 
                                    width = 12, 
                                    solidHeader = FALSE,
                                    status = "danger",
                                    plotlyOutput("preRacePlot")
                                  )
                         ),
                         tabPanel("Degradation Model", 
                                  plotlyOutput("degradationPlot")
                         ),
                         tabPanel("Pit Stop Analysis", 
                                  plotlyOutput("pitSectorPlot")
                         )
                       )
                )
              )
      ) # Akhir tabItem "setup"
    ) # Akhir tabItems
  ) # Akhir dashboardBody
) # Akhir dashboardPage


# =======================================================
# Bagian 2: Server (Logika)
# =======================================================
server <- function(input, output, session) {
  
  # --- DIPERBARUI: Fungsi helper sekarang berada di luar server ---
  # (Sudah dipindahkan ke atas)
  
  plot_text_color <- reactiveVal("white") 
  
  analysis_results <- reactiveValues(
    avg_temp = NULL, avg_pace = NULL, total_laps = NULL,
    model_data = NULL, model_fit = NULL, deg_estimate = 0.0,
    stationary_pit = NULL, pit_delta = NULL, pit_plot_data = NULL
  )
  
  race_state <- reactiveValues(
    current_lap = 1,
    laps_on_tire = 1
  )
  
  shinyjs::addClass(selector = "body", class = "dark-mode")
  observeEvent(input$dark_mode_toggle, {
    if (input$dark_mode_toggle == TRUE) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
      shinyjs::removeClass(selector = "body", class = "light-mode")
      plot_text_color("white")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
      shinyjs::addClass(selector = "body", class = "light-mode")
      plot_text_color("black")
    }
  })
  
  # (Logika Tombol tidak berubah)
  observeEvent(input$reset_sim, {
    race_state$current_lap <- 1
    race_state$laps_on_tire <- 1
    showNotification("Simulation Reset to Lap 1.", type = "message")
  })
  observeEvent(input$advance_lap, {
    race_state$current_lap <- race_state$current_lap + 1
    race_state$laps_on_tire <- race_state$laps_on_tire + 1
  })
  observeEvent(input$pit_stop_taken, {
    race_state$laps_on_tire <- 1 
    showNotification("Pit Stop Logged! Tires reset to 1 lap.", type = "warning", duration = 5)
  })
  
  # (Logika Analisis tidak berubah)
  observeEvent(input$run_analysis, {
    req(input$file_endurance, input$file_weather, input$file_baseline)
    showNotification("Starting race data analysis...", type = "message", duration = 5)
    
    tryCatch({
      df_laps <- read_delim(input$file_endurance$datapath, delim = ";")
      df_weather <- read_delim(input$file_weather$datapath, delim = ";")
      df_baseline <- read_delim(input$file_baseline$datapath, delim = ";")
      
      names(df_laps) <- trimws(names(df_laps)); names(df_weather) <- trimws(names(df_weather)); names(df_baseline) <- trimws(names(df_baseline))
      
      analysis_results$avg_temp <- mean(df_weather$AIR_TEMP, na.rm = TRUE)
      analysis_results$total_laps <- max(df_laps$LAP_NUMBER, na.rm = TRUE)
      
      if ("AVERAGE" %in% names(df_baseline)) {
        avg_pace_value <- df_baseline %>%
          mutate(AVERAGE_SEC = convert_to_seconds(AVERAGE)) %>%
          pull(AVERAGE_SEC) %>%
          mean(na.rm = TRUE)
        analysis_results$avg_pace <- avg_pace_value
        updateNumericInput(session, "baseline_pace", value = round(analysis_results$avg_pace, 2))
      } else {
        analysis_results$avg_pace <- 90.0
      }
      updateNumericInput(session, "total_race_laps", value = analysis_results$total_laps)
      
      df_stints <- df_laps %>%
        arrange(NUMBER, LAP_NUMBER) %>%
        group_by(NUMBER) %>%
        mutate(
          is_pit_lap = ifelse(is.na(PIT_TIME), 0, ifelse(PIT_TIME > 0, 1, 0)),
          new_stint_flag = lag(is_pit_lap, default = 0),
          stint_id = cumsum(new_stint_flag) + 1
        ) %>%
        ungroup() %>%
        group_by(NUMBER, stint_id) %>% 
        mutate(laps_on_tire = row_number()) %>%
        ungroup()
      
      flag_to_use <- "GF" 
      if (!("GF" %in% trimws(df_stints$FLAG_AT_FL))) { flag_to_use <- "GREEN" }
      
      df_model_filtered <- df_stints %>%
        filter(trimws(FLAG_AT_FL) == flag_to_use, is_pit_lap == 0, laps_on_tire > 1)
      
      df_model_converted <- df_model_filtered %>%
        mutate(
          LAP_TIME = convert_to_seconds(LAP_TIME), S1 = convert_to_seconds(S1),
          S2 = convert_to_seconds(S2), S3 = convert_to_seconds(S3),
          laps_on_tire = as.numeric(laps_on_tire)
        ) %>%
        filter(!is.na(LAP_TIME) & !is.na(S1) & !is.na(S2) & !is.na(S3) & !is.na(laps_on_tire))
      
      analysis_results$model_data <- df_model_converted
      
      calculated_deg <- 0.0
      if (nrow(df_model_converted) > 10) {
        model_degradation <- lm(LAP_TIME ~ laps_on_tire, data = df_model_converted) # Disederhanakan
        analysis_results$model_fit <- fitted(model_degradation)
        model_summary <- broom::tidy(model_degradation)
        deg_stats <- model_summary %>% filter(term == 'laps_on_tire')
        if (deg_stats$p.value < 0.05) { calculated_deg <- deg_stats$estimate }
      }
      analysis_results$deg_estimate <- calculated_deg
      
      pit_laps_raw <- df_stints %>%
        filter(is_pit_lap == 1) %>%
        mutate(pit_time_seconds = convert_to_seconds(PIT_TIME)) %>%
        filter(!is.na(pit_time_seconds))
      avg_stationary_time <- mean(pit_laps_raw$pit_time_seconds, na.rm = TRUE)
      analysis_results$stationary_pit <- avg_stationary_time
      
      avg_S1_normal <- mean(df_model_converted$S1, na.rm = TRUE)
      avg_S3_normal <- mean(df_model_converted$S3, na.rm = TRUE)
      
      in_laps_data <- df_stints %>%
        filter(is_pit_lap == 1, trimws(FLAG_AT_FL) == flag_to_use) %>%
        mutate(S3_sec = convert_to_seconds(S3)) %>% filter(!is.na(S3_sec))
      avg_S3_inlap <- mean(in_laps_data$S3_sec, na.rm = TRUE)
      time_lost_entering <- avg_S3_inlap - avg_S3_normal
      
      out_laps_data <- df_stints %>%
        filter(laps_on_tire == 1, stint_id > 1, trimws(FLAG_AT_FL) == flag_to_use) %>%
        mutate(S1_sec = convert_to_seconds(S1)) %>% filter(!is.na(S1_sec))
      avg_S1_outlap <- mean(out_laps_data$S1_sec, na.rm = TRUE)
      time_lost_exiting <- avg_S1_outlap - avg_S1_normal
      
      calculated_delta <- time_lost_entering + time_lost_exiting + avg_stationary_time
      
      analysis_results$pit_delta <- calculated_delta
      analysis_results$pit_plot_data <- data.frame(
        Category = c("Time Lost Entering (S3)", "Stationary Time", "Time Lost Exiting (S1)"),
        Time_Lost = c(time_lost_entering, avg_stationary_time, time_lost_exiting)
      )
      
      updateNumericInput(session, "degradation", value = round(calculated_deg, 3))
      updateNumericInput(session, "pit_delta", value = round(calculated_delta, 1)) 
      
      showNotification(paste("Analysis Complete! Degradation:", 
                             round(calculated_deg, 3), "s/lap.",
                             "Calculated Pit Delta:", round(calculated_delta, 1), "sec."),
                       type = "message", duration = 10)
      
    }, error = function(e) {
      showNotification(paste("Analysis Failed:", e$message), type = "error", duration = 10)
    })
  })
  observeEvent(input$clear_files, {
    shinyjs::reset("file_inputs_div")
    analysis_results$avg_temp <- NULL; analysis_results$avg_pace <- NULL
    analysis_results$total_laps <- NULL; analysis_results$model_data <- NULL
    analysis_results$model_fit <- NULL; analysis_results$pit_delta <- NULL
    analysis_results$pit_plot_data <- NULL
    updateNumericInput(session, "degradation", value = 0.0)
    updateNumericInput(session, "pit_delta", value = 50.0)
    showNotification("File inputs and analysis have been cleared.", type = "message", duration = 3)
  })
  
  # --- LOGIKA SIMULASI LIVE (DIPERBARUI DENGAN "TIRE CLIFF") ---
  strategyLogic <- reactive({
    req(input$degradation, input$pit_delta, input$baseline_pace, input$gap_to_car_behind,
        input$cliff_start_lap, input$cliff_multiplier) # Input baru
    req(race_state$current_lap, race_state$laps_on_tire)
    
    DEGRADATION_PER_LAP <- input$degradation
    PIT_STOP_DELTA_NORMAL <- input$pit_delta
    PIT_STOP_DELTA_CAUTION <- PIT_STOP_DELTA_NORMAL / 2 
    CLIFF_START <- input$cliff_start_lap
    CLIFF_MULT <- input$cliff_multiplier
    BASE_PACE <- input$baseline_pace
    
    if (input$caution_flag == TRUE) { current_pit_delta <- PIT_STOP_DELTA_CAUTION
    } else { current_pit_delta <- PIT_STOP_DELTA_NORMAL }
    
    laps_remaining_sim <- input$total_race_laps - race_state$current_lap
    
    if (laps_remaining_sim < 1) {
      return(list(decision = "RACE COMPLETE", color = "black", icon = "flag-checkered", 
                  subtitle = "Simulation finished."))
    }
    
    # --- MENGGUNAKAN FUNGSI BARU "get_pace_vector" ---
    pace_stay_out <- get_pace_vector(laps_remaining_sim, DEGRADATION_PER_LAP, BASE_PACE, 
                                     CLIFF_START, CLIFF_MULT, 
                                     start_tire_age = race_state$laps_on_tire)
    
    pace_pit_now <- get_pace_vector(laps_remaining_sim, DEGRADATION_PER_LAP, BASE_PACE, 
                                    CLIFF_START, CLIFF_MULT, 
                                    start_tire_age = 0) # Ban baru = 0
    
    df_compare <- data.frame(
      Lap = (1:laps_remaining_sim) + race_state$current_lap,
      Time_Stay_Out = cumsum(pace_stay_out),
      Time_Pit_Now = current_pit_delta + cumsum(pace_pit_now)
    )
    
    TOTAL_TIME_STAY_OUT <- tail(df_compare$Time_Stay_Out, 1)
    TOTAL_TIME_PIT_NOW <- tail(df_compare$Time_Pit_Now, 1)
    
    is_pit_faster_on_time <- TOTAL_TIME_PIT_NOW < TOTAL_TIME_STAY_OUT
    pit_cost_time <- current_pit_delta
    gap_behind <- input$gap_to_car_behind
    exit_position_relative <- gap_behind - pit_cost_time
    is_clear_window <- exit_position_relative > 0
    
    # (Pohon keputusan tidak berubah, tapi sekarang didasarkan pada perhitungan "cliff")
    decision <- "STAY OUT"; color <- "green"; icon_name <- "road"; 
    subtitle <- "Pace is optimal. Watching traffic."
    
    if (is_pit_faster_on_time) {
      if (is_clear_window) {
        decision <- "PIT NOW (CLEAR WINDOW)"; color <- "lime"; icon_name <- "rocket"
        subtitle <- paste("Tire Undercut is ON! You will exit", round(exit_position_relative, 1), "sec IN FRONT of traffic.")
      } else {
        decision <- "PIT NOW (EXIT IN TRAFFIC)"; color <- "orange"; icon_name <- "exclamation-triangle"
        subtitle <- paste("Pit is faster, BUT you will exit", round(abs(exit_position_relative), 1), "sec BEHIND traffic.")
      }
    } else {
      if (is_clear_window) {
        decision <- "STAY OUT (WINDOW IS CLEAR)"; color <- "green"; icon_name <- "road"
        subtitle <- paste("Pace is good. Safe to pit for damage (you have a", round(exit_position_relative, 1), "sec window).")
      } else {
        decision <- "STAY OUT (NO WINDOW)"; color <- "blue"; icon_name <- "lock"
        subtitle <- "Pace is good. DO NOT PIT. You will exit in traffic."
      }
    }
    
    if (input$caution_flag == TRUE) {
      icon_name <- "car-crash"
      if (is_clear_window) {
        decision <- "PIT NOW (SAFETY CAR)"
        color <- "red"
        subtitle <- paste("FCY ACTIVE! Take this 'discounted' pit stop. You will exit", round(exit_position_relative, 1), "sec in front.")
      } else {
        decision <- "STAY OUT (NO WINDOW)"
        color <- "blue"
        subtitle <- "FCY Active, but no window. STAY OUT unless damaged."
      }
    }
    
    list(
      df_compare = df_compare, decision = decision, color = color,
      icon = icon_name, subtitle = subtitle,
      time_stay_out = TOTAL_TIME_STAY_OUT,
      time_pit_now = TOTAL_TIME_PIT_NOW, delta = current_pit_delta
    )
  })
  
  # --- LOGIKA PRE-RACE (DIPERBARUI DENGAN "TIRE CLIFF") ---
  preRaceLogic <- reactive({
    req(input$degradation, input$pit_delta, input$baseline_pace, input$total_race_laps,
        input$cliff_start_lap, input$cliff_multiplier) # Input baru
    
    deg_rate <- input$degradation; pit_delta <- input$pit_delta; base_pace <- input$baseline_pace
    total_laps <- input$total_race_laps
    CLIFF_START <- input$cliff_start_lap
    CLIFF_MULT <- input$cliff_multiplier
    
    # Fungsi helper memanggil fungsi global
    calc_stint <- function(num_laps, start_age = 0) {
      sum(get_pace_vector(num_laps, deg_rate, base_pace, CLIFF_START, CLIFF_MULT, start_tire_age = start_age))
    }
    
    # 0-Stop: 1 stint penuh
    time_0_stop <- calc_stint(total_laps, start_age = 0)
    
    # 1-Stop: 2 stint (ban baru)
    stint_laps_1 <- floor(total_laps / 2)
    stint_laps_2 <- total_laps - stint_laps_1
    time_1_stop <- calc_stint(stint_laps_1, start_age = 0) + 
      pit_delta + 
      calc_stint(stint_laps_2, start_age = 0)
    
    df_plot <- data.frame(
      Strategy = c("0-Stop", "1-Stop"),
      Total_Time = c(time_0_stop, time_1_stop)
    )
    
    df_plot <- df_plot %>% filter(!is.na(Total_Time))
    
    if(nrow(df_plot) > 0) {
      optimal <- df_plot[which.min(df_plot$Total_Time), ]
      optimal_decision <- paste(optimal$Strategy, "is Optimal")
      optimal_time <- optimal$Total_Time
    } else {
      optimal_decision <- "Calculation Error"; optimal_time <- NA
    }
    
    list(
      df_plot = df_plot,
      optimal_decision = optimal_decision,
      optimal_time = optimal_time
    )
  })
  
  # =======================================================
  # BAGIAN D: RENDER OUTPUT
  # =======================================================
  
  output$crossoverPlot <- renderPlotly({
    logic <- strategyLogic(); req(logic); req(logic$decision != "RACE COMPLETE") 
    current_plot_text_color <- plot_text_color() 
    p_main <- plot_ly(logic$df_compare, x = ~Lap) %>%
      add_trace(y = ~Time_Stay_Out, name = 'Stay Out', type = 'scatter', mode = 'lines', line = list(color = '#00b050')) %>%
      add_trace(y = ~Time_Pit_Now, name = 'Pit Now', type = 'scatter', mode = 'lines', line = list(color = '#E60000')) %>%
      layout(title = "Cumulative Race Time (Live)", 
             xaxis = list(title = "Race Lap Number"), yaxis = list(title = "Total Seconds Elapsed from Now"), 
             hovermode = "x unified", font = list(color = current_plot_text_color),
             plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
    p_main
  })
  
  output$preRacePlot <- renderPlotly({
    logic <- preRaceLogic(); req(logic); current_plot_text_color <- plot_text_color()
    
    p <- plot_ly(logic$df_plot, x = ~Strategy, y = ~Total_Time, 
                 type = 'bar', text = ~paste(round(Total_Time / 60, 2), "min"),
                 textposition = 'auto', marker = list(color = '#E60000')) %>%
      layout(
        title = "Pre-Race Strategy Comparison (Full Race)",
        xaxis = list(title = "Strategy (Tire Degradation Only)"),
        yaxis = list(title = "Total Race Time (seconds)"),
        font = list(color = current_plot_text_color),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    p
  })
  
  output$degradationPlot <- renderPlotly({
    req(analysis_results$model_data)
    df_plot <- analysis_results$model_data; current_plot_text_color <- plot_text_color()
    p_deg <- ggplot(df_plot, aes(x = laps_on_tire, y = LAP_TIME)) +
      geom_point(alpha = 0.5, color = "grey") + 
      geom_smooth(method = "lm", se = FALSE, color = "#E60000", formula = y ~ x) + 
      labs(title = "Tire Degradation Model", x = "Laps on Tire", y = "Lap Time (seconds)") +
      theme_minimal() + 
      theme(
        text = element_text(color = current_plot_text_color),
        plot.title = element_text(color = current_plot_text_color),
        axis.title = element_text(color = current_plot_text_color),
        axis.text = element_text(color = current_plot_text_color),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
      )
    ggplotly(p_deg) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$pitSectorPlot <- renderPlotly({
    req(analysis_results$pit_plot_data)
    df_plot <- analysis_results$pit_plot_data
    current_plot_text_color <- plot_text_color()
    
    p <- plot_ly(df_plot, x = ~Category, y = ~Time_Lost, 
                 type = 'bar',
                 text = ~paste(round(Time_Lost, 1), "sec"),
                 textposition = 'auto',
                 marker = list(color = '#E60000')) %>%
      layout(
        title = "Pit Stop Delta Breakdown",
        xaxis = list(title = "Component"),
        yaxis = list(title = "Time Lost (seconds)"),
        font = list(color = current_plot_text_color),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    p
  })
  
  output$decisionBox <- renderValueBox({ 
    logic <- strategyLogic(); req(logic)
    valueBox(logic$decision, logic$subtitle, icon = icon(logic$icon), color = logic$color) 
  })
  
  output$optimalStrategyBox <- renderInfoBox({
    logic <- preRaceLogic(); req(logic)
    infoBox("Optimal Pre-Race Strategy (Tires Only)", logic$optimal_decision,
            subtitle = paste("Total Time:", round(logic$optimal_time, 1), "sec"),
            icon = icon("sitemap"), color = "red", width = 12
    )
  })
  
  output$stayOutBox <- renderInfoBox({ 
    logic <- strategyLogic(); req(logic); req(logic$time_stay_out)
    infoBox("Total Time: STAY OUT", paste(round(logic$time_stay_out, 1), "sec"), icon = icon("clock"), color = "green", subtitle = NULL) 
  })
  
  output$pitNowBox <- renderInfoBox({ 
    logic <- strategyLogic(); req(logic); req(logic$time_pit_now)
    infoBox("Total Time: PIT NOW", paste(round(logic$time_pit_now, 1), "sec"), icon = icon("wrench"), color = "red", subtitle = NULL) 
  })
  
  output$avgTempBox <- renderInfoBox({ req(analysis_results$avg_temp); infoBox("Avg. Air Temp", paste(round(analysis_results$avg_temp, 1), "°C"), icon = icon("thermometer-half"), color = "black") })
  output$avgPaceBox <- renderInfoBox({ req(analysis_results$avg_pace); infoBox("Avg. Baseline Pace", paste(round(analysis_results$avg_pace, 3), "sec"), icon = icon("tachometer-alt"), color = "black") })
  output$totalLapsBox <- renderInfoBox({ req(analysis_results$total_laps); infoBox("Total Race Laps", paste(analysis_results$total_laps), icon = icon("flag-checkered"), color = "black") })
  
  output$pitDeltaBox <- renderInfoBox({
    req(analysis_results$pit_delta)
    infoBox(
      "Calculated Pit Delta",
      paste(round(analysis_results$pit_delta, 1), "sec"),
      icon = icon("stopwatch"),
      color = "black"
    )
  })
  
  output$currentLapBox <- renderInfoBox({ infoBox("Current Lap", paste(race_state$current_lap, "/", input$total_race_laps), icon = icon("road"), color = "red") })
  output$currentTireAgeBox <- renderInfoBox({ infoBox("Tire Age", paste(race_state$laps_on_tire, "Laps"), icon = icon("dot-circle"), color = "red") })
  
}


# =======================================================
# Bagian 3: Jalankan Aplikasi
# =======================================================
shinyApp(ui = ui, server = server)