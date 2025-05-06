# Load necessary libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(baseballr)
library(kableExtra)

chadwick <- chadwick_player_lu()

# Define UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  # Background and text styles
  tags$style(HTML("
    body {
      background-image: url('https://github.com/IDBach16/BachAnalytics-Pitcher_Comparison_Dashboard/blob/main/1503447065.jpg?raw=true');
      background-attachment: fixed;
      background-size: cover;
    }
    .tab-content .active h3, .tab-content .active p {
        color: #E0E0E0;
    }
    .tab-content .active {
        background-color: rgba(0, 0, 0, 0.7);
        padding: 15px;
        border-radius: 5px;
    }
  ")),
  
  # Application title
  titlePanel("MLB Pitcher Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("first_name", "First Name (Case Sensitive)", value = "Kenny"),
      textInput("last_name", "Last Name (Case Sensitive)", value = "Powers"),
      actionButton("submit", "Get Player Data (Please Wait 10-15
                   Seconds for Data Import)"),
      br(),
      selectInput("zone_pitch_type", "Select Pitch Type for Zone Plot", choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pitcher Data", 
                 h3("Pitch Plot"),
                 plotlyOutput("pitchPlot"),
                 h3("Summary Tables (Season)"),
                 tableOutput("summaryTableSeason1"),
                 tableOutput("summaryTableSeason2"),
                 tableOutput("summaryTableSeason3"),
                 h3("Pitch Usage Table"),
                 tableOutput("pitchUsageTable"), 
                 h3("Usage vs RHH Table"),
                 tableOutput("usageRTable"), 
                 h3("Stats vs RHH Table"),
                 tableOutput("statsRTable"),
                 h3("Usage vs LHH Table"),
                 tableOutput("usageLTable"),
                 h3("Stats vs LHH Table"),
                 tableOutput("statsLTable"),
                 h3("Velocity Plot"),
                 plotlyOutput("veloPlot"),
                 h3("Zone Plot"),
                 plotlyOutput("zonePlot"),
                 h3("Release Height Plot"),
                 plotlyOutput("releaseHeightPlot")
        ),
        tabPanel("R vs L Pitch Plot",
                 h3("R vs L Pitch Plot"),
                 uiOutput("rVsLPitchPlots")
        ),
        tabPanel("Date Filter",
                 h3("Filter Data by Date"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("date_select", "Select Date", choices = NULL)
                   ),
                   mainPanel(
                     h3("Pitch Plot"),
                     plotlyOutput("filteredPitchPlot"),
                     h3("Summary Tables (Single Game)"),
                     tableOutput("summaryTableGame1"),
                     tableOutput("summaryTableGame2"),
                     tableOutput("summaryTableGame3"),
                     h3("Pitch Usage Table"),
                     tableOutput("filteredPitchUsageTable"), 
                     h3("Usage vs RHH Table"),
                     tableOutput("filteredUsageRTable"), 
                     h3("Stats vs RHH Table"),
                     tableOutput("filteredStatsRTable"),
                     h3("Usage vs LHH Table"),
                     tableOutput("filteredUsageLTable"),
                     h3("Stats vs LHH Table"),
                     tableOutput("filteredStatsLTable"),
                     h3("Velocity Plot"),
                     plotlyOutput("filteredVeloPlot"),
                     h3("Zone Plot"),
                     plotlyOutput("filteredZonePlot"),
                     h3("Release Height Plot"),
                     plotlyOutput("filteredReleaseHeightPlot")
                   )
                 )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Fetch and process pitcher data when the submit button is clicked
  pitcher_data <- eventReactive(input$submit, {
    req(input$first_name, input$last_name)
    #Pitcher_id <- baseballr::playerid_lookup(last_name = input$last_name, first_name = input$first_name)
    Pitcher_ID <- chadwick %>% dplyr::filter(mlb_played_last == 2024, name_last == input$last_name, name_first == input$first_name) %>% dplyr::pull(key_mlbam)
    #Pitcher_ID <- Pitcher_id %>% pull(mlbam_id)
    baseballr::statcast_search_pitchers(start_date = "2024-03-28", end_date = "2024-12-01", pitcherid = Pitcher_ID)
  })
  
  # Update the date dropdown based on the fetched data
  observeEvent(pitcher_data(), {
    dates <- pitcher_data() %>% pull(game_date) %>% unique() %>% as.character()
    updateSelectInput(session, "date_select", choices = dates)
  })
  
  # Filter data based on the selected date
  filtered_data <- reactive({
    req(input$date_select)
    pitcher_data() %>% filter(game_date == input$date_select)
  })
  
  # Output the filtered data table
  output$filteredDataTable <- renderTable({
    filtered_data()
  })
  
  # Render filtered pitch plot
  output$filteredPitchPlot <- renderPlotly({
    Pitcher <- filtered_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    pitch_colors <- c("4-Seam Fastball" = "red", "Curveball" = "blue", "Cutter" = "cyan", "Pitch Out" = "violet", "Sinker" = "black", "Slider" = "green", "Slurve" = "pink", "Split-Finger" = "orange", "PO" = "gray50", "SV" = "beige", "Change-Up" = "gold")
    pitch_types <- unique(cleaned_data$pitch_name)
    
    gg <- cleaned_data %>% 
      ggplot(aes(x = pfx_x_in_pv, y = pfx_z_in, color = pitch_name)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(aes(text = paste0("Velo: ",release_speed,"<br>",
                                   "Spin Axis: ",spin_axis,"</br>",
                                   "Event:  ",description,"</br>",
                                   "Spin Rate:  ",release_spin_rate,"</br>",
                                   "Release Ext:  ",release_extension,"</br>"
      )),size = 1.5, alpha = 0.25) +
      scale_color_manual(values = pitch_colors, limits = pitch_types) +
      scale_x_continuous(limits = c(-25, 25), breaks = seq(-20, 20, 5), labels = scales::number_format(suffix = "\"")) +
      scale_y_continuous(limits = c(-25, 25), breaks = seq(-20, 20, 5), labels = scales::number_format(suffix = "\"")) +
      coord_equal() +
      labs(title = "Pitch Movement", subtitle = "2024 MLB Season | Pitcher's POV", caption = "Data: Baseball Savant via baseballr", x = "Horizontal Break", y = "Induced Vertical Break", color = "Pitch Type")
    
    ggplotly(gg)
  })
  
  # Render filtered pitch usage table
  output$filteredPitchUsageTable <- renderTable({
    Pitcher <- filtered_data()
    Pitcher %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `#` = n(),
        `Use%` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strk%` = round(sum(description %in% c("swinging_strike", "foul", "hit_into_play", "called_strike"))/n(), 3) * 100,
        `Whiff%` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Use%` = round(`Use%` / sum(`Use%`), 3) * 100)
  })
  
  # Render filtered usage vs RHH table
  output$filteredUsageRTable <- renderTable({
    Pitcher <- filtered_data()
    Pitcher %>%
      filter(stand == 'R') %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `No.` = n(),
        `Usage %` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strike %` = round(sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play", "called_strike")) / n(), 3) * 100,
        `Whiff %` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Usage %` = round(`Usage %` / sum(`Usage %`), 3) * 100, `1P%` = round(`1P` / sum(`1P`), 3) * 100, `2K%` = round(`2K` / sum(`2K`), 3) * 100) %>%
      select(-`1P`, -`2K`)
  })
  
  # Render filtered stats vs RHH table
  output$filteredStatsRTable <- renderTable({
    Pitcher <- filtered_data()
    Pitcher %>%
      filter(stand == 'R') %>%
      summarize(
        BF = n_distinct(inning, batter),
        K = sum(events == "strikeout"),
        BB = sum(events == "walk"),
        HBP = sum(events == 'hit_by_pitch'),
        H = sum(events %in% c('single', 'double', 'triple', 'home_run')),
        `1B` = sum(events == 'single'),
        `2B` = sum(events == 'double'),
        `3B` = sum(events == 'triple'),
        HR = sum(events == 'home_run'),
        AVG = round(H / (BF - BB - HBP), 3),
        SLG = round(sum((`1B` * 1) + (`2B` * 2) + (`3B` * 3) + (HR * 4)) / (BF - BB - HBP), 3)
      ) %>%
      select(BF, K, BB, HBP, H, HR, AVG, SLG)
  })
  
  # Render filtered usage vs LHH table
  output$filteredUsageLTable <- renderTable({
    Pitcher <- filtered_data()
    Pitcher %>%
      filter(stand == 'L') %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `No.` = n(),
        `Usage %` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strike %` = round(sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play", "called_strike")) / n(), 3) * 100,
        `Whiff %` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Usage %` = round(`Usage %` / sum(`Usage %`), 3) * 100, `1P%` = round(`1P` / sum(`1P`), 3) * 100, `2K%` = round(`2K` / sum(`2K`), 3) * 100) %>%
      select(-`1P`, -`2K`)
  })
  
  # Render filtered stats vs LHH table
  output$filteredStatsLTable <- renderTable({
    Pitcher <- filtered_data()
    Pitcher %>%
      filter(stand == 'L') %>%
      summarize(
        BF = n_distinct(inning, batter),
        K = sum(events == "strikeout"),
        BB = sum(events == "walk"),
        HBP = sum(events == 'hit_by_pitch'),
        H = sum(events %in% c('single', 'double', 'triple', 'home_run')),
        `1B` = sum(events == 'single'),
        `2B` = sum(events == 'double'),
        `3B` = sum(events == 'triple'),
        HR = sum(events == 'home_run'),
        AVG = round(H / (BF - BB - HBP), 3),
        SLG = round(sum((`1B` * 1) + (`2B` * 2) + (`3B` * 3) + (HR * 4)) / (BF - BB - HBP), 3)
      ) %>%
      select(BF, K, BB, HBP, H, HR, AVG, SLG)
  })
  
  # Render filtered velocity plot
  output$filteredVeloPlot <- renderPlotly({
    Pitcher <- filtered_data()
    
    plot <- ggplot(Pitcher, aes(x = as.factor(inning), y = release_speed, fill = as.factor(inning))) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      scale_fill_manual(values = c('1' = 'red', '2' = 'darkgreen', '3' = '#f47b20', '4' = 'cornflowerblue', '5' = 'yellow', '6' = 'lightblue', '7' = 'purple')) +
      labs(title = "Pitch Velocity by Inning", x = "Inning", y = "Pitch Velocity (MPH)", fill = "Inning") +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.text = element_text(size = 10), legend.position = "right", legend.text = element_text(size = 10), axis.title = element_text(size = 12))
    
    ggplotly(plot)
  })
  
  # Render filtered zone plot
  output$filteredZonePlot <- renderPlotly({
    req(input$zone_pitch_type)
    
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    
    create_plot <- function(pitch_type, data) {
      data_filtered <- data %>% filter(pitch_type == !!pitch_type)
      p <- ggplot(data_filtered, aes(px, pz, color = ball_strike)) + 
        geom_point(aes(text = paste0("Velo: ",release_speed,"<br>",
                                     "Spin Axis: ",spin_axis,"</br>",
                                     "Event:  ",description,"</br>",
                                     "Spin Rate:  ",release_spin_rate,"</br>",
                                     "Release Ext:  ",release_extension,"</br>"
        )),size = 3, alpha = 0.7) +
        geom_path(data = kZone, aes(x = x, y = y), lwd = 1, col = "blue") +
        scale_color_manual(values = c("Strike" = "red", "Ball" = "green")) +
        ggtitle(paste("Pitch Locations: ", pitch_type)) + 
        xlim(-1.5, 1.5) +  # Adjust these limits to zoom in as needed
        ylim(1, 4) +       # Adjust these limits to zoom in as needed
        coord_fixed()
      
      ggplotly(p, tooltip = c("text", "x", "y"))
    }
    
    bip2 <- filtered_data() %>%
      mutate(
        px = as.numeric(as.character(plate_x)),
        pz = as.numeric(as.character(plate_z)),
        ball_strike = ifelse(description %in% c("called_strike", "swinging_strike", "foul"), "Strike", "Ball")
      )
    
    create_plot(input$zone_pitch_type, bip2)
  })
  
  # Render filtered release height plot
  output$filteredReleaseHeightPlot <- renderPlotly({
    Pitcher <- filtered_data()
    pitch_types <- unique(Pitcher$pitch_type)
    
    plot_list <- lapply(pitch_types, function(pitch) {
      pitch_data <- Pitcher %>% filter(pitch_type == pitch)
      
      gg <- ggplot(pitch_data, aes(x = release_pos_x, y = release_pos_z)) + 
        geom_point(aes(
          text = paste0(
            "Inning:", inning, "<br>",
            "Pitch Type: ", pitch_type, "<br>",
            "Velo: ", release_speed, "<br>",
            "Spin Axis: ", spin_axis, "<br>",
            "Event: ", description, "<br>",
            "Spin Rate: ", release_spin_rate, "<br>",
            "Release Ext: ", release_extension
          ))) + 
        labs(title = paste("RH", "Pitches"), x = "RP X (feet)", y = "RP Z (feet)") + 
        theme_minimal()
      
      ggplotly(gg)
    })
    
    plotly::subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
  })
  
  ###############################################
  
  # Update pitch type choices for zone plot based on fetched data
  observeEvent(pitcher_data(), {
    updateSelectInput(session, "zone_pitch_type", choices = unique(pitcher_data()$pitch_type))
  })
  
  # Plot for Pitcher
  output$pitchPlot <- renderPlotly({
    Pitcher <- pitcher_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    pitch_colors <- c("4-Seam Fastball" = "red", "Curveball" = "blue", "Cutter" = "cyan", "Pitch Out" = "violet", "Sinker" = "black", "Slider" = "green", "Slurve" = "pink", "Split-Finger" = "orange", "PO" = "gray50", "SV" = "beige", "Change-Up" = "gold")
    pitch_types <- unique(cleaned_data$pitch_name)
    
    gg <- cleaned_data %>% 
      ggplot(aes(x = pfx_x_in_pv, y = pfx_z_in, color = pitch_name)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(aes(text = paste0("Velo: ",release_speed,"<br>",
                                   "Spin Axis: ",spin_axis,"</br>",
                                   "Event:  ",description,"</br>",
                                   "Spin Rate:  ",release_spin_rate,"</br>",
                                   "Release Ext:  ",release_extension,"</br>"
      )),size = 1.5, alpha = 0.25) +
      scale_color_manual(values = pitch_colors, limits = pitch_types) +
      scale_x_continuous(limits = c(-25, 25), breaks = seq(-20, 20, 5), labels = scales::number_format(suffix = "\"")) +
      scale_y_continuous(limits = c(-25, 25), breaks = seq(-20, 20, 5), labels = scales::number_format(suffix = "\"")) +
      coord_equal() +
      labs(title = "Pitch Movement", subtitle = "2024 MLB Season | Pitcher's POV", caption = "Data: Baseball Savant via baseballr", x = "Horizontal Break", y = "Induced Vertical Break", color = "Pitch Type")
    
    ggplotly(gg)
  })
  
  # Table for Pitch Usage
  output$pitchUsageTable <- renderTable({
    Pitcher <- pitcher_data()
    Pitcher %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `#` = n(),
        `Use%` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strk%` = round(sum(description %in% c("swinging_strike", "foul", "hit_into_play", "called_strike"))/n(), 3) * 100,
        `Whiff%` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Use%` = round(`Use%` / sum(`Use%`), 3) * 100)
  })
  
  # Usage RHH Table
  output$usageRTable <- renderTable({
    Pitcher <- pitcher_data()
    Pitcher %>%
      filter(stand == 'R') %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `No.` = n(),
        `Usage %` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strike %` = round(sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play", "called_strike")) / n(), 3) * 100,
        `Whiff %` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Usage %` = round(`Usage %` / sum(`Usage %`), 3) * 100, `1P%` = round(`1P` / sum(`1P`), 3) * 100, `2K%` = round(`2K` / sum(`2K`), 3) * 100) %>%
      select(-`1P`, -`2K`)
  })
  
  # Stats vs RHH
  output$statsRTable <- renderTable({
    Pitcher <- pitcher_data()
    Pitcher %>%
      filter(stand == 'R') %>%
      summarize(
        BF = n_distinct(inning, batter),
        K = sum(events == "strikeout"),
        BB = sum(events == "walk"),
        HBP = sum(events == 'hit_by_pitch'),
        H = sum(events %in% c('single', 'double', 'triple', 'home_run')),
        `1B` = sum(events == 'single'),
        `2B` = sum(events == 'double'),
        `3B` = sum(events == 'triple'),
        HR = sum(events == 'home_run'),
        AVG = round(H / (BF - BB - HBP), 3),
        SLG = round(sum((`1B` * 1) + (`2B` * 2) + (`3B` * 3) + (HR * 4)) / (BF - BB - HBP), 3)
      ) %>%
      select(BF, K, BB, HBP, H, HR, AVG, SLG)
  })
  
  # Usage LHH Table
  output$usageLTable <- renderTable({
    Pitcher <- pitcher_data()
    Pitcher %>%
      filter(stand == 'L') %>%
      group_by(Pitch = pitch_type) %>%
      summarize(
        `No.` = n(),
        `Usage %` = n(),
        `1P` = sum(balls == 0 & strikes == 0),
        `2K` = sum(strikes == 2),
        `Strike %` = round(sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play", "called_strike")) / n(), 3) * 100,
        `Whiff %` = round(sum(description %in% c("swinging_strike")) / sum(description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play")), 3) * 100
      ) %>%
      mutate(`Usage %` = round(`Usage %` / sum(`Usage %`), 3) * 100, `1P%` = round(`1P` / sum(`1P`), 3) * 100, `2K%` = round(`2K` / sum(`2K`), 3) * 100) %>%
      select(-`1P`, -`2K`)
  })
  
  # Stats vs LHH
  output$statsLTable <- renderTable({
    Pitcher <- pitcher_data()
    Pitcher %>%
      filter(stand == 'L') %>%
      summarize(
        BF = n_distinct(inning, batter),
        K = sum(events == "strikeout"),
        BB = sum(events == "walk"),
        HBP = sum(events == 'hit_by_pitch'),
        H = sum(events %in% c('single', 'double', 'triple', 'home_run')),
        `1B` = sum(events == 'single'),
        `2B` = sum(events == 'double'),
        `3B` = sum(events == 'triple'),
        HR = sum(events == 'home_run'),
        AVG = round(H / (BF - BB - HBP), 3),
        SLG = round(sum((`1B` * 1) + (`2B` * 2) + (`3B` * 3) + (HR * 4)) / (BF - BB - HBP), 3)
      ) %>%
      select(BF, K, BB, HBP, H, HR, AVG, SLG)
  })
  
  # Velocity Plot
  output$veloPlot <- renderPlotly({
    Pitcher <- pitcher_data()
    
    plot <- ggplot(Pitcher, aes(x = as.factor(inning), y = release_speed, fill = as.factor(inning))) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      scale_fill_manual(values = c('1' = 'red', '2' = 'darkgreen', '3' = '#f47b20', '4' = 'cornflowerblue', '5' = 'yellow', '6' = 'lightblue', '7' = 'purple')) +
      labs(title = "Pitch Velocity by Inning", x = "Inning", y = "Pitch Velocity (MPH)", fill = "Inning") +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.text = element_text(size = 10), legend.position = "right", legend.text = element_text(size = 10), axis.title = element_text(size = 12))
    
    ggplotly(plot)
  })
  
  # Zone Plot
  observeEvent(pitcher_data(), {
    bip2 <- pitcher_data() %>%
      mutate(
        px = as.numeric(as.character(plate_x)),
        pz = as.numeric(as.character(plate_z)),
        ball_strike = ifelse(description %in% c("called_strike", "swinging_strike", "foul"), "Strike", "Ball")
      )
    
    updateSelectInput(session, "zone_pitch_type", choices = unique(bip2$pitch_type))
  })
  
  output$zonePlot <- renderPlotly({
    req(input$zone_pitch_type)
    
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    
    create_plot <- function(pitch_type, data) {
      data_filtered <- data %>% filter(pitch_type == !!pitch_type)
      p <- ggplot(data_filtered, aes(px, pz, color = ball_strike)) + 
        geom_point(aes(text = paste0("Velo: ",release_speed,"<br>",
                                     "Spin Axis: ",spin_axis,"</br>",
                                     "Event:  ",description,"</br>",
                                     "Spin Rate:  ",release_spin_rate,"</br>",
                                     "Release Ext:  ",release_extension,"</br>"
        )),size = 3, alpha = 0.7) +
        geom_path(data = kZone, aes(x = x, y = y), lwd = 1, col = "blue") +
        scale_color_manual(values = c("Strike" = "red", "Ball" = "green")) +
        ggtitle(paste("Pitch Locations: ", pitch_type)) + 
        xlim(-1.5, 1.5) +  # Adjust these limits to zoom in as needed
        ylim(1, 4) +       # Adjust these limits to zoom in as needed
        coord_fixed()
      
      ggplotly(p, tooltip = c("text", "x", "y"))
    }
    
    bip2 <- pitcher_data() %>%
      mutate(
        px = as.numeric(as.character(plate_x)),
        pz = as.numeric(as.character(plate_z)),
        ball_strike = ifelse(description %in% c("called_strike", "swinging_strike", "foul"), "Strike", "Ball")
      )
    
    create_plot(input$zone_pitch_type, bip2)
  })
  
  # Render release height plot
  output$releaseHeightPlot <- renderPlotly({
    Pitcher <- pitcher_data()
    pitch_types <- unique(Pitcher$pitch_type)
    
    plot_list <- lapply(pitch_types, function(pitch) {
      pitch_data <- Pitcher %>% filter(pitch_type == pitch)
      
      gg <- ggplot(pitch_data, aes(x = release_pos_x, y = release_pos_z)) + 
        geom_point(aes(
          text = paste0(
            "Pitch Type: ", pitch_type, "<br>",
            "Velo: ", release_speed, "<br>",
            "Spin Axis: ", spin_axis, "<br>",
            "Event: ", description, "<br>",
            "Spin Rate: ", release_spin_rate, "<br>",
            "Release Ext: ", release_extension
          )
        ), color = "black") + 
        labs(
          title = paste("RH-x", "RH-Y"), 
          x = "RH-Y", 
          y = "RH-X"
        ) + 
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_text(size = 10)  # Adjust font size here
        )
      
      ggplotly(gg, tooltip = "text")
    })
    
    plotly::subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
  })
  
  # Define the coordinates for the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  # R vs L Pitch Plot
  output$rVsLPitchPlots <- renderUI({
    req(pitcher_data())
    
    Pitcher <- pitcher_data()
    
    bip2 <- Pitcher %>%
      mutate(
        px = as.numeric(as.character(plate_x)),
        pz = as.numeric(as.character(plate_z)),
        ball_strike = ifelse(description %in% c("called_strike", "swinging_strike", "foul"), "Strike", "Ball"),
        stand = ifelse(stand == "L", "L", "R")  # Assuming batter_stand column exists
      )
    
    # Function to create individual plots for each pitch type and stand
    create_plot <- function(pitch_type, stand, data) {
      data_filtered <- data %>% filter(pitch_type == !!pitch_type, stand == !!stand)
      
      p <- ggplot(data_filtered, aes(px, pz, color = ball_strike)) + 
        geom_point(aes(text = paste0("Velo: ",release_speed,"<br>",
                                     "Spin Axis: ",spin_axis,"</br>",
                                     "Event:  ",description,"</br>",
                                     "Spin Rate:  ",release_spin_rate,"</br>",
                                     "Release Ext:  ",release_extension,"</br>"
        )),size = 3, alpha = 0.7) +
        geom_path(data = kZone, aes(x = x, y = y), lwd = 1, col = "blue") +
        geom_polygon(data = data.frame(
          x = c(-1, -0.5, 0, 0.5, 1, 0),
          y = c(0, -0.5, -1, -0.5, 0, 0)
        ), aes(x = x, y = y), fill = "white", color = "black", size = 1) +
        scale_color_manual(values = c("Strike" = "red", "Ball" = "green")) +
        ggtitle(paste("Pitch Locations: ", pitch_type, " (Stand: ", stand, ")", sep = "")) + 
        xlim(-1.5, 1.5) +  # Adjust these limits to zoom in as needed
        ylim(1, 4) +       # Adjust these limits to zoom in as needed
        coord_fixed()
      
      ggplotly(p, tooltip = c("text", "x", "y"))
    }
    
    # Get unique combinations of pitch type and stand
    unique_combinations <- bip2 %>%
      select(pitch_type, stand) %>%
      distinct()
    
    # Create and display plots for each combination of pitch type and stand
    plot_list <- lapply(1:nrow(unique_combinations), function(i) {
      create_plot(unique_combinations$pitch_type[i], unique_combinations$stand[i], bip2)
    })
    
    plotly_output_list <- lapply(1:length(plot_list), function(i) {
      plotlyOutput(paste0("plot_", i))
    })
    
    output_list <- lapply(1:length(plot_list), function(i) {
      local({
        plot <- plot_list[[i]]
        plot_name <- paste0("plot_", i)
        output[[plot_name]] <- renderPlotly({ plot })
      })
    })
    
    do.call(tagList, plotly_output_list)
  })
  
  # Render summary table for the entire season
  output$summaryTableSeason1 <- renderTable({
    Pitcher <- pitcher_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Horizontal_Break = mean(pfx_x_in_pv, na.rm = TRUE),
        Mean_Induced_Vertical_Break = mean(pfx_z_in, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
  
  output$summaryTableSeason2 <- renderTable({
    Pitcher <- pitcher_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Launch_Angle = mean(launch_angle, na.rm = TRUE),
        Mean_Release_Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
  
  # Render summary table for the entire season
  output$summaryTableSeason3 <- renderTable({
    Pitcher <- pitcher_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Release_Extension = mean(release_extension, na.rm = TRUE),
        Mean_Swing_Length = mean(swing_length, na.rm = TRUE),
        Mean_Bat_Speed = mean(bat_speed, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
  
  # Render summary table for a single game
  output$summaryTableGame1 <- renderTable({
    Pitcher <- filtered_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Horizontal_Break = mean(pfx_x_in_pv, na.rm = TRUE),
        Mean_Induced_Vertical_Break = mean(pfx_z_in, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
  
  output$summaryTableGame2 <- renderTable({
    Pitcher <- filtered_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Launch_Angle = mean(launch_angle, na.rm = TRUE),
        Mean_Release_Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
  
  output$summaryTableGame3 <- renderTable({
    Pitcher <- filtered_data()
    cleaned_data <- Pitcher %>% 
      filter(!is.na(pfx_x), !is.na(pfx_z), game_type == "R") %>% 
      mutate(pfx_x_in_pv = -12*pfx_x, pfx_z_in = 12*pfx_z)
    
    cleaned_data %>% 
      group_by(pitch_name) %>% 
      summarize(
        Mean_Release_Extension = mean(release_extension, na.rm = TRUE),
        Mean_Swing_Length = mean(swing_length, na.rm = TRUE),
        Mean_Bat_Speed = mean(bat_speed, na.rm = TRUE),
        Count = n()
      ) %>% 
      arrange(desc(Count))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

