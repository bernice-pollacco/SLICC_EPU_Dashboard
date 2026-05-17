############################# EPU DASHBOARD ##########################

#Install and load necessary packages
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)



#importing the functions
source("functions_for_epu_dashboard.R")

#setting theme for standard plot fonts

theme_set(
  theme_minimal(base_family = "Roboto", base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12)
    )
)


ui <- dashboardPage(
  
  dashboardHeader(title = "Early Pregnancy Unit Dashboard"),
  
  dashboardSidebar(
    tags$head(                        #to fix the calendar issue
      tags$style(HTML("
    .datepicker-dropdown {
      z-index: 9999 !important;
    }
    .main-sidebar {
      overflow: visible !important;
    }
  "))
    ),
    
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Clinical Performance", tabName = "clinical", icon = icon("hospital")),
      menuItem("Staffing Levels", tabName = "staff", icon = icon("user-md")),
      menuItem("Insights", tabName = "insights", icon = icon("chart-line"))
    ),
      
    dateRangeInput(
          "date_range",
          "Date range:",
          start = as.Date("2025-01-01"),  # default start
          end   = as.Date("2025-03-31"),  # default end
          min   = as.Date("2025-01-01"),  # earliest selectable
          max   = as.Date("2025-12-31")   # latest selectable
        ),
        
    selectInput(
          "diagnosis",
          "Diagnosis:",
          choices = c("Early Miscarriage", "Late Miscarriage", "Ectopic Pregnancy",
                      "PUL", "IUD")
        ),
        
    selectInput(
          "shift_type",
          "Shift type:",
          choices = c("Day", "Night", "Morning","Evening")
        ),
  
  sliderInput(
    inputId = "staff_slider",
    label = "Number of Staff:",
    min = 0,
    max = 4,
    value = 2,   
    step = 1     
  )
  ),


  
  dashboardBody(
    
    tabItems(
      
      # ---------------- OVERVIEW TAB ----------------
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("wait_time_pred_value", width = 3),
          valueBoxOutput("avg_waiting_time_value", width = 3),
          valueBoxOutput("avg_los_value", width = 3),
          valueBoxOutput("avg_hrs_hcg_value", width = 3)
        ),
        
        
        fluidRow(
          valueBoxOutput("percent_us_first_visit_value", width = 3),
          valueBoxOutput("percent_med_management_success_value", width = 3),
          valueBoxOutput("ectopic_diagnosis_avg_value", width = 3),
          valueBoxOutput("percent_emergency_ectopic_surgery_value", width = 3)
        ),
        
       
        
        fluidRow(
          box(
            title = "Diagnosis at EPU",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("counts_per_diagnosis_plot", height = 250)
          ),
          
          box(
            title = "Management Type",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("management_type_barchart_plot", height = 250)
          )
          ),
        
        fluidRow(
          box(
            title = "Referral Sources",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("referral_source_barchart_plot", height = 250)
          ),
          
          box(
            title = "Attendance and Lost to Follow-Up",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("dna_ltfu_plot", height = 250)
          )
        )
      ), 
  
  
      
      # ---------------- CLINICAL TAB ----------------
      tabItem(
        tabName = "clinical",
        fluidRow(
          
          box(
            title = "Admissions",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("admissions_over_time_plot", height = 250)
          ), 
          
          box(
            title = "Reviews in Admission Room",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("seen_adm_rm_over_time_linechart_plot", height = 250)
          ), 
          
          box(
            title = "Emergency Surgery Rate",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("monthly_emergency_rate_plot", height = 250)
          ), 
          
          box(
            title = "Average Length of Stay",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("monthly_los_linechart_plot", height = 250)
          ), 
          
          box(
            title = "Average Time for Ectopic Diagnosis",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("avg_ectopic_diagnosis_linechart_plot", height = 250)
          )
      )
    ),
      
      # ---------------- STAFF TAB ----------------
      tabItem(
        tabName = "staff",
        
        fluidRow(
          valueBoxOutput("days_of_epu_core_value", width = 6),
          valueBoxOutput("days_of_pool_support_value", width = 6)
        ),
        
        fluidRow(
          box(
            title = "Staffing Levels",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("staff_line_chart_plot", height = 250)
          ),
          
          box(
            title = "Pool Staff Support",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotOutput("percent_pool_chart_plot", height = 250)
          )
          )
        ),
    
    #--------------------INSIGHTS TAB----------------------#
    tabItem(
      tabName = "insights",
      
      fluidRow(
        box(
          title = "Waiting Time per Diagnosis",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotOutput("rel_wait_diagnosis_plot", height = 250)
        ),
        
        box(
          title = "Emergency Surgery and Waiting Time",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotOutput("rel_wait_emsurgery_plot", height = 250)
        ),
        
        box(
          title = "Lenght of Stay and Diagnosis",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotOutput("rel_los_diagnosis_plot", height = 250)
        )
      )
    )
      )
    )
)
  



server <- function(input, output) {
  
  # ---------------- FUCTIONS FOR OVERVIEW TAB ----------------

  
  
  output$percent_emergency_ectopic_surgery_value <- renderValueBox({
    req(input$date_range)
    
    percent_value <- percent_emergency_ectopic_surgery_function(
      start_date = input$date_range[1],
      end_date   = input$date_range[2]
    ) 
    
    colour_choice <- case_when(
      percent_value < 30 ~ "green",
      percent_value < 40 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(percent_value, 1), "%"),
      subtitle = "% Emergency Ectopic Surgeries",
      icon = icon("exclamation-triangle"),
      color = colour_choice
    )
  })
  
  output$percent_us_first_visit_value <- renderValueBox({
    req(input$date_range)
    req(input$diagnosis)
    
    percent_value <- percent_us_first_visit_function(
      start_date = input$date_range[1],
      end_date   = input$date_range[2],
      diagnosis = input$diagnosis
    ) 
    
    colour_choice <- case_when(
      percent_value >= 95 ~ "green",
      percent_value >= 90 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(percent_value, 1), "%"),
      subtitle = "% Ultrasound Performed at 1st Visit",
      icon = icon("user-md"),
      color = colour_choice
    )
  })
  
  output$percent_med_management_success_value <- renderValueBox({
    req(input$date_range)
    req(input$diagnosis)
    
    percent_value <- percent_med_management_success_function(
      start_date = input$date_range[1],
      end_date   = input$date_range[2],
      diagnosis = input$diagnosis
    ) 
    
    colour_choice <- case_when(
      percent_value >= 85 ~ "green",
      percent_value >= 75 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(percent_value, 1), "%"),
      subtitle = "Medical Management Success Rate",
      icon = icon("pills"),
      color = colour_choice
    )
  })
  
  output$wait_time_pred_value <- renderValueBox({
    req(input$date_range)
    
    wait_pred_value <- wait_time_pred_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
    
    colour_choice <- case_when(
      wait_pred_value <= 60 ~ "green",
      wait_pred_value <= 120 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(wait_pred_value, 1), " mins"),
      subtitle = "Next day's Predicted Waiting Time",
      icon = icon("clock"),
      color = colour_choice
    )
  })
  
  
  output$avg_waiting_time_value <- renderValueBox({
    req(input$date_range)
    req(input$diagnosis)
    
    avg_value <- avg_waiting_time_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis  = input$diagnosis
    )
    
    colour_choice <- case_when(
      avg_value <= 1 ~ "green",
      avg_value <= 2 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(avg_value, 1), " hours"),
      subtitle = "Average Waiting Time",
      icon = icon("clock"),
      color = colour_choice
    )
  })
  
  output$avg_los_value <- renderValueBox({
    req(input$date_range)
    req(input$diagnosis)
    
    avg_los_value <- avg_los_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis  = input$diagnosis
    )
    
    colour_choice <- case_when(
      avg_los_value <= 48 ~ "green",
      avg_los_value <= 72 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(avg_los_value, 1), " hours"),
      subtitle = "Average Length Of Stay",
      icon = icon("procedures"),
      color = colour_choice
    )
  }) 
  
  output$avg_hrs_hcg_value <- renderValueBox({
    req(input$date_range)
    req(input$diagnosis)
    
    avg_hcg_value <- hcg_avg_hrs_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis  = input$diagnosis
    )
    
    colour_choice <- case_when(
      avg_hcg_value <= 2 ~ "green",
      avg_hcg_value < 4 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(avg_hcg_value, 1), " hours"),
      subtitle = "Average Time for hcG result",
      icon = icon("clock"),
      color = colour_choice
    )
  }) 
  
  output$ectopic_diagnosis_avg_value <- renderValueBox({
    req(input$date_range)
    
    avg_ectopic_value <- ectopic_diagnosis_avg_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
    
    colour_choice <- case_when(
      avg_ectopic_value < 12 ~ "green",
      avg_ectopic_value < 24 ~ "yellow",
      TRUE ~ "red"
    )
    
    valueBox(
      value = paste0(round(avg_ectopic_value, 1), " hours"),
      subtitle = "Average Time for Ectopic Diagnosis",
      icon = icon("clock"),
      color = colour_choice
    )
  })
  
  output$counts_per_diagnosis_plot <- renderPlot({
    req(input$date_range)
    counts_per_diagnosis_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
  })
  
  output$dna_ltfu_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    dna_ltfu_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  })
  
  output$referral_source_barchart_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    referral_source_barchart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  })
  
  
  output$management_type_barchart_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    management_type_barchart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  }) 
  
  # ---------------- FUNCTIONS FOR CLINICAL TAB ----------------
  
  output$admissions_over_time_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    admissions_over_time_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  }) 
  
  output$seen_adm_rm_over_time_linechart_plot <- renderPlot({
    req(input$date_range)
    
    
    seen_adm_rm_over_time_linechart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
  }) 
  
  output$monthly_emergency_rate_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    monthly_emergency_rate_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  }) 
  
  output$monthly_los_linechart_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    monthly_los_linechart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  }) 
  
  output$avg_ectopic_diagnosis_linechart_plot <- renderPlot({
    req(input$date_range)
    
    
    avg_ectopic_diagnosis_linechart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
  }) 
  
  # ---------------- FUNCTIONS FOR STAFF TAB ----------------
  
  output$days_of_epu_core_value <- renderValueBox({
    req(input$date_range)
    req(input$staff_slider)
    req(input$shift_type)
    
    total_days <- as.numeric(input$date_range[2] - input$date_range[1]) + 1
    
    epu_value <- days_of_epu_core_function(
      start_date = input$date_range[1],
      end_date   = input$date_range[2],
      num_staff = input$staff_slider,
      shift = input$shift_type
    ) 
    
    valueBox(
      value = paste0(epu_value, " days out of ", total_days),
      subtitle = paste("Days with", input$staff_slider, "EPU staff members"),
      icon = icon("user-md"),
      color = "maroon"
    )
  })
  
  
  output$days_of_pool_support_value <- renderValueBox({
    req(input$date_range)
    req(input$staff_slider)
    req(input$shift_type)
    
    total_days <- as.numeric(input$date_range[2] - input$date_range[1]) + 1
    
    pool_value <- days_of_pool_support_function(
      start_date = input$date_range[1],
      end_date   = input$date_range[2],
      num_staff = input$staff_slider,
      shift = input$shift_type
    ) 
    
    valueBox(
      value = paste0(pool_value, " days out of ", total_days),
      subtitle = paste("Days with", input$staff_slider , "Pool staff members"),
      icon = icon("user-md"),
      color = "aqua"
    )
  })
  
  
  output$staff_line_chart_plot <- renderPlot({
    req(input$date_range)
    req(input$shift_type)
    
    
    staff_line_chart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      shift = input$shift_type
    )
  }) 
  
  output$percent_pool_chart_plot <- renderPlot({
    req(input$date_range)
    req(input$shift_type)
    
    
    percent_pool_chart_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      shift = input$shift_type
    )
  })
  
  #-------------------------FUNCTIONS FOR INSIGHTS TAB-------------------
  
  
  output$rel_wait_diagnosis_plot <- renderPlot({
    req(input$date_range)
    
    
    rel_wait_diagnosis_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
  }) 
  
  output$rel_wait_emsurgery_plot <- renderPlot({
    req(input$date_range)
    req(input$diagnosis)
    
    
    rel_wait_emsurgery_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      diagnosis = input$diagnosis
    )
  }) 
  
  output$rel_los_diagnosis_plot <- renderPlot({
    req(input$date_range)
    
    
    rel_los_diagnosis_function(
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
  }) 

}

shinyApp(ui, server)

