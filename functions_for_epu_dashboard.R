#EPU DASHBOARD FUCTIONS
#in order of appearance

library(tidyverse)

#load datasets
episodes_raw <- read_csv("data/EPU_Episodes_v2.csv")
lab_results_raw <- read_csv("data/EPU_Lab_Results_v2.csv")
staff_shifts_raw <- read_csv("data/EPU_Staffing_Shifts_v2.csv")

#joining labs dataset to episodes dataset

episodes_labs_joined <-  full_join(episodes_raw, lab_results_raw, by = "Episode_ID")

#Setting up dates for function use
episodes_with_date <- episodes_labs_joined %>%
  mutate(admission_day = as.Date(Admission_Date),
         seen_date = as.Date(Seen_in_Admission_Time),
  )

#specifying start and end dates for the purpose of testing out functions
#used in most functions unless otherwise specified
#start_date <- as.Date("2025-01-01")
#end_date   <- as.Date("2025-01-31")



##################################OVERVIEW TAB#############################

#Predicting waiting time
#setting up the df with num of patients seen and avg waiting time per day
wait_time_pred_function <- function(start_date, end_date){
  adm_rm_data <- episodes_with_date %>%
    mutate(
      waiting_time = as.numeric(difftime(Seen_in_Admission_Time,
                                         Arrival_Time,
                                         units = "mins"))) %>%
    group_by(seen_date) %>%
    summarise(
      num_seen = n(),
      avg_waiting_time = mean(waiting_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      seen_date = seq.Date(start_date, end_date, by = "day"),
      fill = list(num_seen = 0, avg_waiting_time = NA)
    ) %>% 
    arrange(seen_date) %>%
    mutate(
      wait_lag = lag(avg_waiting_time))
  
  wait_time_model <- lm(avg_waiting_time ~ wait_lag + num_seen,
                        data = adm_rm_data)
  
  latest <- adm_rm_data %>%
    filter(!is.na(avg_waiting_time)) %>%   
    arrange(desc(seen_date)) %>%           
    slice(1)
  
  current_data <- data.frame(
    wait_lag = latest$avg_waiting_time,
    num_seen = latest$num_seen
  )
  
  predict(wait_time_model, newdata = current_data)
}

#wait_time_pred_function()
#------------------------------------------------------------

#Average waiting time
avg_waiting_time_function <- function(start_date, end_date, diagnosis){
  wait <- episodes_with_date %>%
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis) %>%
    mutate(
      waiting_time = as.numeric(difftime(Seen_in_Admission_Time, Arrival_Time, units = "hours")))
  Avg_waiting_time <- mean(wait$waiting_time, na.rm = TRUE)
  Avg_waiting_time  
} 

#avg_waiting_time_function(start_date, end_date, "Ectopic Pregnancy")

#------------------------------------------------

#Average Length of Stay

avg_los_function <- function(start_date, end_date, diagnosis){
  los <- episodes_with_date %>%
    filter(
      Admission_Required=="Yes",
      seen_date >= start_date & seen_date <= end_date,
      Outcome_Category == diagnosis
    ) %>% 
    mutate(
      LOS = as.numeric(difftime(Discharge_Date, Admission_Date, units = "hours")))
  Avg_los <-  mean(los$LOS, na.rm = TRUE)
  Avg_los
}

#avg_los_function(start_date, end_date, "Early Miscarriage")

#---------------------------------------------------
#Average time for hcG result
hcg_avg_hrs_function <- function(start_date, end_date, diagnosis){
  hcg <- episodes_with_date %>%
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis) %>%
    mutate(
      hcg_time = as.numeric(difftime(hCG_Result_Time, hCG_Sample_Time, units = "hours")))
  Avg_hcg_time = mean(hcg$hcg_time, na.rm = TRUE)
  Avg_hcg_time
}   

#hcg_avg_hrs_function(start_date, end_date, "Early Miscarriage")

#----------------------------------------------------

#Percentage US performed at first visit
percent_us_first_visit_function <- function(start_date, end_date, diagnosis){
  percent_us <- episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis)
  
  percent_us_first_visit <- mean(percent_us$Ultrasound_First_Visit == "Yes") * 100  
  round(percent_us_first_visit, 2)
  
}

#percent_us_first_visit_function(start_date, end_date, "Early Miscarriage")
#---------------------------------------

#Percentage Medical Management success

percent_med_management_success_function <- function(start_date, end_date, diagnosis){
  med_management <- episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis)
  
  percent_med_management_success <- mean(med_management$Medical_Management_Success == "Yes") * 100  
  
}

#percent_med_management_success_function(start_date, end_date, "Early Miscarriage")

#----------------------------------------

#Average time for ectopic diagnosis
ectopic_diagnosis_avg_function <- function(start_date, end_date){
  ectopic <- episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category=="Ectopic Pregnancy") %>% 
    mutate(
      ectopic_diagnosis_time = as.numeric(difftime(Diagnosis_Time, Seen_in_Admission_Time, units = "hours")))
  Avg_ectopic_diagnosis_time <- mean(ectopic$ectopic_diagnosis_time, na.rm = TRUE)
  
  Avg_ectopic_diagnosis_time
  
}

#ectopic_diagnosis_avg_function(start_date, end_date)

#-------------------------------------------

#Percentage Emergency Surgery for Ectopics
percent_emergency_ectopic_surgery_function <- function(start_date, end_date){
 em_ectopic <- episodes_with_date %>%
  filter(Outcome_Category=="Ectopic Pregnancy",
        Management_Type=="Surgical",
       seen_date >= start_date & seen_date <= end_date)
 percent_emergency_surgery <- mean(em_ectopic$Emergency_Surgery == "Yes") * 100
 percent_emergency_surgery
 
}

#percent_emergency_ectopic_surgery_function(start_date, end_date)

#--------------------------------------------


#Diagnosis at EPU
counts_per_diagnosis_function <- function(start_date, end_date){
  episodes_with_date %>% 
    filter(seen_date>=start_date & seen_date<=end_date) %>% 
    ggplot(aes(x=Outcome_Category, fill = Outcome_Category))+
    geom_bar()+
    coord_flip()+
    geom_text(
      stat = "count",
      aes(label = after_stat(count)))+
    theme(legend.position = "none")+
    labs(title="Counts of Diagnosis at EPU",
         x= "Diagnosis",
         y= "Count")
}
#--------------------------------------------------------

#Management Type
management_type_barchart_function <- function(start_date, end_date, diagnosis){
  episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis) %>% 
    ggplot(aes(x=Management_Type, fill = Management_Type))+
    geom_bar()+
    geom_text(
      stat = "count",
      aes(label = after_stat(count)))+
    theme(legend.position = "none")+
    labs(title=paste("Management Type for", diagnosis, "at EPU"),
         x= "Management Type",
         y= "Count")
}

#management_type_barchart_function(start_date, end_date, "Early Miscarriage")
#-----------------------------------------------------------

#Referral sources
referral_source_barchart_function <- function(start_date, end_date, diagnosis){
  episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis) %>% 
    ggplot(aes(x=Referral_Source, fill = Referral_Source))+
    geom_bar()+
    geom_text(
      stat = "count",
      aes(label = after_stat(count)))+
    theme(legend.position = "none")+
    labs(title="Referal Source to EPU",
         x= "Referal Source",
         y= "Count")
}

#referral_source_barchart_function(start_date, end_date, "Early Miscarriage")
#----------------------------------------------------------

#Attendance and lost to follow up
dna_ltfu_function <- function(start_date, end_date, diagnosis){
  episodes_with_date %>% 
    filter(seen_date>=start_date & seen_date<=end_date) %>% 
    filter(Outcome_Category==diagnosis) %>% 
    select(Lost_to_Followup, DNA) %>% 
    pivot_longer(cols = c(Lost_to_Followup, DNA),
                 names_to = "Outcome",
                 values_to = "Response") %>% 
    ggplot(aes(x = Outcome, fill = Response)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c(
      "Yes" = "#E34234",
      "No" = "#93C572"))+
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      position = position_dodge(width = 0.9))+
    labs(title = "Counts for Attendance and Lost to Follow-up",
         subtitle = paste("For patients diagnosed with", diagnosis),
         y = "Number of Patients")
}

#dna_ltfu_function(start_date, end_date, "Early Miscarriage")
#---------------------------------------------------------

############################ CLINICAL PERFORMANCE TAB #####################

#Admissions

admissions_over_time_function <- function(start_date, end_date, diagnosis){
  admission_data <- episodes_with_date %>%
    filter(Admission_Required == "Yes",
           admission_day >= start_date & admission_day <= end_date,
           Outcome_Category==diagnosis) %>% 
    group_by(admission_day, Outcome_Category) %>%
    summarise(num_admissions = n(), .groups = "drop") %>% 
    complete(
      admission_day = seq.Date(start_date, end_date, by = "day"),
      fill = list(num_admissions = 0)
    )
  
  ggplot(admission_data, aes(x = admission_day, y = num_admissions)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "black") +
    scale_x_date(date_labels = "%d %b", date_breaks = "3 day") +
    scale_y_continuous(
      breaks = seq(0, max(admission_data$num_admissions), by = 1)
    )+
    labs(
      title = paste("Number of Admissions due to", diagnosis),
      subtitle = paste("Between", start_date, "-", end_date),
      x = "Date",
      y = "Number of Admissions"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#admissions_over_time_function(start_date, end_date, "Early Miscarriage")
#---------------------------------------------------------------

#Reviews in Admission Room

seen_adm_rm_over_time_linechart_function <- function(start_date, end_date){
  adm_rm_data <- episodes_with_date %>%
    filter(seen_date >= start_date & seen_date <= end_date) %>%
    group_by(seen_date) %>%
    summarise(num_seen = n(), .groups = "drop") %>% 
    complete(
      seen_date = seq.Date(start_date, end_date, by = "day"),
      fill = list(num_seen = 0)
    )
  
  ggplot(adm_rm_data, aes(x = seen_date, y = num_seen)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "red") +
    scale_x_date(date_labels = "%d %b", date_breaks = "3 day") +
    scale_y_continuous(
      breaks = seq(0, max(adm_rm_data$num_seen), by = 1)
    )+
    labs(
      title = "Number of Patients Seen in Admission Room",
      subtitle = paste("Between", start_date, "-", end_date),
      x = "Date",
      y = "Number of Patients Seen"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} 

#seen_adm_rm_over_time_linechart_function(start_date, end_date)
#----------------------------------------------------------

#Monthly emergency surgery trend

monthly_emergency_rate_function <- function(start_date, end_date, diagnosis){
  emergency_data <- episodes_with_date %>% 
    mutate(month = floor_date(seen_date, "month")) %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category == diagnosis) %>% 
    group_by(month) %>%
    summarise(
      total_patients = n(),
      surgical_cases = sum(Management_Type == "Surgical"),
      surgical_percent = mean(Management_Type == "Surgical") * 100,
      .groups = "drop"
    )
  
  max_patients <- max(emergency_data$total_patients)
  
  ggplot(emergency_data, aes(x = month)) +
    geom_line(aes(y = total_patients, color = "Total patients"), linewidth = 1) +
    geom_line(aes(y = surgical_cases, color = "Surgical Cases"), linewidth = 1) +
    geom_line(
      aes(y = surgical_percent * max_patients / 100,
          color = "Surgical %"),
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "2 month") +
    scale_y_continuous(
      name = "Number of Patients",
      sec.axis = sec_axis(~ . / max_patients * 100,
                          name = "Surgical (%)")
    ) +
    geom_hline(
      yintercept = 40 * max_patients / 100,
      linetype = "dashed",
      color = "red"             #threshold line
    ) +
    scale_color_manual(values = c(
      "Total patients" = "darkgreen",
      "Surgical Cases" = "steelblue",
      "Surgical %" = "firebrick"
    )) +
    labs(
      title = paste("Emergency Surgical Management and Surgery Rate for", diagnosis),
      x = "Month",
      color = "Metric"
    ) 
}

#start_date = as.Date("2025-01-01")
#end_date = as.Date("2025-12-31")

#monthly_emergency_rate_function(start_date, end_date, "Ectopic Pregnancy")
#---------------------------------------------------------------

#Monthly LOS trend
monthly_avg_los_function <- function(start_date, end_date, diagnosis){
  episodes_with_date %>%
    filter(Admission_Required=="Yes",
           seen_date >= start_date & seen_date <= end_date,
           Outcome_Category==diagnosis) %>%
    mutate(
      Avg_LOS = as.numeric(difftime(Discharge_Date, Admission_Date, units = "hours")),
      Admission_Month = as.Date(floor_date(Admission_Date, "month")
      )) %>%
    group_by(Admission_Month) %>%
    summarise(
      avg_los = mean(Avg_LOS, na.rm = TRUE),
      Patients = n()
    )
  
} 

monthly_los_linechart_function <- function(start_date, end_date, diagnosis){
  monthly_avg_los_function(start_date, end_date, diagnosis) %>% 
    ggplot(aes(x = Admission_Month, y = avg_los)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "steelblue", size = 3) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
    labs(
      title = "Monthly Average Length of Stay Trend",
      subtitle = paste("Between", start_date, "-", end_date),
      x = "Month",
      y = "Average LOS (Hours)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} 

#start_date = as.Date("2025-01-01")
#end_date = as.Date("2025-12-31")

#monthly_los_linechart_function(start_date, end_date, "Early Miscarriage")
#-----------------------------------------------------------

#Average ectopic diagnosis over time

avg_monthly_ectopic_diagnosis_function <- function(start_date, end_date){
  episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category=="Ectopic Pregnancy") %>% 
    mutate(
      ectopic_diagnosis_time = as.numeric(difftime(Diagnosis_Time, Seen_in_Admission_Time, units = "hours")),
      Ectopic_Review_Month = as.Date(floor_date(Seen_in_Admission_Time, "month")
      )) %>%
    group_by(Ectopic_Review_Month) %>%
    summarise(
      Avg_ectopic_diagnosis_time = mean(ectopic_diagnosis_time, na.rm = TRUE),
      Patients = n()
    )
} 

avg_ectopic_diagnosis_linechart_function <- function(start_date, end_date){
  avg_monthly_ectopic_diagnosis_function(start_date, end_date) %>% 
    ggplot(aes(x = Ectopic_Review_Month, y = Avg_ectopic_diagnosis_time)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "steelblue", size = 3) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
    labs(
      title = "Monthly Average Time (Hrs) for an Ectopic Diagnosis Trend",
      subtitle = paste("Between", start_date, "-", end_date),
      x = "Month",
      y = "Average time to Diagnosis (Hours)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} 

#start_date = as.Date("2025-01-01")
#end_date = as.Date("2025-12-31")

#avg_ectopic_diagnosis_linechart_function(start_date, end_date)

#----------------------------------------------------------------

################################# STAFF TAB ############################

#Create dataset with counts of staff type per shift type per date

staff_summary <- staff_shifts_raw %>%
  group_by(Date, Staff_Type, Shift_Type) %>%
  summarise(total_staff = sum(Staff_On_Duty), .groups = "drop")

staff_wide <- staff_summary %>%
  pivot_wider(
    names_from = Staff_Type,
    values_from = total_staff,
    values_fill = 0
  ) %>%
  mutate(
    total_staff = `EPU Core` + Pool,
    pool_percentage = (Pool / total_staff) * 100
  )

#----------------------------------------------------------------------

#Staffing levels

#Number of days with EPU core staff

days_of_epu_core_function <- function(start_date, end_date, num_staff, shift){
  staff_wide %>% 
    filter(Date >= start_date, Date <= end_date,
           `EPU Core`==num_staff,
           Shift_Type==shift) %>% 
    count()
}

#days_of_epu_core_function(start_date, end_date, "2", "Day")

#Number of days with pool staff

days_of_pool_support_function <- function(start_date, end_date, num_staff, shift){
  staff_wide %>% 
    filter(Date >= start_date, Date <= end_date,
           Pool==num_staff,
           Shift_Type==shift) %>% 
    count()
}


staff_line_chart_function <- function(start_date, end_date, shift){
  staff_wide %>% 
    filter(Date >= start_date & Date <= end_date,
           Shift_Type==shift) %>% 
    ggplot(aes(x = Date)) +
    geom_line(aes(y = `EPU Core`, color = "Ward staff"), linewidth = 1) +
    geom_line(aes(y = Pool, color = "Pool staff"), linewidth = 1) +
    geom_line(aes(y = total_staff, color = "Total staff"), linewidth = 1.2) +
    scale_y_continuous(name = "Number of Staff") +
    scale_color_manual(values = c(
      "Ward staff" = "#D55E00",
      "Pool staff" = "#4477AA",
      "Total staff" = "black"
    )) +
    scale_x_date(date_labels = "%d-%b", date_breaks = "3 day") +
    labs(
      title = paste("EPU Staff and Pool Support During", shift, "Shift Over Time"),
      x = "Date",
      color = "Staff Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#start_date = as.Date("2025-01-01")
#end_date = as.Date("2025-01-31")


#staff_line_chart_function(start_date, end_date, "Day")
#staff_line_chart_function(start_date, end_date, "Night")
#staff_line_chart_function(start_date, end_date, "Morning")
#staff_line_chart_function(start_date, end_date, "Evening")

#----------------------------------------------------------------------

#Pool staff Support

percent_pool_chart_function <- function(start_date, end_date, shift){
  staff_wide %>% 
    filter(Date >= start_date & Date <= end_date,
           Shift_Type==shift) %>% 
    ggplot(aes(x = Date, y = pool_percentage)) +
    geom_line(linewidth = 1, color = "black") +
    geom_point(size = 2, color = "firebrick") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red")+ #to indicate threshold
    scale_x_date(date_labels = "%d-%b", date_breaks = "3 day") +
    labs(
      title = paste("Percentage Pool Staff Support During", shift, "Shift Over Time"),
      x = "Date",
      y = "Pool Staff (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#start_date = as.Date("2025-01-01")
#end_date = as.Date("2025-01-31")

#percent_pool_chart_function(start_date, end_date, "Day")
#percent_pool_chart_function(start_date, end_date, "Night")
#percent_pool_chart_function(start_date, end_date, "Morning")
#percent_pool_chart_function(start_date, end_date, "Evening")

####################################INSIGHTS TAB############################

#Relationship between Diagnosis and Waiting time

rel_wait_diagnosis_function <- function(start_date, end_date){
  episodes_with_date %>%
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category!="Ongoing Pregnancy") %>%
    mutate(
      waiting_time = as.numeric(difftime(Seen_in_Admission_Time, Arrival_Time, units = "hours"))) %>% 
    ggplot(aes(x=Outcome_Category, y=waiting_time, fill=Outcome_Category))+
    geom_boxplot()+
    labs(title = "Relationship between Diagnosis and Waiting Time (hrs)",
         x= "Diagnosis",
         y= "Waiting Time (hrs)")+
    theme(legend.position = "none")
  
}

#rel_wait_diagnosis_function(start_date, end_date)
#----------------------------------------------------------

#Relationship between emergency surgery (yes/no) and waiting time per diagnosis

rel_wait_emsurgery_function <- function(start_date, end_date, diagnosis){
  episodes_with_date %>% 
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category == diagnosis) %>%
    mutate(
      waiting_time = as.numeric(difftime(Seen_in_Admission_Time, Arrival_Time, units = "hours"))) %>% 
    ggplot(aes(x=Emergency_Surgery, y=waiting_time))+
    geom_boxplot()+
    labs(title = "Relationship between Emergency Surgery and Waiting Time",
         subtitle = paste("For", diagnosis),
         x= "Emergency Surgery",
         y= "Waiting Time (hrs)")
}

#--------------------------------------------------------------

#relationship between los and diagnosis
rel_los_diagnosis_function <- function(start_date, end_date){
  episodes_with_date %>%
    filter(seen_date >= start_date & seen_date <= end_date,
           Outcome_Category!="Ongoing Pregnancy") %>%
    mutate(
      LOS = as.numeric(difftime(Discharge_Date, Admission_Date, units = "hours"))) %>% 
    ggplot(aes(x=Outcome_Category, y=LOS, fill=Outcome_Category))+
    geom_boxplot()+
    labs(title = "Relationship between Diagnosis and Length of Stay (hrs)",
         x= "Diagnosis",
         y= "Length of Stay (hrs)")+
    theme(legend.position = "none")
  
}
