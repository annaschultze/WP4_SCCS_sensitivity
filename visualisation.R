
# SCRIPT INFORMATION 
# ------------------------------------------------
# script: visaulisation.R
# author: a schultze
# description: make forestplots of results
#
# dependencies: n/a
#
# outputs: 
#
# ------------------------------------------------
# 0. HOUSEKEEPING --------------------------------

library(tidyverse)
library(readxl)
library(ggplot2)
library(forestplot)
library(janitor)

# 1. DATA CLEANING --------------------------------------------------------
homepath <- getwd()
filepath <- paste0(homepath,"/Preliminary/for-paper/T3_visualisation.xls")

input <- read_excel(filepath)

clean_data <- input %>% 
  mutate(design = case_when(study_design == "scri_pre" ~ 1, 
                            study_design == "scri_post" ~ 2, 
                            study_design == "standard_sccs" ~ 3, 
                            study_design == "extended_sccs" ~4)) %>% 
  mutate(dose = case_when(var == "dose 1 risk window" ~ 1, 
                          var == "dose 2 risk window" ~ 2)) %>% 
  mutate(adjust = case_when(analysis == "unadjusted" ~ 0, 
                                analysis == "adjusted"  ~1)) %>% 
  select(country, vaccine, adjust, dose, design, everything()) %>% 
  arrange(country, vaccine, adjust, dose, design) %>% 
  group_by(country, vaccine, adjust, dose) %>% 
  mutate(scaled_rr = irr / first(irr)) %>% 
  ungroup() %>% 
  mutate(study_design = case_when(study_design == "scri_pre" ~ "pre SCRI", 
                                  study_design == "scri_post" ~ "post SCRI", 
                                  study_design =="standard_sccs" ~ "standard SCCS",
                                  study_design == "extended_sccs" ~ "extended SCCS")) %>% 
  mutate(analysis = case_when(analysis == "unadjusted" ~ "Unadjusted", 
                              analysis == "adjusted" ~ "Adjusted")) %>% 
  mutate(vaccine = case_when(vaccine == "moderna" ~ "Moderna",
                             vaccine == "pfizer" ~ "Pfizer", 
                             vaccine == "astrazeneca" ~ "AstraZeneca", 
                             vaccine == "janssen" ~ "Janssen")) 

  # cannot calculate CI for the scaled RR without the SE from the model 
  # because this is a case-only method, not confident that this is accurately estimated as 1/d 

# 2. PLOT -----------------------------------------------------------------
# Raw plots 

plot_data <- clean_data %>% 
  mutate(rr = round(irr, 2)) %>% 
  arrange(country,adjust, vaccine, dose, design) %>% 
  group_by(country, adjust, vaccine, dose) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(country, adjust, vaccine, dose) %>% 
  mutate(dose = ifelse(row_number() > 1, "", as.character(dose))) %>% 
  ungroup() %>% 
  group_by(country, adjust, vaccine) %>% 
  mutate(vaccine = ifelse(row_number() > 1, "", as.character(vaccine))) %>% 
  ungroup() %>% 
  group_by(country, adjust) %>% 
  mutate(analysis = ifelse(row_number() > 1, "", as.character(analysis))) %>% 
  ungroup() 

# Plot for ARS 
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_ARS <- plot_data %>% 
  filter(country == "ARS")

plot_data_ARS |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(analysis, vaccine, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("4" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "7" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "11" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "15" = gpar(lty=3, 
                                           columns = c(4,5)), 
                               "16" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "17" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "21" = gpar(lty = 3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(analysis = c("Analysis"),
                vaccine = c("Vaccine"),
                study_design = c("Design"), 
                dose = "Dose") 


# Plot for SIDIAP 
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_SIDIAP <- plot_data %>% 
  filter(country == "SIDIAP")

plot_data_SIDIAP |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(analysis, vaccine, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("5" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "8" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "14" = gpar(lty=3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "22" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "26" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "30" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "34" = gpar(lty=3, 
                                           columns = c(4,5)), 
                               "38" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "42" = gpar(lty = 3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(analysis = c("Analysis"),
                vaccine = c("Vaccine"),
                study_design = c("Design"), 
                dose = "Dose") 

# Plot for BIFAP 
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_BIFAP <- plot_data %>% 
  filter(country == "BIFAP-HOSP")

plot_data_BIFAP |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(analysis, vaccine, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "14" = gpar(lty=3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "22" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "26" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "30" = gpar(lty = 3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(analysis = c("Analysis"),
                vaccine = c("Vaccine"),
                study_design = c("Design"), 
                dose = "Dose") 

# Plot for FISABIO 
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_FISABIO <- plot_data %>% 
  filter(country == "FISABIO")

plot_data_FISABIO |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(analysis, vaccine, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("4" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "14" = gpar(lty=3, 
                                           columns = c(4,5)), 
                               "16" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "22" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "26" = gpar(lty = 3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(analysis = c("Analysis"),
                vaccine = c("Vaccine"),
                study_design = c("Design"), 
                dose = "Dose") 

# 3. PLOT BY VACCINE ------------------------------------------------------

# Plot for Pfizer Dose 1  
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_Pfizer1un <- clean_data %>% 
  filter(vaccine == "Pfizer") %>% 
  filter(dose==1) %>% 
  filter(analysis == "Unadjusted") %>% 
  mutate(rr = round(irr, 2)) %>% 
  arrange(vaccine, country, adjust, dose, design) %>% 
  group_by(vaccine, country, adjust, dose) %>% 
  mutate(dose = ifelse(row_number() > 1, "", as.character(dose))) %>% 
  ungroup() %>% 
  group_by(vaccine, country, adjust) %>% 
  mutate(analysis = ifelse(row_number() > 1, "", as.character(analysis))) %>% 
  ungroup() %>% 
  group_by(vaccine, country) %>% 
  mutate(country = ifelse(row_number() > 1, "", as.character(country))) %>% 
  ungroup() 

plot_data_Pfizer1un |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(country, analysis, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "14" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty=3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(country = c("Country"),
                analysis = c("Analysis"),
                study_design = c("Design"), 
                dose = "Dose") 

plot_data_Pfizer1ad <- clean_data %>% 
  filter(vaccine == "Pfizer") %>% 
  filter(dose==1) %>% 
  filter(analysis == "Adjusted") %>% 
  mutate(rr = round(irr, 2)) %>% 
  arrange(vaccine, country, adjust, dose, design) %>% 
  group_by(vaccine, country, adjust, dose) %>% 
  mutate(dose = ifelse(row_number() > 1, "", as.character(dose))) %>% 
  ungroup() %>% 
  group_by(vaccine, country, adjust) %>% 
  mutate(analysis = ifelse(row_number() > 1, "", as.character(analysis))) %>% 
  ungroup() %>% 
  group_by(vaccine, country) %>% 
  mutate(country = ifelse(row_number() > 1, "", as.character(country))) %>% 
  ungroup() 

plot_data_Pfizer1ad |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(country, analysis, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "14" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty=3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(country = c("Country"),
                analysis = c("Analysis"),
                study_design = c("Design"), 
                dose = "Dose") 


# Plot for Pfizer Dose 2 
# some manual fix of axes and lines at the moment, unfortunately 

plot_data_Pfizer2un <- clean_data %>% 
  filter(vaccine == "Pfizer") %>% 
  filter(dose==2) %>% 
  filter(analysis == "Unadjusted") %>% 
  mutate(rr = round(irr, 2)) %>% 
  arrange(vaccine, country, adjust, dose, design) %>% 
  group_by(vaccine, country, adjust, dose) %>% 
  mutate(dose = ifelse(row_number() > 1, "", as.character(dose))) %>% 
  ungroup() %>% 
  group_by(vaccine, country, adjust) %>% 
  mutate(analysis = ifelse(row_number() > 1, "", as.character(analysis))) %>% 
  ungroup() %>% 
  group_by(vaccine, country) %>% 
  mutate(country = ifelse(row_number() > 1, "", as.character(country))) %>% 
  ungroup() 

plot_data_Pfizer2un |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(country, analysis, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "14" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty=3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(country = c("Country"),
                analysis = c("Analysis"),
                study_design = c("Design"), 
                dose = "Dose") 

plot_data_Pfizer2ad <- clean_data %>% 
  filter(vaccine == "Pfizer") %>% 
  filter(dose==2) %>% 
  filter(analysis == "Adjusted") %>% 
  mutate(rr = round(irr, 2)) %>% 
  arrange(vaccine, country, adjust, dose, design) %>% 
  group_by(vaccine, country, adjust, dose) %>% 
  mutate(dose = ifelse(row_number() > 1, "", as.character(dose))) %>% 
  ungroup() %>% 
  group_by(vaccine, country, adjust) %>% 
  mutate(analysis = ifelse(row_number() > 1, "", as.character(analysis))) %>% 
  ungroup() %>% 
  group_by(vaccine, country) %>% 
  mutate(country = ifelse(row_number() > 1, "", as.character(country))) %>% 
  ungroup() 

plot_data_Pfizer2ad |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(country, analysis, dose, study_design),
             clip = c(0.1, 10),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(4,5)), 
                               "10" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "14" = gpar(lty = 3, 
                                           columns = c(4,5)), 
                               "18" = gpar(lty=3, 
                                           columns = c(4,5))),
             boxsize = 0.25, 
             col = fpColors(hrz_lines = "gray")) |>
  fp_set_style(box = "rosybrown4",
               line = "rosybrown3") |> 
  fp_add_header(country = c("Country"),
                analysis = c("Analysis"),
                study_design = c("Design"), 
                dose = "Dose") 




