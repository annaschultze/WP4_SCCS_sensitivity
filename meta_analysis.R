
# SCRIPT INFORMATION 
# ------------------------------------------------
# script: meta_analysis.R
# author: a schultze
# description: meta-analyse results as a post-hoc addition, by design 
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
library(meta)

# set filepath to the data (note manually needs export from secure server)
homepath <- getwd()
filepath <- paste0(homepath,"/Preliminary/for-paper/T3_visualisation.xls")

# FUNCTIONS ---------------------------------------------------------------
# small helper functions to meta-analyse the data 

fit_meta <- function(input) {
  
  data <- input 
  
  metagen(data = data, 
          TE = logrr, 
          lower = loglci, 
          upper = loguci,
          random = T, 
          fixed = F,
          sm = "IRR", 
          overall = TRUE, 
          studlab = country,
          subgroup = design) 
  
} 

draw_meta <- function(input, subgroup) {
  
  data <- input[[subgroup]]
  
  forest.meta(data, 
              overall = F)
}

# 1. DATA CLEANING --------------------------------------------------------
input <- read_excel(filepath)

# clean the variable names 
clean_data <- input %>% 
  mutate(design = case_when(study_design == "scri_pre" ~ 1, 
                            study_design == "scri_post" ~ 2, 
                            study_design == "standard_sccs" ~ 3, 
                            study_design == "extended_sccs" ~4)) %>% 
  mutate(dose = case_when(var == "dose 1 risk window" ~ 1, 
                          var == "dose 2 risk window" ~ 2)) %>% 
  mutate(adjust = case_when(analysis == "unadjusted" ~ 0, 
                            analysis == "adjusted"  ~1)) %>% 
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

# back-calculate the SEs 
clean_data_ma <- clean_data %>% 
  # order the variables 
  select(country, vaccine, adjust, dose, design, everything()) %>% 
  # sort how you want it 
  arrange(vaccine, adjust, dose, design) %>% 
  # log things 
  mutate(logrr = log(irr), 
         loglci = log(lci), 
         loguci = log(uci))


# DOSE 1 - UNADJUSTED  ----------------------------------------------------

# apply filters 
clean_data_ma_un1 <- clean_data_ma %>% 
  filter(adjust == 0) %>% 
  filter(dose == 1)

# fit the meta-analysis 
output.1 <- lapply(split(clean_data_ma_un1, clean_data_ma_un1$vaccine), fit_meta)

# extract output needed 
result_df <- NULL
analysis_strata <- unique(clean_data_ma_un1$vaccine)

for (i in seq_along(analysis_strata)) {

  irr = exp(output.1[[i]]$TE.random.w)
  lci = exp(output.1[[i]]$lower.random.w)
  uci = exp(output.1[[i]]$upper.random.w)
  isquared = (output.1[[i]]$I2.w)
  design = (output.1[[i]]$subgroup.levels)
  
  new_row <- data.frame(vaccine = paste0((names(output.1))[i]), 
                        design = design,
                        irr = irr, 
                        lci = lci, 
                        uci = uci, 
                        isquared = isquared)

  result_df <- rbind(result_df, new_row)
  
} 

# format the output for display in graphs 
result_df <- result_df %>% 
  group_by(vaccine) %>% 
  mutate(vaccine = ifelse(row_number() > 1, "", as.character(vaccine))) %>% 
  ungroup() %>% 
  mutate(study_design = case_when(design == 1 ~ "SCRI pre", 
                            design == 2 ~ "SCRI post", 
                            design == 3 ~ "Standard SCCS", 
                            design == 4 ~"Extended SCCS")) %>% 
  mutate(round_irr = format(round(irr,2), nsmall = 2), 
         round_lci = format(round(lci,2), nsmall = 2), 
         round_uci = format(round(uci,2), nsmall = 2),  
         round_ci = paste0(round_irr," (", round_lci, " - ", round_uci, ")")) %>% 
  mutate(round_I2 = round(isquared*100,2), 
         format_I2 = ifelse(round_I2 == "NA", "", paste0(round_I2, "%")))

# plot the summary estimates 
result_df |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(vaccine, study_design, round_ci, format_I2),
             clip = c(0.1, 10),
             is.summary = FALSE,
             fn.ci_norm = fpDrawDiamondCI,
             zero = 1,
             boxsize = 0.25,
             col = fpColors(
               box = c("darkseagreen4"),
               line = c("darkseagreen3"),
               hrz_lines = ("gray")),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("5" = gpar(lty = 3, 
                                          columns = c(2,3)), 
                               "7" = gpar(lty = 3, 
                                          columns = c(2,3)), 
                               "11" = gpar(lty = 3, 
                                           columns = c(2,3)))) |>
  fp_add_header(vaccine = c("Vaccine"),
                study_design = c("Design"), 
                round_ci = "Summary IRR (95%CI)", 
                format_I2 = "I2") |>
  fp_decorate_graph(graph.pos = 3)


# DOSE 2 - UNADJUSTED  ----------------------------------------------------

# apply filters 
clean_data_ma_un2 <- clean_data_ma %>% 
  filter(adjust == 0) %>% 
  filter(dose == 2)

# fit the meta-analysis 
output.1 <- lapply(split(clean_data_ma_un2, clean_data_ma_un2$vaccine), fit_meta)

# extract output needed 
result_df <- NULL
analysis_strata <- unique(clean_data_ma_un2$vaccine)

for (i in seq_along(analysis_strata)) {
  
  irr = exp(output.1[[i]]$TE.random.w)
  lci = exp(output.1[[i]]$lower.random.w)
  uci = exp(output.1[[i]]$upper.random.w)
  isquared = (output.1[[i]]$I2.w)
  design = (output.1[[i]]$subgroup.levels)
  
  new_row <- data.frame(vaccine = paste0((names(output.1))[i]), 
                        design = design,
                        irr = irr, 
                        lci = lci, 
                        uci = uci, 
                        isquared = isquared)
  
  result_df <- rbind(result_df, new_row)
  
} 

# format the output for display in graphs 
result_df <- result_df %>% 
  group_by(vaccine) %>% 
  mutate(vaccine = ifelse(row_number() > 1, "", as.character(vaccine))) %>% 
  ungroup() %>% 
  mutate(study_design = case_when(design == 1 ~ "SCRI pre", 
                                  design == 2 ~ "SCRI post", 
                                  design == 3 ~ "Standard SCCS", 
                                  design == 4 ~"Extended SCCS")) %>% 
  mutate(round_irr = format(round(irr,2), nsmall = 2), 
         round_lci = format(round(lci,2), nsmall = 2), 
         round_uci = format(round(uci,2), nsmall = 2),  
         round_ci = paste0(round_irr," (", round_lci, " - ", round_uci, ")")) %>% 
  mutate(round_I2 = round(isquared*100,2), 
         format_I2 = ifelse(round_I2 == "NA", "", paste0(round_I2, "%")))

# plot the summary estimates 
result_df |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(vaccine, study_design, round_ci, format_I2),
             clip = c(0.1, 10),
             is.summary = FALSE,
             fn.ci_norm = fpDrawDiamondCI,
             zero = 1,
             boxsize = 0.25,
             col = fpColors(
               box = c("darkseagreen4"),
               line = c("darkseagreen3"),
               hrz_lines = ("gray")),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("5" = gpar(lty = 3, 
                                          columns = c(2,3)), 
                               "9" = gpar(lty = 3, 
                                          columns = c(2,3)))) |>
  fp_add_header(vaccine = c("Vaccine"),
                study_design = c("Design"), 
                round_ci = "Summary IRR (95%CI)", 
                format_I2 = "I2") |>
  fp_decorate_graph(graph.pos = 3)

# DOSE 1 - ADJUSTED  ----------------------------------------------------

# apply filters 
clean_data_ma_ad1 <- clean_data_ma %>% 
  filter(adjust == 1) %>% 
  filter(dose == 1)

# fit the meta-analysis 
output.1 <- lapply(split(clean_data_ma_ad1, clean_data_ma_ad1$vaccine), fit_meta)

# extract output needed 
result_df <- NULL
analysis_strata <- unique(clean_data_ma_ad1$vaccine)

for (i in seq_along(analysis_strata)) {
  
  irr = exp(output.1[[i]]$TE.random.w)
  lci = exp(output.1[[i]]$lower.random.w)
  uci = exp(output.1[[i]]$upper.random.w)
  isquared = (output.1[[i]]$I2.w)
  design = (output.1[[i]]$subgroup.levels)
  
  new_row <- data.frame(vaccine = paste0((names(output.1))[i]), 
                        design = design,
                        irr = irr, 
                        lci = lci, 
                        uci = uci, 
                        isquared = isquared)
  
  result_df <- rbind(result_df, new_row)
  
} 

# format the output for display in graphs 
result_df <- result_df %>% 
  group_by(vaccine) %>% 
  mutate(vaccine = ifelse(row_number() > 1, "", as.character(vaccine))) %>% 
  ungroup() %>% 
  mutate(study_design = case_when(design == 1 ~ "SCRI pre", 
                                  design == 2 ~ "SCRI post", 
                                  design == 3 ~ "Standard SCCS", 
                                  design == 4 ~"Extended SCCS")) %>% 
  mutate(round_irr = format(round(irr,2), nsmall = 2), 
         round_lci = format(round(lci,2), nsmall = 2), 
         round_uci = format(round(uci,2), nsmall = 2),  
         round_ci = paste0(round_irr," (", round_lci, " - ", round_uci, ")")) %>% 
  mutate(round_I2 = round(isquared*100,2), 
         format_I2 = ifelse(round_I2 == "NA", "", paste0(round_I2, "%")))

# plot the summary estimates 
result_df |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(vaccine, study_design, round_ci, format_I2),
             clip = c(0.1, 10),
             is.summary = FALSE,
             fn.ci_norm = fpDrawDiamondCI,
             zero = 1,
             boxsize = 0.25,
             col = fpColors(
               box = c("darkseagreen4"),
               line = c("darkseagreen3"),
               hrz_lines = ("gray")),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(2,3)))) |>
  fp_add_header(vaccine = c("Vaccine"),
                study_design = c("Design"), 
                round_ci = "Summary IRR (95%CI)", 
                format_I2 = "I2") |>
  fp_decorate_graph(graph.pos = 3)


# DOSE 2 - ADJUSTED  ----------------------------------------------------

# apply filters 
clean_data_ma_ad2 <- clean_data_ma %>% 
  filter(adjust == 1) %>% 
  filter(dose == 2)

# fit the meta-analysis 
output.1 <- lapply(split(clean_data_ma_ad2, clean_data_ma_ad2$vaccine), fit_meta)

# extract output needed 
result_df <- NULL
analysis_strata <- unique(clean_data_ma_ad2$vaccine)

for (i in seq_along(analysis_strata)) {
  
  irr = exp(output.1[[i]]$TE.random.w)
  lci = exp(output.1[[i]]$lower.random.w)
  uci = exp(output.1[[i]]$upper.random.w)
  isquared = (output.1[[i]]$I2.w)
  design = (output.1[[i]]$subgroup.levels)
  
  new_row <- data.frame(vaccine = paste0((names(output.1))[i]), 
                        design = design,
                        irr = irr, 
                        lci = lci, 
                        uci = uci, 
                        isquared = isquared)
  
  result_df <- rbind(result_df, new_row)
  
} 

# format the output for display in graphs 
result_df <- result_df %>% 
  group_by(vaccine) %>% 
  mutate(vaccine = ifelse(row_number() > 1, "", as.character(vaccine))) %>% 
  ungroup() %>% 
  mutate(study_design = case_when(design == 1 ~ "SCRI pre", 
                                  design == 2 ~ "SCRI post", 
                                  design == 3 ~ "Standard SCCS", 
                                  design == 4 ~"Extended SCCS")) %>% 
  mutate(round_irr = format(round(irr,2), nsmall = 2), 
         round_lci = format(round(lci,2), nsmall = 2), 
         round_uci = format(round(uci,2), nsmall = 2),  
         round_ci = paste0(round_irr," (", round_lci, " - ", round_uci, ")")) %>% 
  mutate(round_I2 = round(isquared*100,2), 
         format_I2 = ifelse(round_I2 == "NA", "", paste0(round_I2, "%")))

# plot the summary estimates 
result_df |>
  forestplot(mean = irr, 
             lower = lci, 
             upper = uci, 
             labeltext = c(vaccine, study_design, round_ci, format_I2),
             clip = c(0.1, 10),
             is.summary = FALSE,
             fn.ci_norm = fpDrawDiamondCI,
             zero = 1,
             boxsize = 0.25,
             col = fpColors(
               box = c("darkseagreen4"),
               line = c("darkseagreen3"),
               hrz_lines = ("gray")),
             xlog = TRUE,
             xticks=c(-3,0,3),
             xtick.digis = 0,
             txt_gp = fpTxtGp(ticks=gpar(cex=0.6), 
                              label=gpar(cex=0.8)), 
             hrzl_lines = list("6" = gpar(lty = 3, 
                                          columns = c(2,3)))) |>
  fp_add_header(vaccine = c("Vaccine"),
                study_design = c("Design"), 
                round_ci = "Summary IRR (95%CI)", 
                format_I2 = "I2") |>
  fp_decorate_graph(graph.pos = 3)
