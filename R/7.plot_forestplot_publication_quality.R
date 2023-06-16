

# ---------------------------------------------------------
# SETUP FOLDER STRUCTURE
# ---------------------------------------------------------
code_dir <- file.path("R/mortality")
data_dir <- file.path("data/mortality")
plot_dir <- file.path("figures_tables/mortality")

source(file.path("R/helper_functions", "create_forest_plot_functions_lancet.R"))

# ---------------------------------------------------------
# LOAD MODEL RESULTS
# ---------------------------------------------------------
# Load model estimates from this file
model_results <- readRDS(file.path(data_dir, "model_fits_mortality.Rds"))

data <- model_results$data[[1]]
data_time <- model_results$data[[3]]
data_PCR <- model_results$data[[4]]

# ---------------------------------------------------------
# DEATH RISK - CREATE DATA FRAME
# ---------------------------------------------------------
mortality_risk.fit <- model_results %>%
  filter(model_formula == "Death ~ vax_status + Sex + RACF_resident + irsd_group + hosp_pres_category + received_treatment") %>%
  filter(model_no == 1) %>%
  select(fit) %>%
  .$fit

plot_layout_mortality <- layout <- c(
  area(t = 0, l = 1, b = 30, r = 90),
  area(t = 0, l = 91, b = 30, r = 170),
  area(t = 0, l = 171, b = 30, r = 220),
  area(t = 1, l = 221, b = 30, r = 600)
)

mortality_risk.plot <- create_forest_plot_lancet(data = mortality_risk.fit[[1]],
                                          levels =  c("vax_status1-2 doses","vax_status3 doses", "vax_status4 doses",
                                                      "SexFemale", "SexMale",
                                                      "RACF_residentnot in RACF", "RACF_residentRACF resident",
                                                      "irsd_group1-3", "irsd_group4-7", "irsd_group8-10",
                                                      "hosp_pres_category<2 Hosp between 2018-2020", "hosp_pres_category2+ Hosp between 2018-2020",
                                                      "received_treatmentNo treatment", "received_treatmentReceived treatment"
                                          ),
                                          labels =  c("1-2 doses","3 doses", "4 doses",
                                                      "Female", "Male",
                                                      "Non-RACF Resident", "RACF Resident",
                                                      "SEIFA deciles 1-3", "SEIFA deciles 4-7", "SEIFA deciles 8-10",
                                                      "<2 Hospitalisations", "2+ Hospitalisations",
                                                      "No treatment", "Treatment"
                                          ),
                                          plot_title = "",
                                          plot_subtitle = "",
                                          plot_layout = plot_layout_mortality
)


# ---------------------------------------------------------
# CREATE PLOT FOR DEATH RISK
# ---------------------------------------------------------
mae_save_local(mortality_risk.plot,
               source_name = "",
               save_file_path = file.path(plot_dir, "mortality_risk_lancet.png"),
               logo_image_path = file.path("figures_tables/no_logo.png"))


# ---------------------------------------------------------
# DEATH - DRUG TYPE
# ---------------------------------------------------------
death_drug.fit <- model_results %>%
  filter(model_formula == "Death ~ vax_status + Sex + irsd_group + RACF_resident + hosp_pres_category + drug") %>%
  filter(model_no == 2) %>%
  select(fit) %>%
  .$fit

plot_layout_mortality_drug <- layout <- c(
  area(t = 0, l = 1, b = 30, r = 90),
  area(t = 0, l = 91, b = 30, r = 170),
  area(t = 0, l = 171, b = 30, r = 220),
  area(t = 1, l = 221, b = 30, r = 600)
)


mortality_risk_drug.plot <- create_forest_plot_lancet(data = death_drug.fit[[1]],
                                               levels =  c("vax_status1-2 doses","vax_status3 doses", "vax_status4 doses",
                                                           "SexFemale", "SexMale",
                                                           "RACF_residentnot in RACF", "RACF_residentRACF resident",
                                                           "irsd_group1-3", "irsd_group4-7", "irsd_group8-10",
                                                           "hosp_pres_category<2 Hosp between 2018-2020", "hosp_pres_category2+ Hosp between 2018-2020",
                                                           "drugNo treatment", "drugPaxlovid", "drugLagevrio"
                                               ),
                                               labels =  c("1-2 doses","3 doses", "4 doses",
                                                           "Female", "Male",
                                                           "Non-RACF Resident", "RACF Resident",
                                                           "SEIFA deciles 1-3", "SEIFA deciles 4-7", "SEIFA deciles 8-10",
                                                           "<2 Hospitalisations", "2+ Hospitalisations",
                                                           "No treatment",  "Nirmatrelvir-ritonavir", "Molnupiravir"
                                               ),
                                               plot_title = "",
                                               plot_subtitle = "",
                                               plot_layout = plot_layout_mortality_drug

)



mortality_risk_drug.plot
mae_save_local(mortality_risk_drug.plot,
               source = "",
               save_file_path = file.path(plot_dir, "mortality_risk_drug_lancet.png"),
               logo_image_path = file.path("figures_tables/no_logo.png"))


# ---------------------------------------------------------
# DEATH RISK - CREATE DATA FRAME
# ---------------------------------------------------------
mortality_risk_time.fit <- model_results %>%
  filter(model_formula == "Death ~ vax_status + Sex + irsd_group + RACF_resident + hosp_pres_category + diag_to_treat.category") %>%
  select(fit) %>%
  .$fit


mortality_risk_time.plot <- create_forest_plot_lancet(data = mortality_risk_time.fit[[1]],
                                               levels =  c("vax_status1-2 doses","vax_status3 doses", "vax_status4 doses",
                                                           "SexFemale", "SexMale",
                                                           "RACF_residentnot in RACF", "RACF_residentRACF resident",
                                                           "irsd_group1-3", "irsd_group4-7", "irsd_group8-10",
                                                           "hosp_pres_category<2 Hosp between 2018-2020", "hosp_pres_category2+ Hosp between 2018-2020",
                                                           "diag_to_treat.categoryNo treatment","diag_to_treat.category0-1 days",
                                                           "diag_to_treat.category2-3 days", "diag_to_treat.category4+ days"
                                               ),
                                               labels =  c("1-2 doses","3 doses", "4 doses",
                                                           "Female", "Male",
                                                           "Non-RACF Resident", "RACF Resident",
                                                           "SEIFA deciles 1-3", "SEIFA deciles 4-7", "SEIFA deciles 8-10",
                                                           "<2 Hospitalisations", "2+ Hospitalisations",
                                                           "No treatment","0-1 days","2-3 days", "4+ days"
                                               ),  plot_title = "",
                                               plot_subtitle = "",
                                               plot_layout = plot_layout_mortality)


mortality_risk_time.plot

mae_save_local(mortality_risk_time.plot,
               source = "",
               save_file_path = file.path(plot_dir, "mortality_risk_time_lancet.png"),
               logo_image_path = file.path("figures_tables/no_logo.png"))


