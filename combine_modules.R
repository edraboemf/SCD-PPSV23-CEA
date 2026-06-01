
home_dir <- "/Users/emmanuel/Dropbox/vaccines/VBC - SCD/PPSV23 model/PPSV23 CEM Thesis/Value in Health/Submission Materials/RR2"
appendix_dir <- file.path(home_dir, "Submission/Appendix")
model_dir <- file.path(home_dir, "model")
library_dir <- file.path(model_dir, "library")

r_files <- c(
  file.path(model_dir, "Readme.R"),
  file.path(model_dir, "PPSV23_Model.R"),
  file.path(library_dir, "PPSV_Settings.R"),
  file.path(library_dir, "PPSV_Functions.R"),
  file.path(library_dir, "PPSV_Customtab_Functions.R"),
  file.path(library_dir, "PPSV_CEA_Functions.R"),
  file.path(library_dir, "PPSV_OWSA_Functions.R"),
  file.path(library_dir, "PPSV_PSA_Functions.R"),
  file.path(library_dir, "PPSV_AMC_Functions.R"),
  file.path(library_dir, "PPSV_Buffer_Stock_Functions.R"),
  file.path(library_dir, "PPSV_Tables_Functions.R"),
  file.path(library_dir, "PPSV_Map.R"),
  file.path(library_dir, "PPSV_Flow_Diag.R"),
  file.path(library_dir, "PPSV_Data.R"),
  file.path(library_dir, "PPSV_Base_Case_CEA.R"),
  file.path(library_dir, "PPSV_OWSA.R"),
  file.path(library_dir, "PPSV_PSA.R"),
  file.path(library_dir, "PPSV_AMC_Scenario_Analyses.R"),
  file.path(library_dir, "PPSV_Buffer_Stock_Scenario_Analyses.R"),
  file.path(library_dir, "PPSV_Tables.R")
)

output_file <- file.path(appendix_dir, "Appendix C - CEA of PPSV23 in Children Under 5 Years With SCD in Burkina Faso - R implementation.R")

file.create(output_file)

for (f in r_files) {
  cat("\n\n# ----", basename(f), "----\n\n", file = output_file, append = TRUE)
  cat(readLines(f), sep = "\n", file = output_file, append = TRUE)
}