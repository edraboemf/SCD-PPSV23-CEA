#################################################################################################
## DATA
#################################################################################################

##******************************************************************************
## CPI data
##******************************************************************************
cpi_params <- readxl::read_xlsx(
  path = ppsv23_params_path, 
  sheet = "cpi", 
  range = "A4:BQ270"
)

omit_pars <- c(
  "Country Name", 
  "Country Code", 
  "Indicator Name", 
  "Indicator Code"
)

bfa_cpi <- cpi_params %>%
  dplyr::filter(`Country Code` == "BFA") %>%
  dplyr::select(-all_of(omit_pars))

##******************************************************************************
## GDP per capita
##******************************************************************************
## gdp per capita
gdp_params <- readxl::read_xlsx(
  path = ppsv23_params_path, 
  sheet = "gdp_per_capita", 
  range = "A4:BQ270"
)
bfa_gdp <- gdp_params %>%
  dplyr::filter(`Country Code` == "BFA") %>%
  dplyr::select(!(c("Country Name", "Country Code", "Indicator Name", "Indicator Code")))

##******************************************************************************
## Read parameters from the Excel file
##******************************************************************************
temp_ppsv23_params <- readxl::read_xlsx(
  path = ppsv23_params_path, 
  sheet = "parameters"
)

ppsv23_params <- temp_ppsv23_params %>%
  filter(!is.na(Parameter))

## Convert parameters to a named vector
ppsv23_params_label <- ppsv23_params$Label
names(ppsv23_params_label) <- ppsv23_params$Parameter

ppsv23_params_curr <- ppsv23_params$Currency
names(ppsv23_params_curr) <- ppsv23_params$Parameter

ppsv23_params_curryear <- ppsv23_params$Year
names(ppsv23_params_curryear) <- ppsv23_params$Parameter

ppsv23_params_include <- ppsv23_params$Include
names(ppsv23_params_include) <- ppsv23_params$Parameter

ppsv23_params_value <- as.numeric(ppsv23_params$Value)
names(ppsv23_params_value) <- ppsv23_params$Parameter

ppsv23_params_lower <- as.numeric(ppsv23_params$Lower)
names(ppsv23_params_lower) <- ppsv23_params$Parameter

ppsv23_params_upper <- as.numeric(ppsv23_params$Upper)
names(ppsv23_params_upper) <- ppsv23_params$Parameter

ppsv23_params_dist <- ppsv23_params$Distribution
names(ppsv23_params_dist) <- ppsv23_params$Parameter

## cpi adjustment factor
ppsv23_params_cpi <- ppsv23_params_curryear
for(x in na.omit(names(ppsv23_params_cpi))) {
  value <- ppsv23_params_cpi[x]
  if(as.character(value) != "NA") {
    ppsv23_params_cpi[x] <- as.vector(bfa_cpi[as.character(value)])
    ppsv23_params_cpi[x] <- (as.numeric(bfa_cpi[as.character(ref_year - 1)]) * (1 + as.numeric(ppsv23_params_value["inflation_rate"]))) / as.numeric(ppsv23_params_cpi[x])
  }
  else {
    ppsv23_params_cpi[x] <- 1
  }
}
ppsv23_params_cpi <- as.numeric(ppsv23_params_cpi)
names(ppsv23_params_cpi) <- ppsv23_params$Parameter

##******************************************************************************
## Estimates for the text
##******************************************************************************
w_med_vsly_value <- round(
  Hmisc::wtd.quantile(
    x = c(6229.00, 12457.00), 
    weights = c(736.00, 714.00), 
    probs = 0.5
  ) / 1862.00, 
  2
)

w_med_vsly_low <- round(
  Hmisc::wtd.quantile(
    x = c(3716.00, 527.00), 
    weights = c(736.00, 714.00), 
    probs = 0.5
  ) / 1862.00, 
  2
)

w_med_vsly_high <- round(
  Hmisc::wtd.quantile(
    x = c(8742.00, 24387.00), 
    weights = c(736.00, 714.00), 
    probs = 0.5
  ) / 1862.00, 
  2
)

#################################################################################################
## END OF MODULE
#################################################################################################