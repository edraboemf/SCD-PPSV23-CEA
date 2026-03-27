#################################################################################################
## GENERAL USE FUNCTIONS
#################################################################################################

##******************************************************************************
## Utilities function
##******************************************************************************
## function to verify, install, and or load packages
# devtools::install_github("DARTH-git/dampack")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

## drop columns
drop.cols <- function(x, drop_vars){
  val <- x[, !(colnames(x) %in% drop_vars)]
  return(val)
}

## keep columns
keep.cols <- function(x, keep_vars){
  val <- x[, (colnames(x) %in% keep_vars)]
  return(val)
}

## function to format numbers
comma2 <- function(x){
  val <- format(round(x, 0), nsmall = 0, big.mark = ",")
  return(val)
}

## Lancet-style number formatter
format_lancet <- function(x, digits = NULL, percent = FALSE) {
  # Replace NA or NaN with a placeholder
  is_missing <- is.na(x) | is.nan(x) | x == 0
  
  # Handle percentages
  if (percent) {
    x <- x * 100
    digits <- ifelse(is.null(digits), 1, digits)
  } else {
    digits <- ifelse(is.null(digits), 0, digits)
  }
    
  # Format numbers
  formatted <- formatC(
    x,
    format = "f",
    big.mark = ",",   
    decimal.mark = "·",
    digits = digits
  )
  
  # Replace missing values with "—"
  formatted[is_missing] <- "—"
  
  # Add percent sign if needed
  if (percent) {
    formatted[!is_missing] <- paste0(formatted[!is_missing], "%")
  }
  
  return(formatted)
}

## Lancet-style formatter for mean ± 95% CI
format_lancet_ci <- function(mean, lower, upper, digits = 0, linebreak = FALSE) {
  mean_f  <- format_lancet(mean, digits)
  lower_f <- format_lancet(lower, digits)
  upper_f <- format_lancet(upper, digits)

  ## Combine into single string: "mean (lower–upper)"
  if(isTRUE(linebreak)){
    ci <- paste0(mean_f, "\n(", lower_f, "–", upper_f, ")")
  }else{
    ci <- paste0(mean_f, " (", lower_f, "–", upper_f, ")")
  }

  for(x in 1:length(mean_f)){
    if(mean_f[x] == "—"){
      ci[x] <- "—"
    } 
  } 
  return(ci)
}


## formatting tables in publication ready format
## https://michaeldismorr.netlify.app/post/publication-ready-tables-with-flextable-and-own-theme-in-r/
source(file.path(ppsv23_library_dir, "PPSV_Customtab_Functions.R"))

##******************************************************************************
## CEA functions
##******************************************************************************

##*********************************************************
## Base Case CEA functions
##*********************************************************
## function to calculate present value
npv_calc <- function(cash_flow = 1, disc_rate = 0.03, time = 0){
  val <- cash_flow/((1 + disc_rate)^(time))
  return(val)
}

## CEA functions
source(file.path(ppsv23_library_dir, "PPSV_CEA_Functions.R"))

##*********************************************************
## Sensitivity Analysis functions
##*********************************************************
## OWSA functions
source(file.path(ppsv23_library_dir, "PPSV_OWSA_Functions.R"))

## PSA functions
source(file.path(ppsv23_library_dir, "PPSV_PSA_Functions.R"))

##*********************************************************
## Scenario Analysis functions
##*********************************************************
## AMC Payment
source(file.path(ppsv23_library_dir, "PPSV_AMC_Functions.R"))

## Buffer Stock
source(file.path(ppsv23_library_dir, "PPSV_Buffer_Stock_Functions.R"))

##******************************************************************************
## BIA functions
##******************************************************************************
## mortgage payment function
mortgage_func <- function(principal, interest, periods){
  if(interest == 0){
    val <- principal/periods
  }else{
    val <- principal*interest*(((1 + interest)^periods)/(((1 + interest)^periods) - 1))
  }
  return(val)
}

#################################################################################################
## END OF MODULE
#################################################################################################