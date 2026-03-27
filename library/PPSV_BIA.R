
gdp_2024 <- 23124729853
gdp_growth <- 0.047
nhce_gdp_share <- 0.078
inflation <- 0.027

## health expenditure
health_expenditure <- nhce_gdp_share * gdp_2024 * (1 + gdp_growth) * (1 + inflation)
print(scales::comma_format()(health_expenditure))
print(scales::percent_format(accuracy = 0.01)((base_cea_results_3gdp$cost_vacc[2] / health_expenditure)))