library(tidyverse)
library(here)
library(janitor)

# Read in HW3 data
data <- read.csv(here("HW3_data.csv")) %>% clean_names()

# Generic demand functions
##P(Q)
fn_demand <- function(q, model){
  p <- model$intercept + model$slope*q
  return(p)
}

## Inverse P(Q) to get Q(P)
fn_inverse_demand <- function(p, model){
  q <- (p - model$intercept)/model$slope
  return(q)
}

# Run linear regression to get demand curves for low and high consumer groups
# low income group
lm_low <- lm(price_dollars ~ q_low_gallons, data=data)
demand_model_low <- list()
demand_model_low['intercept'] <- lm_low$coefficients[[1]]
demand_model_low['slope'] <- lm_low$coefficients[[2]]

# build functions
##P(Q)
demand_low <- function(q){
  p <- demand_model_low$intercept + demand_model_low$slope*q
  return(p)
}

## Inverse P(Q) to get Q(P)
demand_low_inv <- function(p){
  q <- (p - demand_model_low$intercept)/demand_model_low$slope
  return(q)
}

# high income group
lm_high <- lm(price_dollars ~ q_high_gallons, data=data)
demand_model_high <- list()
demand_model_high['intercept'] <- lm_high$coefficients[[1]]
demand_model_high['slope'] <- lm_high$coefficients[[2]]

# build functions
##P(Q)
demand_high <- function(q){
  p <- demand_model_high$intercept + demand_model_high$slope*q
  return(p)
}

## Inverse P(Q) to get Q(P)
demand_high_inv <- function(p){
  q <- (p - demand_model_high$intercept)/demand_model_high$slope
  return(q)
}

# Get horizontal aggregate demand (combined quantity at given price)
demand_agg <- function(p) {
  demand_high_inv(p) + demand_low_inv(p)
}

# Other variables
current_gas_price <- 3.00 # dollars per gallon
scc_co2 <- 51.00 # dollars per metric ton
co2_per_gal <- 19.6 #pounds per gallon
pounds_per_ton <- 2204.62 # pounds per metric ton

# marginal cost of producing a gallon of gasoline is linear and has a price-intercept of 0
# intersects aggregate curve at price of 3
mcp_gas <- function(q) {
  current_gas_price/demand_agg(current_gas_price) * q
}

mcp_gas_inv <- function(p) {
  p * demand_agg(current_gas_price)/current_gas_price
}


####
# 1.
####

mec_per_gal <- scc_co2 * co2_per_gal/pounds_per_ton

## Plot
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0,20) +
  ylim(0,800000) +
  stat_function(fun = demand_high_inv, size = 2) +
  stat_function(fun = demand_low_inv, size = 2) +
  stat_function(fun = demand_agg, size = 2, col = "green") +
  stat_function(fun = mcp_gas_inv, size = 2) +
  geom_vline(xintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = demand_agg(current_gas_price), linetype = "dashed") +
  geom_vline(xintercept = mec_per_gal, linetype = "dashed", col = "red") +
  theme_minimal() +
  labs(y = "quantity", x = "price") + coord_flip()



msc_gas <- function(q) {
  mcp_gas(q) + mec_per_gal
}

msc_gas_inv <- function(p) {
  (p - mec_per_gal)*demand_agg(current_gas_price)/current_gas_price
}



