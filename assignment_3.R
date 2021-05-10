library(tidyverse)
library(here)
library(janitor)
library(broom)

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

status_quo_quantity <- mcp_gas_inv(3)
status_quo_price <- current_gas_price

# Test plot
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0,800000) +
  ylim(0,16) +
  stat_function(fun = demand_high, size = 2) +
  stat_function(fun = demand_low, size = 2) +
  stat_function(fun = mcp_gas, size = 2) +
  stat_function(fun = msc_gas, size = 1, col = "red") +
  geom_hline(yintercept = mec_per_gal, linetype = "dashed", col = "red") +
  theme_minimal() +
  labs(x = "quantity", y = "price") +
  geom_segment(aes(x = 0, xend = uniroot(function(x)  demand_high(x) - demand_model_low$intercept  , c(0,600000), tol=1e-8)$root, 
                   y = demand_model_high$intercept, yend = demand_model_low$intercept), size = 2, col = "green") +
  geom_segment(aes(x = demand_high_inv(demand_model_low$intercept), xend = demand_agg(0), 
                   y = demand_model_low$intercept, yend = 0), size = 2, col = "green")

####
# 2. 
####
# Consumer surplus of low income
cs_low <- (demand_model_low$intercept - current_gas_price) * demand_low_inv(3) * 0.5
# Consumer surplus of high income
cs_high <- (demand_model_high$intercept - current_gas_price) * demand_high_inv(3) * 0.5

# Total consumer surplus
cs_total <- cs_low + cs_high

# Total producer surplus
ps_total <- current_gas_price*status_quo_quantity * 0.5

# Environmental damage
env_cost <- mec_per_gal * status_quo_quantity

####
# 3.
####
# Proportion of total benefit to low income
cs_low_pct <- cs_low/cs_total * 100
# Proportion of total benefit to high income
cs_high_pct <- cs_high/cs_total * 100

####
# 4.
####
optimal_tax <- mec_per_gal
optimal_price <- uniroot(function(x)  demand_agg(x) - msc_gas_inv(x)  , c(0,600000), tol=1e-8)$root
optimal_quantity <-  msc_gas_inv(optimal_price)

optimal_cs_high <- (demand_model_high$intercept-optimal_price) * demand_high_inv(optimal_price) * 0.5
optimal_cs_low <- (demand_model_low$intercept-optimal_price) * demand_low_inv(optimal_price) * 0.5

optimal_ps_total <- mcp_gas(optimal_quantity) * optimal_quantity * 0.5

optimal_env_cost <- mec_per_gal * optimal_quantity
tax_rev <- (optimal_price - mcp_gas(optimal_quantity)) * optimal_quantity

# Since low income group bears all cost of environmental damages, we subtract it from their welfare ot get adjusted value
optimal_cs_low_adjusted <- optimal_cs_low - optimal_env_cost

####
# 5.
####
tax_proportion_low <- demand_low_inv(3)/status_quo_quantity
tax_proportion_high <- demand_high_inv(3)/status_quo_quantity

# Function 
effects_of_scc <- function(scc) {
  mec <- scc * co2_per_gal/pounds_per_ton
  
  msc_inv <- function(p) {
    (p - mec)*demand_agg(current_gas_price)/current_gas_price
  }
  
  new_price <- uniroot(function(x)  demand_agg(x) - msc_inv(x)  , c(0,800000), tol=1e-8)$root
  new_quantity <-  msc_inv(new_price)
  
  new_cs_high <- (demand_model_high$intercept-new_price) * demand_high_inv(new_price) * 0.5
  new_cs_low <- (demand_model_low$intercept-new_price) * demand_low_inv(new_price) * 0.5
  new_ps <- mcp_gas(new_quantity) * new_quantity * 0.5
  
  new_tax_gen <- (new_price - mcp_gas(new_quantity)) * new_quantity
  new_env_cost <- mec * new_quantity
  cs_low_total <- new_cs_low - new_env_cost + (tax_proportion_low * new_tax_gen)
  cs_high_total <- new_cs_high + (tax_proportion_high * new_tax_gen)
    
    
  out <- list()
  out$scc <- scc
  out$low_income_welfare <- cs_low_total
  out$high_income_welfare <- cs_high_total
  out$producer_surplus <- new_ps
  return(out)
}

# Run scenario with different scc values
# $51, $75, $100, $125, and $150 per metric ton of CO2
nl <- list()
nl[[1]] <- effects_of_scc(51)
nl[[2]] <- effects_of_scc(75)
nl[[3]] <- effects_of_scc(100)
nl[[4]] <- effects_of_scc(125)
nl[[5]] <- effects_of_scc(150)

out_table <- data.frame(matrix(unlist(nl), nrow=length(nl), byrow=TRUE))
colnames(out_table) <- c("scc ($ per metric ton CO2)", "low-income welfare ($)", "high-income welfare ($)", "producer surplus ($)")

write_csv(out_table, here("question5.csv"))

####
# 6.
####
##P(Q)
new_demand_high <- function(q){
  p <- demand_high(q)*0.5
  return(p)
}

## Inverse P(Q) to get Q(P)
new_demand_high_inv <- function(p){
  q <- ((2*p - demand_model_high$intercept)/demand_model_high$slope)
  return(q)
}

new_demand_agg <- function(p) {
  new_demand_high_inv(p) + demand_low_inv(p)
}

new_price <- uniroot(function(x)  new_demand_agg(x) - mcp_gas_inv(x)  , c(0,800000), tol=1e-8)$root
new_quantity <-  mcp_gas_inv(new_price)
new_env_cost <- new_quantity * mec_per_gal

desired_price_after_tax <- uniroot(function(x)  demand_agg(x) - new_quantity  , c(0,800000), tol=1e-8)$root
new_tax <- desired_price_after_tax-new_price

mcp_w_tax <- function(q) {
  mcp_gas(q) + new_tax
}

mcp_w_tax_inv <- function(p) {
  mcp_gas_inv(p-new_tax)
}

# plot
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0,20) +
  ylim(0,800000) +
  stat_function(fun = new_demand_high_inv, size = 2) +
  stat_function(fun = demand_low_inv, size = 2) +
  stat_function(fun = demand_agg, size = 1, col = "green") +
  stat_function(fun = new_demand_agg, size = 2, col = "green") +
  stat_function(fun = mcp_gas_inv, size = 2) +
  stat_function(fun = mcp_w_tax_inv, size = 1, color = "red") +
  #stat_function(fun = msc_gas_inv, size = 1, color= "red") +
  geom_vline(xintercept = new_price, linetype = "dashed") +
  geom_hline(yintercept = new_quantity, linetype = "dashed") +
  geom_vline(xintercept = mec_per_gal, linetype = "dashed", col = "red") +
  theme_minimal() +
  labs(y = "quantity", x = "price") + coord_flip()


# Test plot
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  xlim(0,800000) +
  ylim(0,20) +
  stat_function(fun = demand_high, size = 2) +
  stat_function(fun = new_demand_high, size = 2, col = "blue") +
  stat_function(fun = demand_low, size = 2) +
  stat_function(fun = mcp_gas, size = 2) +
  stat_function(fun = msc_gas, size = 1, col = "red") +
  geom_hline(yintercept = mec_per_gal, linetype = "dashed", col = "red") +
  theme_minimal() +
  labs(y = "quantity", x = "price") +
  geom_segment(aes(x = 0, xend = uniroot(function(x)  demand_high(x) - demand_model_low$intercept  , c(0,600000), tol=1e-8)$root, 
                   y = demand_model_high$intercept, yend = demand_model_low$intercept), size = 2, col = "green") +
  geom_segment(aes(x = demand_high_inv(demand_model_low$intercept), xend = demand_agg(0), 
                   y = demand_model_low$intercept, yend = 0), size = 2, col = "green") +
  geom_point(aes(x = optimal_quantity, y = optimal_price), size = 3)



