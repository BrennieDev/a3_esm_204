# ESM204 Week 6: R tricks
# vthivierge@ucsb.edu
# created: 05/05/2021
# updated: 05/06/2021

## set up environment 

rm(list=ls())
options(scipen=999) # not scientific notation
gc() 

#Packages

packages=c("dplyr","tidyr", "stringr",  
           "ggplot2", "stargazer", "cowplot" )

lapply(1:length(packages), function(x) 
  ifelse((require(packages[x],character.only=TRUE)==FALSE),
         install.packages(packages[x]),
         require(packages[x],character.only=TRUE)))

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd() 

## Load data

demand_data <- read.csv("ESM204_week6_data.csv", stringsAsFactors = F) 

demand_data$price

str(demand_data)
head(demand_data)
tail(demand_data)

## Plot data

plot_1 <- ggplot(data=demand_data, aes(x=quantity,y=price))+
  geom_point()+
  theme_cowplot(16); plot_1

## Estimate linear model (model has an implied intercept)

model_demand <- lm(price ~ quantity, data=demand_data)

a <- model_demand$coefficients[[1]]
b <- model_demand$coefficients[[2]]

## Add estimated curve to our plot

price_fitted <- a + b*demand_data$quantity

plot_1 +
  geom_line(aes(y=price_fitted, x=quantity))

plot_1 +
  geom_smooth(formula = y ~ x, method = "lm", se = F)

## Functions

add_these_numbers <- function(input_1, input_2){
  
  output <- input_1 + input_2 
  
  return(output)
}

add_these_numbers(1,2)

add_these_numbers(1,2) + 4

x <- add_these_numbers(1,2) +4 

#Consumer surplus

##P(Q)
inverse_demand <- function(q, model){
  p <- model$coefficients[[1]] + model$coefficients[[2]]*q
return(p)
}

## Inverse P(Q) to get Q(P)

demand <- function(p, model){
  q <- (p - model$coefficients[[1]])/model$coefficients[[2]]
  return(q)
}

demand(0,model_demand)
inverse_demand(111.4418,model_demand)
inverse_demand(demand(0,model_demand),model_demand)

demand(50,model_demand)
inverse_demand(83.31603,model_demand)

demand(a,model_demand)
inverse_demand(0,model_demand)


##Consumer surplus functions (they are equivalent, either you feed p or q)

CS_p <- function(p, model){
  q <- demand(p, model)
  cs <- 0.5*(model$coefficients[[1]] - p)*q
  return(cs)
}

CS_p(50, model_demand)
CS_p(2, model_demand)
CS_p(a, model_demand)

CS_q <- function(q, model){
  p <- inverse_demand(q, model)
  cs <- 0.5*(model$coefficients[[1]] - p)*q
  return(cs)
}

CS_q(demand(50,model_demand), model_demand)
CS_q(demand(2,model_demand), model_demand)
CS_q(0, model_demand)
