
## Hypothetical Harm Aversion Task - Computational Model

## Contreras-Huerta et al., 2022
# Prosocial behavior is associated wit transdiagnostic
# markers of affective sensitivity in multiple domains

# Diego Garrido Cerpa - Viña del mar


# Load required libraries
library(ggplot2)
library(tidyr)


# Calculate Delta function
# Delta_value = c(, diff(data$Value))
# Delta Method (Car Package)
# delta_result = deltaMethod(harm_aversion)

#  Define Hypothetical harm aversion task model
harm_aversion = function(k)
{
  # Delta functionss
  m = deltaMethod(data$Reward) #Money
  s = deltaMethod(data$shocks) #Shocs
  
  # Formula of the model
  result = (1-k)*m - (k*s)
  
  # Print result
  return(result)
}
  

# Define Softmax function
softmax = function(v, beta)
{
  # Exponential e
  exp_value = exp^(-beta*v)
  
  # Model Formula
  probabilities = 1/1 + exp_value
  
  # Print result
  return(probabilities)
}


# Define Prosocial effort Task models
prosocial_task = function(reward, effort, lambda)
{
  # Formula
  result = reward - lambda*(effort^2)
  
  # Print result
  return(result)
  
}

# Define Softmax function Prosocial effort task
softmax_prosocial = function(sv, beta, i, t)
{
  # e values
  exp_value1 = exp(beta)
  exp_value2 = exp(beta * sv(i) * sv(t))
  
  # Formula
  probabilities = exp_value1 * sv(i) / exp_value1 + exp_value1

  # Result
  return(probabilities)
}









