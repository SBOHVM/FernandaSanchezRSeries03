#' ---
#' title: "Simple birth-death difference equation model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' We are going to evaluate the population growth with a birth rate of 0.2 and death rate of 0.1.
#'
#' 1. Birth-death model: where $\beta$ is the birth rate and $\lambda$ the death rate
#'
#'    $$N(t + 1) = N(t)+ (N(t)\times\beta) - (N(t)\times\lambda)$$
#'
#'

library(RPiR)
library(FernandaSanchezRSeries03)

#' Set up the simulation parameters
# Set the birth and death rates
birth.rate <- 0.2
death.rate <- 0.1

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

#' Run the simulation

## Set up the population starting size (at the first timestep)
population.df <- data.frame(count = initial.count)

## the timesteps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time)

## Now we loop through the time itself (starting at the second timestep)
for (new.time in timesteps) {
  updated.population <-
    step_deterministic_birth_death(latest = tail(population.df, 1),
                                   birth.rate = birth.rate,
                                   death.rate = death.rate)
  population.df <- rbind(population.df, updated.population)
}

#' And plot the results
population.df$time <- c(start.time, timesteps)
plot_populations(population.df)
