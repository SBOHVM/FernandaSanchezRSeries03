#' # Stochastic birth-death difference equation model

library(RPiR)
library(FernandaSanchezRSeries03)
library(stats)

#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.2
human.annual.death <- 0.1

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

# Timestep
this.timesteps<-1

#' Run the simulation

# Set up the population starting size (at the first timestep)
population.df <- data.frame(count = initial.count,
                            time=start.time)

#' Using a while loop to run the simulation
#' And we want the simulation to end under 2 conditions: when the time runs out or when the dynamics are complete (next count is 0)
#'

latest.df<-population.df
keep.going <- (latest.df$time < end.time)


while (keep.going) {
  data <- timestep_stochastic_birth_death(latest=latest.df,
                                          birth.rate=human.annual.birth,
                                          death.rate=human.annual.death,
                                          timestep=this.timesteps)
  latest.df <- data$updated.pop
  population.df <- rbind(population.df, latest.df)

  # The experiment may end stochastically or the time may run out
  keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

#' And plot the results
plot_populations(population.df)

#' 1. Demonstrate stochasticity
#'
#'
#' To demonstrate stochasticity we need to see variable outputs, therefore we have to include the 'while' loop inside a 'for loop', to plot several plots into one.
#' Outside of the for loop we have to specify that the first graph is true, which will come in handy for the last part of the loop
#' Here we did a sequence of 10, and inside we included the timestep sequence and the initial population data frame.
#' We include the same while loop, with no changes.
#' At the end of the 'for' loop we have to include an 'if'-'else' statement
#' to be able to plot in the same plot.
#' Therefore we state the if the first graph it's the first graph, it's a new plot.
#' If it's not, then it's not a new graph and we plot it on top of the first graph

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:20){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  keep.going <- T
  while (keep.going) {
    data <- timestep_stochastic_birth_death(latest=tail(population.df,1),
                                            birth.rate=human.annual.birth,
                                            death.rate=human.annual.death,
                                            timestep=this.timesteps)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)

    # The experiment may end stochastically or the time may run out
    keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

 if (first.graph){
    plot_populations(population.df,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(population.df,
                     new.graph = FALSE)
  }
   }

#' # Increase variability
#'
#'
#' We will increase birth rate and death rate,
#' as well as the end time to see variability.
#'
#'
#' ## Increase birth rate
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.5
human.annual.death <- 0.1

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:20){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  keep.going <- T
  while (keep.going) {
    data <- timestep_stochastic_birth_death(latest=tail(population.df,1),
                                            birth.rate=human.annual.birth,
                                            death.rate=human.annual.death,
                                            timestep=this.timesteps)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)

    # The experiment may end stochastically or the time may run out
    keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

  if (first.graph){
    plot_populations(population.df,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(population.df,
                     new.graph = FALSE)
  }
}

#' It can be seen that the population growth is much faster if we increase the birth rate to 0.5 and the death stays at 0.1, the curve is more steep.
#' This is because the birth rate is 5 times the death rate, therefore the probability of birth is much higher that death.
#'
#'
#' # Increase death rate and birth.rate
#'
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.5
human.annual.death <- 0.3

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:20){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  keep.going <- T
  while (keep.going) {
    data <- timestep_stochastic_birth_death(latest=tail(population.df,1),
                                            birth.rate=human.annual.birth,
                                            death.rate=human.annual.death,
                                            timestep=this.timesteps)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)

    # The experiment may end stochastically or the time may run out
    keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

  if (first.graph){
    plot_populations(population.df,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(population.df,
                     new.graph = FALSE)
  }
}

#' It can be seen that if we also increase the death rate, the population takes a while longer to grow, the curve is less steep.
#' It is important to mention, if the death rate is higher than the birth rate, the population dies very quickly.
#'
#'
#' # Increase end time
#'
#'

#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.2
human.annual.death <- 0.1

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 200

this.timesteps<-1

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:20){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  keep.going <- T
  while (keep.going) {
    data <- timestep_stochastic_birth_death(latest=tail(population.df,1),
                                            birth.rate=human.annual.birth,
                                            death.rate=human.annual.death,
                                            timestep=this.timesteps)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)

    # The experiment may end stochastically or the time may run out
    keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

  if (first.graph){
    plot_populations(population.df,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(population.df,
                     new.graph = FALSE)
  }
}

#' When we increase the end time to 200 (double as the initial end.time), we can see that the population sizes it reaches are much higher.
#'
#'
#' # Increase timestep
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.2
human.annual.death <- 0.1

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-3

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:20){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  keep.going <- T
  while (keep.going) {
    data <- timestep_stochastic_birth_death(latest=tail(population.df,1),
                                            birth.rate=human.annual.birth,
                                            death.rate=human.annual.death,
                                            timestep=this.timesteps)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)

    # The experiment may end stochastically or the time may run out
    keep.going <- (!data$end.experiment) && (latest.df$time < end.time)
  }

  if (first.graph){
    plot_populations(population.df,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(population.df,
                     new.graph = FALSE)
  }
}

#' When we increase the timestep to 3, the stochasticity varies more.
#' We can see that there are scenarios when the populations grows faster than the other ones in a very noticeable way.
#' Which makes it not a very accurate model, given that the probabilities are very different.
