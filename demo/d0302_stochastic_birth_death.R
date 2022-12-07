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

this.timesteps<-1

#' Run the simulation

## Set up the population starting size (at the first timestep)
population.df <- data.frame(count = initial.count,
                            time=start.time)

next.population <- timestep_stochastic_birth_death(latest = population.df,
                                                birth.rate = human.annual.birth,
                                                death.rate = human.annual.death,
                                                timestep= this.timesteps)
next.population

## while loop
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

population.df

#' And plot the results
plot_populations(population.df)

#' 1. Demonstrate stochasticity

population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:10){
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

#' Increase variability
#

# Increase birth rate
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
for (i in 1:10){
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

#' Increase death rate
#' If the death rate is higher than the birth rate the plots are not normal because the population dies very quickly.
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.6
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
for (i in 1:10){
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


#' Increase timestep
#' We needed to add the if statement in the function
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
for (i in 1:10){
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

