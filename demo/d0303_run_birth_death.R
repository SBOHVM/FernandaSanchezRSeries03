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

#' Using run simple function
population.df <- data.frame(count = initial.count,
                            time=start.time)

final.populations <- run_simple(timestep_stochastic_birth_death,
                                population.df,
                                end.time,
                                birth.rate = human.annual.birth,
                                death.rate = human.annual.death,
                                timestep = this.timesteps)
final.populations
plot_populations(final.populations)


#' Using run simulation: checks and reports additional arguments.
#'
#'
final.populations <- run_simulation(timestep_stochastic_birth_death,
                                population.df,
                                end.time,
                                birth.rate = human.annual.birth,
                                death.rate = human.annual.death,
                                timestep = this.timesteps)
final.populations
plot_populations(final.populations)

#' Compare stochastic and deterministic models
#'
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth <- 0.06
human.annual.death <- 0.02

# Starting population size
initial.count <- 1

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

#' Using run simple function
population.df <- data.frame(count = initial.count,
                            time=start.time)

final.populations <- run_simple(timestep_stochastic_birth_death,
                                population.df,
                                end.time,
                                birth.rate = human.annual.birth,
                                death.rate = human.annual.death,
                                timestep = this.timesteps)
final.populations
plot_populations(final.populations)

#' For loop
#'
#'
population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:10){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simple(timestep_stochastic_birth_death,
                                  tail(population.df,1),
                                  end.time,
                                  birth.rate = human.annual.birth,
                                  death.rate = human.annual.death,
                                  timestep = this.timesteps)
  if (first.graph){
    plot_populations(final.populations,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations,
                     new.graph = FALSE)
  }
}

#'For loop deterministic
population.df <- data.frame(count = initial.count)

timesteps<-seq(from=start.time + this.timesteps, to=end.time )

for (new.time in timesteps) {
  updated.population <-
    step_deterministic_birth_death(latest = tail(population.df, 1),
                                   birth.rate = human.annual.birth,
                                   death.rate = human.annual.death)
  population.df <- rbind(population.df, updated.population)
}

# Plot together
population.df$time <- c(start.time, timesteps)
plot_populations(population.df, new.graph=FALSE, col=c("red"))
legend("topright", legend = c("stochastic", "deterministic"),
       col = c("black", "red"), lty=c(1,1))

#' Increase initial population size
#'
#
# Set the birth and death rates
human.annual.birth <- 0.06
human.annual.death <- 0.02

# Starting population size
initial.count <- 5

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

#' Using run simple function
population.df <- data.frame(count = initial.count,
                            time=start.time)

final.populations <- run_simple(timestep_stochastic_birth_death,
                                population.df,
                                end.time,
                                birth.rate = human.annual.birth,
                                death.rate = human.annual.death,
                                timestep = this.timesteps)
final.populations
plot_populations(final.populations)

#' For loop
#'
#'
population.df <- data.frame(count = initial.count,
                            time=start.time)
first.graph<-TRUE
for (i in 1:10){
  population.df <- data.frame(count = initial.count,
                              time=start.time)
  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simple(timestep_stochastic_birth_death,
                                  tail(population.df,1),
                                  end.time,
                                  birth.rate = human.annual.birth,
                                  death.rate = human.annual.death,
                                  timestep = this.timesteps)
  if (first.graph){
    plot_populations(final.populations,
                     new.graph = TRUE, ylim = c(0, 100 * initial.count))
    first.graph <- FALSE
  } else {
    plot_populations(final.populations,
                     new.graph = FALSE, ylim = c(0, 100 * initial.count))
  }
}

#'For loop deterministic
population.df <- data.frame(count = initial.count)

timesteps<-seq(from=start.time + this.timesteps, to=end.time )

for (new.time in timesteps) {
  updated.population <-
    step_deterministic_birth_death(latest = tail(population.df, 1),
                                   birth.rate = human.annual.birth,
                                   death.rate = human.annual.death)
  population.df <- rbind(population.df, updated.population)
}

# Plot together
population.df$time <- c(start.time, timesteps)
plot_populations(population.df, new.graph=FALSE, col=c("red"), ylim = c(0, 100 * initial.count))
legend("topright", legend = c("stochastic", "deterministic"),
       col = c("black", "red"), lty=c(1,1))
