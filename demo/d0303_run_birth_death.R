#' # Stochastic birth-death difference equation model, with run_simple and run_simulation

library(RPiR)
library(FernandaSanchezRSeries03)
library(stats)

#'Enables the code blocks to see each other, making it easier to plot in different blocks
knitr::opts_knit$set(global.device = TRUE)

#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth.a <- 0.2
human.annual.death.a <- 0.1

# Starting population size
initial.count.a <- 1

# And setting times
start.time.a <- 0
end.time.a <- 100

this.timesteps.a<-1

#' Using run simple function:
#' it's a function that includes the our function inside, as well as the while loop created in 3.2.
#' This makes the function simpler and automates the running of the simulations.
#' We only need to call our function and specify the data frame, end.time and the arguments for our funtion.
#'
population.df.a.1 <- data.frame(count = initial.count.a,
                            time=start.time.a)

final.populations.a.1 <- run_simple(timestep_stochastic_birth_death,
                                population.df.a.1,
                                end.time.a,
                                birth.rate = human.annual.birth.a,
                                death.rate = human.annual.death.a,
                                timestep = this.timesteps.a)

plot_populations(final.populations.a.1)


#' Using run simulation: checks and reports additional arguments.
#'
#' We get the same results as run simple.
#'
#'
population.df.a.2 <- data.frame(count = initial.count.a,
                                time=start.time.a)

final.populations.a.2 <- run_simulation(timestep_stochastic_birth_death,
                                population.df.a.2,
                                end.time.a,
                                birth.rate = human.annual.birth.a,
                                death.rate = human.annual.death.a,
                                timestep = this.timesteps.a)

plot_populations(final.populations.a.2)

#' ## Compare stochastic and deterministic models
#'
#' Set up the simulation parameters
# Set the birth and death rates
human.annual.birth.b <- 0.06
human.annual.death.b <- 0.02

# Starting population size
initial.count.b <- 1

# And setting times
start.time.b <- 0
end.time.b <- 100

this.timesteps.b<-1

#' Using run simple function
population.df.b <- data.frame(count = initial.count.b,
                            time=start.time.b)

final.populations.b <- run_simple(timestep_stochastic_birth_death,
                                population.df.b,
                                end.time.b,
                                birth.rate = human.annual.birth.b,
                                death.rate = human.annual.death.b,
                                timestep = this.timesteps.b)
plot_populations(final.populations.b)

#' # For loop
#'
#'Instead of having to specify the simulation inside of the for loop (as we did in 3.2), we just use run_simple or run_simulation.
#'
population.df.b <- data.frame(count = initial.count.b,
                            time=start.time.b)
first.graph<-TRUE
for (i in 1:10){
  population.df.b <- data.frame(count = initial.count.b,
                              time=start.time.b)
  timesteps.b<-seq(from=start.time.b + this.timesteps.b, to=end.time.b )
  final.populations.b <- run_simple(timestep_stochastic_birth_death,
                                  tail(population.df.b,1),
                                  end.time.b,
                                  birth.rate = human.annual.birth.b,
                                  death.rate = human.annual.death.b,
                                  timestep = this.timesteps.b)
  if (first.graph){
    plot_populations(final.populations.b,
                     new.graph = TRUE, lty=1)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.b,
                     new.graph = FALSE, lty=1)
  }
}

#' # For loop deterministic:
#' for this we need to modify the data frame and not include timestep inside the function
#'

population.df.b.det <- data.frame(count = initial.count.b)

timesteps.b.det<-seq(from=start.time.b + this.timesteps.b, to=end.time.b )

for (new.time in timesteps.b.det) {
  updated.population.b.det <-
    step_deterministic_birth_death(latest = tail(population.df.b.det, 1),
                                   birth.rate = human.annual.birth.b,
                                   death.rate = human.annual.death.b)
  population.df.b.det <- rbind(population.df.b.det, updated.population.b.det)
}

# We need to include time in the data frame given that is not part of the function
population.df.b.det$time <- c(start.time.b, timesteps.b.det)

#' **Plot together**
plot_populations(population.df.b.det, new.graph=FALSE, col=c("red"), lty=1)
legend("topright", legend = c("stochastic", "deterministic"),
       col = c("black", "red"), lty=c(1,1))

#' We can see the variability of the stochastic model vs the deterministic.
#' Given that the deterministic model doesn't take into account probabilities and randomness, the plot will always be the same.
#' But the stochastic model given different outputs according to different scenarios.
#'
#'
#' ## Increase initial population size
#'
#'
#' To be able to plot correctly the increase of initial population size,
#' we need to adjust the y-axis scale to be able to see what happens with larger population sizes.
#' Therefore, we need to set the y axis limit to 100*initial.count,
#' to increase the limit according to the initial count of our choice.
#'
#'
# Set the birth and death rates
human.annual.birth.c <- 0.06
human.annual.death.c <- 0.02

# Starting population size
initial.count.c <- 5

# And setting times
start.time.c <- 0
end.time.c <- 100

this.timesteps.c<-1

#' Using run simple function
population.df.c <- data.frame(count = initial.count.c,
                            time=start.time.c)

final.populations.c <- run_simple(timestep_stochastic_birth_death,
                                population.df.c,
                                end.time.c,
                                birth.rate = human.annual.birth.c,
                                death.rate = human.annual.death.c,
                                timestep = this.timesteps.c)

plot_populations(final.populations.c)

#' # For loop
#'
#'
population.df.c <- data.frame(count = initial.count.c,
                            time=start.time.c)
first.graph<-TRUE
for (i in 1:10){
  population.df.c <- data.frame(count = initial.count.c,
                              time=start.time.c)
  timesteps.c<-seq(from=start.time.c + this.timesteps.c, to=end.time.c )
  final.populations.c <- run_simple(timestep_stochastic_birth_death,
                                  tail(population.df.c,1),
                                  end.time.c,
                                  birth.rate = human.annual.birth.c,
                                  death.rate = human.annual.death.c,
                                  timestep = this.timesteps.c)
  if (first.graph){
    plot_populations(final.populations.c,
                     new.graph = TRUE, lty=1,
                     ylim = c(0, 100 * initial.count.c))
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.c,
                     new.graph = FALSE, lty=1,
                     ylim = c(0, 100 * initial.count.c))
  }
}

#' # For loop deterministic
population.df.c.det <- data.frame(count = initial.count.c)

timesteps.c.det<-seq(from=start.time.c + this.timesteps.c, to=end.time.c )

for (new.time in timesteps.c.det) {
  updated.population.c.det <-
    step_deterministic_birth_death(latest = tail(population.df.c.det, 1),
                                   birth.rate = human.annual.birth.c,
                                   death.rate = human.annual.death.c)
  population.df.c.det <- rbind(population.df.c.det, updated.population.c.det)
}

# Include time in the data frame
population.df.c.det$time <- c(start.time.c, timesteps.c)

#'**Plot together**

plot_populations(population.df.c.det, new.graph=FALSE, col=c("red"), ylim = c(0, 100 * initial.count.c), lty=1)
legend("topright", legend = c("stochastic", "deterministic"),
       col = c("black", "red"), lty=c(1,1))

