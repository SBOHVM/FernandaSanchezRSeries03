#' # Stochastic birth-death difference equation model

library(RPiR)
library(FernandaSanchezRSeries03)
library(stats)
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate<-0.3
disease.recovery.rate <-0.1

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.susceptibles <- num.cattle-initial.infecteds

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df<- data.frame(time=start.time,
                            susceptibles = initial.susceptibles,
                            infecteds=initial.infecteds)

final.populations <- run_simple(timestep_stochastic_SIS,
                                population.df,
                                end.time,
                                transmission.rate = disease.transmission.rate,
                                recovery.rate = disease.recovery.rate,
                                timestep = this.timesteps)


plot_populations(final.populations)

#' For loop
#'
#'
population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds)

first.graph<-TRUE
for (i in 1:10){
  population.df<- data.frame(time=start.time,
                             susceptibles = initial.susceptibles,
                             infecteds=initial.infecteds)

  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simple(timestep_stochastic_SIS,
                                  tail(population.df,1),
                                  end.time,
                                  transmission.rate = disease.transmission.rate,
                                  recovery.rate = disease.recovery.rate,
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


