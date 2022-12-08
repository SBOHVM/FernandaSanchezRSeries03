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
initial.recovereds<-0
initial.susceptibles <- num.cattle-initial.infecteds-initial.recovereds

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds=initial.recovereds)

final.populations <- run_simulation(timestep_stochastic_SIR,
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
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)

first.graph<-TRUE
for (i in 1:10){
  population.df<- data.frame(time=start.time,
                             susceptibles = initial.susceptibles,
                             infecteds=initial.infecteds,
                             recovereds=initial.recovereds)

  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simulation(timestep_stochastic_SIR,
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

#Deterministic SIR
population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)


final.populations <- run_simulation(timestep_deterministic_SIR,
                                    population.df,
                                    end.time,
                                    transmission.rate = disease.transmission.rate,
                                    recovery.rate = disease.recovery.rate,
                                    timestep = this.timesteps)



plot_populations(final.populations, new.graph=FALSE, col=c("orange", "blue", "purple"))
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))


#' Compare stochastic and deterministic models
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate<-0.2
disease.recovery.rate <-0.1

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.recovereds<-0
initial.susceptibles <- num.cattle-initial.infecteds-initial.recovereds

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds=initial.recovereds)

final.populations <- run_simulation(timestep_stochastic_SIR,
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
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)

first.graph<-TRUE
for (i in 1:10){
  population.df<- data.frame(time=start.time,
                             susceptibles = initial.susceptibles,
                             infecteds=initial.infecteds,
                             recovereds=initial.recovereds)

  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simulation(timestep_stochastic_SIR,
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

#Deterministic SIR
population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)


final.populations <- run_simulation(timestep_deterministic_SIR,
                                    population.df,
                                    end.time,
                                    transmission.rate = disease.transmission.rate,
                                    recovery.rate = disease.recovery.rate,
                                    timestep = this.timesteps)



plot_populations(final.populations, new.graph=FALSE, col=c("orange", "blue", "purple"))
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' Vary population parameters
#'
#'

#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate<-0.3
disease.recovery.rate <-0.1

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.recovereds<-0
initial.susceptibles <- num.cattle-initial.infecteds-initial.recovereds

# And setting times
start.time <- 0
end.time <- 500

this.timesteps<-1

population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds=initial.recovereds)

final.populations <- run_simulation(timestep_stochastic_SIR,
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
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)

first.graph<-TRUE
for (i in 1:10){
  population.df<- data.frame(time=start.time,
                             susceptibles = initial.susceptibles,
                             infecteds=initial.infecteds,
                             recovereds=initial.recovereds)

  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simulation(timestep_stochastic_SIR,
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

#Deterministic SIR
population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)


final.populations <- run_simulation(timestep_deterministic_SIR,
                                    population.df,
                                    end.time,
                                    transmission.rate = disease.transmission.rate,
                                    recovery.rate = disease.recovery.rate,
                                    timestep = this.timesteps)



plot_populations(final.populations, new.graph=FALSE, col=c("orange", "blue", "purple"))
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' Vary transmission rate
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate<-0.8
disease.recovery.rate <-0.1

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.recovereds<-0
initial.susceptibles <- num.cattle-initial.infecteds-initial.recovereds

# And setting times
start.time <- 0
end.time <- 100

this.timesteps<-1

population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds=initial.recovereds)

final.populations <- run_simulation(timestep_stochastic_SIR,
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
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)

first.graph<-TRUE
for (i in 1:10){
  population.df<- data.frame(time=start.time,
                             susceptibles = initial.susceptibles,
                             infecteds=initial.infecteds,
                             recovereds=initial.recovereds)

  timesteps<-seq(from=start.time + this.timesteps, to=end.time )
  final.populations <- run_simulation(timestep_stochastic_SIR,
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

#Deterministic SIR
population.df<- data.frame(time=start.time,
                           susceptibles = initial.susceptibles,
                           infecteds=initial.infecteds,
                           recovereds= initial.recovereds)


final.populations <- run_simulation(timestep_deterministic_SIR,
                                    population.df,
                                    end.time,
                                    transmission.rate = disease.transmission.rate,
                                    recovery.rate = disease.recovery.rate,
                                    timestep = this.timesteps)



plot_populations(final.populations, new.graph=FALSE, col=c("orange", "blue", "purple"))
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

