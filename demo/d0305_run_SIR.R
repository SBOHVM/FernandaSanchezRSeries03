#' ---
#' title: "Stochastic Susceptible-Infected-Recovered (SIR) model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

library(RPiR)
library(FernandaSanchezRSeries03)
library(stats)

#' We are going to compare the stochastic and deterministic Susceptible-Infected-Recovered (SIR) model.
#'
#'
#'SIR Model
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-rbinom(1,I(t),\beta)$$
#'
#' 2. Infected model
#'
#'    $$I(t + 1) = S(t)+rbinom(1,I(t),\beta)- rbinom(1, I(t),\sigma)$$
#'
#' 3. Recovered model
#'
#'    $$R(t + 1) = R(t)+ rbinom(1, I(t),\sigma)$$
#'
#' 4. N is a constant for total population
#'
#'    $$N = S(t)+ I(t) + R(t)$$
#'
#'


#' # Compare stochastic and deterministic models
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.a<-0.2
disease.recovery.rate.a <-0.1

# Starting population size

num.cattle.a<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.a <- 2
initial.recovereds.a<-0
initial.susceptibles.a <- num.cattle.a-initial.infecteds.a-initial.recovereds.a

# And setting times
start.time.a <- 0
end.time.a <- 100

# Timestep of our choice
this.timesteps.a<-1


#' ## Stochastic SIR (Scenario A)
#'
#'

# Our data frame
population.df.a<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a,
                           recovereds=initial.recovereds.a)

#' Run simulation once
final.populations.a <- run_simulation(timestep_stochastic_SIR,
                                    population.df.a,
                                    end.time.a,
                                    transmission.rate = disease.transmission.rate.a,
                                    recovery.rate = disease.recovery.rate.a,
                                    timestep = this.timesteps.a)


plot_populations(final.populations.a)

#' For loop
#'
#'
population.df.a<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a,
                           recovereds= initial.recovereds.a)

first.graph<-TRUE
for (i in 1:20){
  population.df.a<- data.frame(time=start.time.a,
                             susceptibles = initial.susceptibles.a,
                             infecteds=initial.infecteds.a,
                             recovereds=initial.recovereds.a)

  timesteps.a<-seq(from=start.time.a + this.timesteps.a, to=end.time.a )
  final.populations.a <- run_simulation(timestep_stochastic_SIR,
                                      tail(population.df.a,1),
                                      end.time.a,
                                      transmission.rate = disease.transmission.rate.a,
                                      recovery.rate = disease.recovery.rate.a,
                                      timestep = this.timesteps.a)
  if (first.graph){
    plot_populations(final.populations.a,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.a,
                     new.graph = FALSE)
  }
}

plt <- recordPlot()


#' ## Deterministic SIR (Scenario A)
population.df.a.det<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a,
                           recovereds= initial.recovereds.a)


final.populations.a.det <- run_simulation(timestep_deterministic_SIR,
                                    population.df.a.det,
                                    end.time.a,
                                    transmission.rate = disease.transmission.rate.a,
                                    recovery.rate = disease.recovery.rate.a,
                                    timestep = this.timesteps.a)

#' **Plot together**
#'
#' We can see a lot of variability with the stochastic model in comparison with the deterministic.
#' Given that the transmission rate is greater than the recovery rate, we see an outbreak at the beginning,
#' but once the individuals recover, the disease dies out.
#' This is shown in the stochastic and deterministic model.
#' The difference with the stochastic model is that it shows you different scenarios
#' regarding how many individuals will get infected and when the outbreak might happen.
#'
#'
replayPlot(plt)
plot_populations(final.populations.a.det, new.graph=FALSE, col=c("orange", "blue", "purple"), lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' # Increase initial infecteds
#'
#'

#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.b<-0.2
disease.recovery.rate.b <-0.1

# Starting population size

num.cattle.b<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.b <- 10
initial.recovereds.b<-0
initial.susceptibles.b <- num.cattle.b-initial.infecteds.b-initial.recovereds.b

# And setting times
start.time.b <- 0
end.time.b <- 100

# Timestep of our choice
this.timesteps.b<-1

#' ## Stochastic SIR (Scenario B)
#'
#'
# Our data frame
population.df.b<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b,
                           recovereds=initial.recovereds.b)

final.populations.b <- run_simulation(timestep_stochastic_SIR,
                                    population.df.b,
                                    end.time.b,
                                    transmission.rate = disease.transmission.rate.b,
                                    recovery.rate = disease.recovery.rate.b,
                                    timestep = this.timesteps.b)


plot_populations(final.populations.b)

#' For loop
#'
#'
population.df.b<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b,
                           recovereds= initial.recovereds.b)

first.graph<-TRUE
for (i in 1:20){
  population.df.b<- data.frame(time=start.time.b,
                             susceptibles = initial.susceptibles.b,
                             infecteds=initial.infecteds.b,
                             recovereds=initial.recovereds.b)

  timesteps.b<-seq(from=start.time.b + this.timesteps.b, to=end.time.b )
  final.populations.b <- run_simulation(timestep_stochastic_SIR,
                                      tail(population.df.b,1),
                                      end.time.b,
                                      transmission.rate = disease.transmission.rate.b,
                                      recovery.rate = disease.recovery.rate.b,
                                      timestep = this.timesteps.b)
  if (first.graph){
    plot_populations(final.populations.b,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.b,
                     new.graph = FALSE)
  }
}

plt <- recordPlot()


#' ## Deterministic SIR (Scenario B)
population.df.b.det<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b,
                           recovereds= initial.recovereds.b)


final.populations.b.det <- run_simulation(timestep_deterministic_SIR,
                                    population.df.b.det,
                                    end.time.b,
                                    transmission.rate = disease.transmission.rate.b,
                                    recovery.rate = disease.recovery.rate.b,
                                    timestep = this.timesteps.b)

#' **Plot together**
#'
#' We can also observe the variability in the stochastic model in this scenario.
#' By increasing the number of initial infected, the outbreak happens faster.
#' It can also be seen that the stochastic models end earlier than the deterministic.
#' This is because the simulation ends when the infected population is 0,
#' which happens when all individuals recover.
#' This is not the case for the deterministic model, given that we didn't give it a condition when to end,
#' so it only ends when the it reaches the end.time (in this case 100)
#'
#'
replayPlot(plt)
plot_populations(final.populations.b.det, new.graph=FALSE, col=c("orange", "blue", "purple"), lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' # Increase number of infecteds and recovereds
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.c<-0.2
disease.recovery.rate.c <-0.1

# Starting population size

num.cattle.c<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.c <- 10
initial.recovereds.c<-5
initial.susceptibles.c <- num.cattle.c-initial.infecteds.c-initial.recovereds.c

# And setting times
start.time.c <- 0
end.time.c <- 100

# Timestep of our choice
this.timesteps.c<-1

#' ## Stochastic SIR (Scenario C)
#'
#'
# Our data frame
population.df.c<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c,
                           recovereds=initial.recovereds.c)

# Run simulation once
final.populations.c <- run_simulation(timestep_stochastic_SIR,
                                      population.df.c,
                                      end.time.c,
                                      transmission.rate = disease.transmission.rate.c,
                                      recovery.rate = disease.recovery.rate.c,
                                      timestep = this.timesteps.c)


plot_populations(final.populations.c)

#' For loop
#'
#'
population.df.c<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c,
                           recovereds= initial.recovereds.c)

first.graph<-TRUE
for (i in 1:20){
  population.df.c<- data.frame(time=start.time.c,
                             susceptibles = initial.susceptibles.c,
                             infecteds=initial.infecteds.c,
                             recovereds=initial.recovereds.c)

  timesteps.c<-seq(from=start.time.c + this.timesteps.c, to=end.time.c )
  final.populations.c <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.c,1),
                                        end.time.c,
                                        transmission.rate = disease.transmission.rate.c,
                                        recovery.rate = disease.recovery.rate.c,
                                        timestep = this.timesteps.c)
  if (first.graph){
    plot_populations(final.populations.c,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.c,
                     new.graph = FALSE)
  }
}

plt <- recordPlot()


#' ## Deterministic SIR (Scenario C)
population.df.c.det<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c,
                           recovereds= initial.recovereds.c)


final.populations.c.det <- run_simulation(timestep_deterministic_SIR,
                                    population.df.c.det,
                                    end.time.c,
                                    transmission.rate = disease.transmission.rate.c,
                                    recovery.rate = disease.recovery.rate.c,
                                    timestep = this.timesteps.c)

#' **Plot together**
#'
#'
#' By also increasing the initial recovereds, the disease dies out quicker,
#' therefore the stochastic simulation also end earlier than in scenario B.
#'
#'
replayPlot(plt)
plot_populations(final.populations.c.det, new.graph=FALSE, col=c("orange", "blue", "purple"),lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' # Increase population size
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.d<-0.2
disease.recovery.rate.d <-0.1

# Starting population size

num.cattle.d<-500

# Set the initial number of infected and susceptible  individuals

initial.infecteds.d <- 2
initial.recovereds.d<-0
initial.susceptibles.d <- num.cattle.d-initial.infecteds.d-initial.recovereds.d

# And setting times
start.time.d <- 0
end.time.d <- 100

# Timestep of our choice
this.timesteps.d<-1

#' ## Stochastic SIR (Scenario D)
#'
#'
# Our data frame
population.df.d<- data.frame(time=start.time.d,
                           susceptibles = initial.susceptibles.d,
                           infecteds=initial.infecteds.d,
                           recovereds=initial.recovereds.d)

# Run simulation once
final.populations.d <- run_simulation(timestep_stochastic_SIR,
                                      population.df.d,
                                      end.time.d,
                                      transmission.rate = disease.transmission.rate.d,
                                      recovery.rate = disease.recovery.rate.d,
                                      timestep = this.timesteps.d)


plot_populations(final.populations.d)

#' For loop
#'
#'
population.df.d<- data.frame(time=start.time.d,
                           susceptibles = initial.susceptibles.d,
                           infecteds=initial.infecteds.d,
                           recovereds= initial.recovereds.d)

first.graph<-TRUE
for (i in 1:20){
  population.df.d<- data.frame(time=start.time.d,
                             susceptibles = initial.susceptibles.d,
                             infecteds=initial.infecteds.d,
                             recovereds=initial.recovereds.d)

  timesteps.d<-seq(from=start.time.d + this.timesteps.d, to=end.time.d )
  final.populations.d <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.d,1),
                                        end.time.d,
                                        transmission.rate = disease.transmission.rate.d,
                                        recovery.rate = disease.recovery.rate.d,
                                        timestep = this.timesteps.d)
  if (first.graph){
    plot_populations(final.populations.d,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.d,
                     new.graph = FALSE)
  }
}

plt <- recordPlot()


#' ## Deterministic SIR
population.df.d.det<- data.frame(time=start.time.d,
                           susceptibles = initial.susceptibles.d,
                           infecteds=initial.infecteds.d,
                           recovereds= initial.recovereds.d)


final.populations.d.det <- run_simulation(timestep_deterministic_SIR,
                                    population.df.d.det,
                                    end.time.d,
                                    transmission.rate = disease.transmission.rate.d,
                                    recovery.rate = disease.recovery.rate.d,
                                    timestep = this.timesteps.d)

#' **Plot together**
#'
#'
#' By increasing the population size to 500, we still see an outbreak.
#' But it takes longer to develop than when the population is 100, like in scenario A.
#' Therefore, the disease takes longer to die out as well.
#' We can see in this scenario that the stochastic models don't get a chance to end because there are no infected left.
#' We would need to increase the end.time to be able to see this happen.
#'
replayPlot(plt)
plot_populations(final.populations.d.det, new.graph=FALSE, col=c("orange", "blue", "purple"), lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))


#' Increase transmission rate
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.e<-0.8
disease.recovery.rate.e <-0.1

# Starting population size

num.cattle.e<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.e <- 2
initial.recovereds.e<-0
initial.susceptibles.e <- num.cattle.e-initial.infecteds.e-initial.recovereds.e

# And setting times
start.time.e <- 0
end.time.e <- 100

# Timestep of our choice
this.timesteps.e<-1

#' ## Stochastic SIR
#'
#'
# Our data frame
population.df.e<- data.frame(time=start.time.e,
                           susceptibles = initial.susceptibles.e,
                           infecteds=initial.infecteds.e,
                           recovereds=initial.recovereds.e)

# Run simulation once
final.populations.e <- run_simulation(timestep_stochastic_SIR,
                                    population.df.e,
                                    end.time.e,
                                    transmission.rate = disease.transmission.rate.e,
                                    recovery.rate = disease.recovery.rate.e,
                                    timestep = this.timesteps.e)


plot_populations(final.populations.e)

#' For loop
#'
#'
population.df.e<- data.frame(time=start.time.e,
                           susceptibles = initial.susceptibles.e,
                           infecteds=initial.infecteds.e,
                           recovereds= initial.recovereds.e)

first.graph<-TRUE
for (i in 1:20){
  population.df.e<- data.frame(time=start.time.e,
                             susceptibles = initial.susceptibles.e,
                             infecteds=initial.infecteds.e,
                             recovereds=initial.recovereds.e)

  timesteps.e<-seq(from=start.time.e + this.timesteps.e, to=end.time.e )
  final.populations.e <- run_simulation(timestep_stochastic_SIR,
                                      tail(population.df.e,1),
                                      end.time.e,
                                      transmission.rate = disease.transmission.rate.e,
                                      recovery.rate = disease.recovery.rate.e,
                                      timestep = this.timesteps.e)
  if (first.graph){
    plot_populations(final.populations.e,
                     new.graph = TRUE)
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.e,
                     new.graph = FALSE)
  }
}

plt <- recordPlot()


#' ## Deterministic SIR
population.df.e.det<- data.frame(time=start.time.e,
                           susceptibles = initial.susceptibles.e,
                           infecteds=initial.infecteds.e,
                           recovereds= initial.recovereds.e)


final.populations.e.det <- run_simulation(timestep_deterministic_SIR,
                                    population.df.e.det,
                                    end.time.e,
                                    transmission.rate = disease.transmission.rate.e,
                                    recovery.rate = disease.recovery.rate.e,
                                    timestep = this.timesteps.e)


#' **Plot together**
#'
#' When we increase the transmission rate, we see a bigger infected population during the outbreak,
#' Given that most of the population gets infected very quickly,
#' the susceptible population decreases with it.
#' And when the susceptible population get's close to zero, the infected population starts to decrease,
#' and the recovered population starts to increase.
#' The stochastic models don't end earlier because the infected population is still recovering.
#'
#'
replayPlot(plt)
plot_populations(final.populations.e.det, new.graph=FALSE, col=c("orange", "blue", "purple"),lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green", "orange", "blue", "purple"), lty=c(1,1,1,1,1,1))

#' The transmission rate to get an R0 of 15, with a recovery rate of 0.1, is 1.5.
#' But we can't model with this parameter given that the function won't work with a transmission rate greater than 1.
