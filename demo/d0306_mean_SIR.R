#' ---
#' title: "Average stochastic SIR model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

library(RPiR)
library(FernandaSanchezRSeries03)
library(stats)

#' We are going to compare the deterministic Susceptible-Infected-Recovered (SIR) model and the average of the stochastic models.
#'
#'
#'**SIR Model**
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-\beta \times \frac{S(t)\times I(t)}{N}+\sigma \times I(t)$$
#'
#' 2. Infected model
#'
#'    $$I(t + 1) = S(t)+\beta \times \frac{S(t)\times I(t)}{N}-\sigma \times I(t)$$
#'
#' 3. Recovered model
#'
#'    $$R(t + 1) = R(t)+\sigma \times I(t)$$
#'
#' 4. N is a constant for total population
#'
#'    $$N = S(t)+ I(t) + R(t)$$
#'
#'


#' # Simulation A where R0=2
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

transmission.rate.a<-0.2
recovery.rate.a <-0.1

# Starting population size

num.cattle.a<-100

# Set the initial number of infected and susceptible individuals

initial.infecteds.a <- 2
initial.recovereds.a<-0
initial.susceptibles.a <- num.cattle.a-initial.infecteds.a-initial.recovereds.a

# And setting times
start.time.a <- 0
end.time.a <- 100

# Timeste of our choice
this.timesteps.a<-1

#' ## Average stochastic SIR
#'
#'
#' Our data frame
population.df.a<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a,
                           recovereds= initial.recovereds.a)

#' For loop to see stochasticity, with 100 simulations.
#'
#'
#' To add data frames we first need to create an empty data frame outside of the loop.
#' Then in the 'if' statement we need to specify that we want the first information and plot into the empty data frame.
#' Then, the rest of the information and plots, will keep adding on to the data frame
#'
#'
#' We also need to include cleanup_timesteps to allow all simulation to have the same number of rows.
#' In this case it will extend the data frame until the end.time when it happens to finish before that.
#' If not, they can not be added and we get and error.
#'
#'
first.graph<-TRUE
final.frame.a<-data.frame()
for (i in 1:100){
  population.df.a<- data.frame(time=start.time.a,
                             susceptibles = initial.susceptibles.a,
                             infecteds=initial.infecteds.a,
                             recovereds=initial.recovereds.a)

  timesteps.a<-seq(from=start.time.a + this.timesteps.a, to=end.time.a )
  final.populations.a <- run_simulation(timestep_stochastic_SIR,
                                      tail(population.df.a,1),
                                      end.time.a,
                                      transmission.rate = transmission.rate.a,
                                      recovery.rate = recovery.rate.a,
                                      timestep = this.timesteps.a)

  final.populations.a <- cleanup_timesteps(final.populations.a,
                                           timestep = this.timesteps.a,
                                           end.time = end.time.a)
  if (first.graph){
    plot_populations(final.populations.a,
                     new.graph = TRUE)
    final.frame.a<-final.populations.a
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.a,
                     new.graph = FALSE)
    final.frame.a<-final.frame.a+final.populations.a
  }
  }


#' Calculate the mean for the stochastic model by diving all the data frames by the number of runs in the 'for' loop.
#'
mean.a<-final.frame.a/100

#' Plot the average
plot_populations(mean.a, new.graph=TRUE)
plt <- recordPlot()

#' ## Deterministic SIR
#'
#'
# Our data frame
population.a.det<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a,
                           recovereds= initial.recovereds.a)


final.populations.a.det <- run_simulation(timestep_deterministic_SIR,
                                    population.a.det,
                                    end.time.a,
                                    transmission.rate = transmission.rate.a,
                                    recovery.rate = recovery.rate.a,
                                    timestep = this.timesteps.a)

#' ** Plot together**
#'
#'
#' We can see the difference between the average stochastic model and the deterministic.
#' The number of infected individuals in the stochastic model is lower,
#' therefore the susceptible population decreases slower as well as the recovered population.
#' This indicated that the outbreak will take longer to die out with the average stochastic model.
#'
replayPlot(plt)
plot_populations(final.populations.a.det, new.graph=FALSE, lty=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green"), lty=c(1,1,1,2,2,2))

#' # Simulation B= Increase initial infected population
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

transmission.rate.b<-0.2
recovery.rate.b <-0.1

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



#' ## Average stochastic SIR
#'
#'
# Our data frame
population.df.b<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b,
                           recovereds= initial.recovereds.b)

#' Same method to add data frames as in scenario A
first.graph<-TRUE
final.frame.b<-data.frame()
for (i in 1:100){
  population.df.b<- data.frame(time=start.time.b,
                             susceptibles = initial.susceptibles.b,
                             infecteds=initial.infecteds.b,
                             recovereds=initial.recovereds.b)

  timesteps.b<-seq(from=start.time.b + this.timesteps.b, to=end.time.b )
  final.populations.b <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.b,1),
                                        end.time.b,
                                        transmission.rate = transmission.rate.b,
                                        recovery.rate = recovery.rate.b,
                                        timestep = this.timesteps.b)

  final.populations.b <- cleanup_timesteps(final.populations.b,
                                           timestep = this.timesteps.b,
                                           end.time = end.time.b)
  if (first.graph){
    plot_populations(final.populations.b,
                     new.graph = TRUE)
    final.frame.b<-final.populations.b
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.b,
                     new.graph = FALSE)
    final.frame.b<-final.frame.b+final.populations.b
  }
}


#Calculate the mean for the stochastic model
mean.b<-final.frame.b/100

# Plot average
plot_populations(mean.b)
plt <- recordPlot()

#' ## Deterministic SIR
population.b.det<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b,
                           recovereds= initial.recovereds.b)

final.populations.b.det <- run_simulation(timestep_deterministic_SIR,
                                        population.b.det,
                                        end.time.b,
                                        transmission.rate = transmission.rate.b,
                                        recovery.rate = recovery.rate.b,
                                        timestep = this.timesteps.b)

#' **Plot together**
#'
#'
#' By increasing the initial population size we can see that the average stochastic model
#' and the deterministic model look more similar than in scenario A.
#'
replayPlot(plt)
plot_populations(final.populations.b.det, new.graph=FALSE, lty=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green"), lty=c(1,1,1,2,2,2))

#' # Simulation C= Small initial infected population and low values of R0
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

transmission.rate.c<-0.05
recovery.rate.c <-0.1

# Starting population size

num.cattle.c<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.c <- 2
initial.recovereds.c<-0
initial.susceptibles.c <- num.cattle.c-initial.infecteds.c-initial.recovereds.c

# And setting times
start.time.c <- 0
end.time.c <- 100

# Timestep of our choice

this.timesteps.c<-1


#' ## Average stochastic SIR
#'
#'
# Our data frame
population.df.c<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c,
                           recovereds= initial.recovereds.c)

#' Same method to add data frames as in scenario A
first.graph<-TRUE
final.frame.c<-data.frame()
for (i in 1:100){
  population.df.c<- data.frame(time=start.time.c,
                             susceptibles = initial.susceptibles.c,
                             infecteds=initial.infecteds.c,
                             recovereds=initial.recovereds.c)

  timesteps.c<-seq(from=start.time.c + this.timesteps.c, to=end.time.c )
  final.populations.c <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.c,1),
                                        end.time.c,
                                        transmission.rate = transmission.rate.c,
                                        recovery.rate = recovery.rate.c,
                                        timestep = this.timesteps.c)

  final.populations.c <- cleanup_timesteps(final.populations.c,
                                           timestep = this.timesteps.c,
                                           end.time = end.time.c)
  if (first.graph){
    plot_populations(final.populations.c,
                     new.graph = TRUE)
    final.frame.c<-final.populations.c
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.c,
                     new.graph = FALSE)
    final.frame.c<-final.frame.c+final.populations.c
  }
}


#Calculate the mean for the stochastic model
mean.c<-final.frame.c/100

# Plot average

plot_populations(mean.c)
plt <- recordPlot()

#' ## Deterministic SIR
#'
#'
population.c.det<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c,
                           recovereds= initial.recovereds.c)

final.populations.c.det <- run_simulation(timestep_deterministic_SIR,
                                        population.c.det,
                                        end.time.c,
                                        transmission.rate = transmission.rate.c,
                                        recovery.rate = recovery.rate.c,
                                        timestep = this.timesteps.c)

#' **Plot together**
#'
#'
#' Given that in this scenario R0 is lower than 1, there is no outbreak.
#' Which is shown by both models.
#'
replayPlot(plt)
plot_populations(final.populations.c.det, new.graph=FALSE,lty=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green"), lty=c(1,1,1,2,2,2))


#' # Simulation D= High R0 and high initial numbers of infecteds
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

transmission.rate.d<-0.4
recovery.rate.d <-0.1

# Starting population size

num.cattle.d<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.d <- 20
initial.recovereds.d<-0
initial.susceptibles.d <- num.cattle.d-initial.infecteds.d-initial.recovereds.d

# And setting times
start.time.d <- 0
end.time.d <- 100

# Timestep of our choice

this.timesteps.d<-1


#' ## Average stochastic SIR
#'
#'
# Our data frame
population.df.d<- data.frame(time=start.time.d,
                             susceptibles = initial.susceptibles.d,
                             infecteds=initial.infecteds.d,
                             recovereds= initial.recovereds.d)

#' Same method to add data frames as in scenario A
first.graph<-TRUE
final.frame.d<-data.frame()
for (i in 1:100){
  population.df.d<- data.frame(time=start.time.d,
                               susceptibles = initial.susceptibles.d,
                               infecteds=initial.infecteds.d,
                               recovereds=initial.recovereds.d)

  timesteps.d<-seq(from=start.time.d + this.timesteps.d, to=end.time.d )
  final.populations.d <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.d,1),
                                        end.time.d,
                                        transmission.rate = transmission.rate.d,
                                        recovery.rate = recovery.rate.d,
                                        timestep = this.timesteps.d)

  final.populations.d <- cleanup_timesteps(final.populations.d,
                                           timestep = this.timesteps.d,
                                           end.time = end.time.d)
  if (first.graph){
    plot_populations(final.populations.d,
                     new.graph = TRUE)
    final.frame.d<-final.populations.d
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.d,
                     new.graph = FALSE)
    final.frame.d<-final.frame.d+final.populations.d
  }
}


#Calculate the mean for the stochastic model
mean.d<-final.frame.d/100

# Plot average

plot_populations(mean.d)
plt <- recordPlot()

#' ## Deterministic SIR
#'
#'
population.d.det<- data.frame(time=start.time.d,
                              susceptibles = initial.susceptibles.d,
                              infecteds=initial.infecteds.d,
                              recovereds= initial.recovereds.d)

final.populations.d.det <- run_simulation(timestep_deterministic_SIR,
                                          population.d.det,
                                          end.time.d,
                                          transmission.rate = transmission.rate.d,
                                          recovery.rate = recovery.rate.d,
                                          timestep = this.timesteps.d)

#' **Plot together**
#'
#'
#' Here we can see that with an R0 of 4 and a high number of initial infecteds,
#' the outbreak happens earlier in time.
#' The average stochastic and deterministic model are very similar in this case as well.
#'
replayPlot(plt)
plot_populations(final.populations.d.det, new.graph=FALSE,lty=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green"), lty=c(1,1,1,2,2,2))


#' # Simulation E= High R0 and low initial infected
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

transmission.rate.e<-0.4
recovery.rate.e <-0.1

# Starting population size

num.cattle.e<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.e <- 1
initial.recovereds.e<-0
initial.susceptibles.e <- num.cattle.e-initial.infecteds.e-initial.recovereds.e

# And setting times
start.time.e <- 0
end.time.e <- 100

# Timestep of our choice
this.timesteps.e<-1

#' ## Average stochastic SIR
#'
#'Same method to add data frames as in scenario A
#'
population.df.e<- data.frame(time=start.time.e,
                           susceptibles = initial.susceptibles.e,
                           infecteds=initial.infecteds.e,
                           recovereds= initial.recovereds.e)

first.graph<-TRUE
final.frame.e<-data.frame()
for (i in 1:100){
  population.df.e<- data.frame(time=start.time.e,
                             susceptibles = initial.susceptibles.e,
                             infecteds=initial.infecteds.e,
                             recovereds=initial.recovereds.e)

  timesteps.e<-seq(from=start.time.e + this.timesteps.e, to=end.time.e )
  final.populations.e <- run_simulation(timestep_stochastic_SIR,
                                        tail(population.df.e,1),
                                        end.time.e,
                                        transmission.rate = transmission.rate.e,
                                        recovery.rate = recovery.rate.e,
                                        timestep = this.timesteps.e)

  final.populations.e <- cleanup_timesteps(final.populations.e,
                                           timestep = this.timesteps.e,
                                           end.time = end.time.e)
  if (first.graph){
    plot_populations(final.populations.e,
                     new.graph = TRUE)
    final.frame.e<-final.populations.e
    first.graph <- FALSE
  } else {
    plot_populations(final.populations.e,
                     new.graph = FALSE)
    final.frame.e<-final.frame.e + final.populations.e
  }
}


# Calculate the mean for the stochastic model
mean.e<-final.frame.e/100

# Plot average
plot_populations(mean.e)
plt <- recordPlot()

#' ## Deterministic SIR
population.e.det<- data.frame(time=start.time.e,
                           susceptibles = initial.susceptibles.e,
                           infecteds=initial.infecteds.e,
                           recovereds= initial.recovereds.e)

final.populations.e.det<- run_simulation(timestep_deterministic_SIR,
                                        population.e.det,
                                        end.time.e,
                                        transmission.rate = transmission.rate.e,
                                        recovery.rate = recovery.rate.e,
                                        timestep = this.timesteps.e)

#' **Plot together**
#'
#'  In this case the deterministic and average stochastic model are not very similar.
#'  This is because with the stochastic model we are able to account for the possibility of "stochastic extinction".
#'  This stochastic extinction happens when we have a high R0 (4) and a low initial infected population, in this case 1 individual.
#'  So, with the stochastic model there is a possibility that that 1 individual will recover and not transmit the disease to anyone,
#'  therefore not causing an outbreak.
#'  Or the possibility of infecting everyone and transmitting very fast, causing a big outbreak.
#'  With this probabilities, the average of the stochastic simulation is somewhere in between these 2 possible scenarios.
#'  And the reason why it's so different from the deterministic, is because the deterministic doesn't account
#'  for the possibility of that individual recovering and the disease not being transmitted.
#'
replayPlot(plt)
plot_populations(final.populations.e.det, new.graph=FALSE, lty=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Stochastic=recov.","Deterministic= susc.", "Deterministic=infect.", "Deterministic=recov."),
       col = c("black", "red","green"), lty=c(1,1,1,2,2,2))
