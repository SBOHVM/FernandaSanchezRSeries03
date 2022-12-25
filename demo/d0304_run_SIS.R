#' ---
#' title: "Stochastic Susceptible-Infected-Susceptible (SIS) model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

library(RPiR)
library(FernandaSanchezRSeries03)

#' We are going to compare the stochastic and deterministic Susceptible-Infected-Susceptible (SIS) models.
#'
#' **SIS model**
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-\beta \times \frac{S(t)\times I(t)}{N}+\sigma \times I(t)$$
#'
#' 2. Infected model
#'
#'    $$I(t + 1) = S(t)+\beta \times \frac{S(t)\times I(t)}{N}-\sigma \times I(t)$$
#'
#' 3. N is a constant for total population
#'
#'    $$N = S(t)+ I(t)$$
#'
#'

#' # Compare stochastic and deterministic models
#'
#'
#' ## Stochastic SIS (Scenario A)

#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.a<-0.3
disease.recovery.rate.a <-0.1

# Starting population size

num.cattle.a<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.a <- 2
initial.susceptibles.a <- num.cattle.a-initial.infecteds.a

# And setting times
start.time.a <- 0
end.time.a <- 100

# Timestep of our choice
this.timesteps.a<-1

# Our data frame
population.df.a<- data.frame(time=start.time.a,
                            susceptibles = initial.susceptibles.a,
                            infecteds=initial.infecteds.a)

# Run simulation once
final.populations.a <- run_simulation(timestep_stochastic_SIS,
                                      population.df.a,
                                      end.time.a,
                                      transmission.rate = disease.transmission.rate.a,
                                      recovery.rate = disease.recovery.rate.a,
                                      timestep = this.timesteps.a)


plot_populations(final.populations.a)


#' For loop to run simulation several times
#'
#'
population.df.a<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a)

first.graph<-TRUE
for (i in 1:20){
  population.df.a<- data.frame(time=start.time.a,
                             susceptibles = initial.susceptibles.a,
                             infecteds=initial.infecteds.a)

  timesteps.a<-seq(from=start.time.a + this.timesteps.a, to=end.time.a )
  final.populations.a <- run_simulation(timestep_stochastic_SIS,
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

# This lets us record the plot to use it later with another plot.
plt <- recordPlot()

#' ## Deterministic SIS (Scenario A)
#'
#'
population.df.a.det<- data.frame(time=start.time.a,
                           susceptibles = initial.susceptibles.a,
                           infecteds=initial.infecteds.a)

#' Run simulation
final.populations.a.det <- run_simulation(timestep_deterministic_SIS,
                                   population.df.a.det,
                                   end.time.a,
                                   transmission.rate = disease.transmission.rate.a,
                                   recovery.rate = disease.recovery.rate.a,
                                   timestep = this.timesteps.a)


#' **Plot together**
#'
#' We can see how the scenarios of the stochastic models can be very different from the deterministic model.
#' By adding probabilities and randomness, we are able to see that some scenarios might start the outbreak earlier than the deterministic.
#' This way, it gives us a chance to prepare for more than one situation as well as giving us a framework of time.
#' Meaning, that we can say that the outbreak might start from 18 days to 40 days, approximately,
#' giving us a window of action.
#' Where as, with the deterministic, it only give us one scenario and doesn't let us prepare for other cases.
#'
#'
# We call the plot created above to continue plotting with it.
replayPlot(plt)
plot_populations(final.populations.a.det, new.graph=FALSE, col=c("orange", "blue"), lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Deterministic= susc.", "Deterministic=infect."),
       col = c("black", "red","orange", "blue"), lty=c(1,1,1,1))


#' # Increased transmission rate and initial infecteds
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.b<-0.5
disease.recovery.rate.b <-0.1

# Starting population size

num.cattle.b<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.b <- 5
initial.susceptibles.b <- num.cattle.b-initial.infecteds.b

# And setting times
start.time.b <- 0
end.time.b <- 100

# Timestep of our choice
this.timesteps.b<-1

#' ## Stochastic SIS (Scenario B)
#'
#'
#Our data frame
population.df.b<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b)

#Run simulation once
final.populations.b <- run_simulation(timestep_stochastic_SIS,
                                population.df.b,
                                end.time.b,
                                transmission.rate = disease.transmission.rate.b,
                                recovery.rate = disease.recovery.rate.b,
                                timestep = this.timesteps.b)


plot_populations(final.populations.b)

#' For loop to get various simulations at once
#'
#'
population.df.b<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b)

first.graph<-TRUE
for (i in 1:20){
  population.df.b<- data.frame(time=start.time.b,
                             susceptibles = initial.susceptibles.b,
                             infecteds=initial.infecteds.b)

  timesteps.b<-seq(from=start.time.b + this.timesteps.b, to=end.time.b )
  final.populations.b <- run_simulation(timestep_stochastic_SIS,
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

#' ## Deterministic SIS (Scenario B)
#'
#'
population.df.b.det<- data.frame(time=start.time.b,
                           susceptibles = initial.susceptibles.b,
                           infecteds=initial.infecteds.b)

#' Run simulation once
final.populations.b.det <- run_simulation(timestep_deterministic_SIS,
                                    population.df.b.det,
                                    end.time.b,
                                    transmission.rate = disease.transmission.rate.b,
                                    recovery.rate = disease.recovery.rate.b,
                                    timestep = this.timesteps.b)


#' **Plot together**
#'
#' When we increase the transmission rate and initial infecteds, the outbreak starts earlier than in the previous case.
#' We can also see that the there is less variation than before.
#' This is because, the probability of transmission is higher in this case,
#' therefore there is less variation on when the outbreak starts or how many susceptibles and infecteds there are.
#'
replayPlot(plt)
plot_populations(final.populations.b.det, new.graph=FALSE, col=c("orange", "blue"),lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Deterministic= susc.", "Deterministic=infect."),
       col = c("black", "red","orange", "blue"), lty=c(1,1,1,1))


#' # Decrease transmission rate and increase initial infecteds
#'
#'
disease.transmission.rate.c<-0.1
disease.recovery.rate.c <-0.1

# Starting population size

num.cattle.c<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds.c <- 10
initial.susceptibles.c <- num.cattle.c-initial.infecteds.c

# And setting times
start.time.c <- 0
end.time.c <- 100

# Timestep of our choice
this.timesteps.c<-1

#' ## Stochastic SIS (Scenario C)
#'
#'
# Our data frame
population.df.c<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c)

# Run simulation once
final.populations.c <- run_simulation(timestep_stochastic_SIS,
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
                           infecteds=initial.infecteds.c)

first.graph<-TRUE
for (i in 1:20){
  population.df.c<- data.frame(time=start.time.c,
                             susceptibles = initial.susceptibles.c,
                             infecteds=initial.infecteds.c)

  timesteps.c<-seq(from=start.time.c + this.timesteps.c, to=end.time.c )
  final.populations.c <- run_simulation(timestep_stochastic_SIS,
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

#' ## Deterministic SIS (Scenario C)
#'
#'
population.df.c.det<- data.frame(time=start.time.c,
                           susceptibles = initial.susceptibles.c,
                           infecteds=initial.infecteds.c)


final.populations.c.det <- run_simulation(timestep_deterministic_SIS,
                                    population.df.c.det,
                                    end.time.c,
                                    transmission.rate = disease.transmission.rate.c,
                                    recovery.rate = disease.recovery.rate.c,
                                    timestep = this.timesteps.c)

#' **Plot together**
#'
#' Here we have the same transmission and recovery rate, and we increased the initial infecteds to 10.
#' We can see that even though we increased 5 times the initial infecteds as scenario A,
#' by decreasing the transmission rate, the pathogen was not able to cause an outbreak.
#'
replayPlot(plt)
plot_populations(final.populations.c.det, new.graph=FALSE, col=c("orange", "blue"),lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Deterministic= susc.", "Deterministic=infect."),
       col = c("black", "red","orange", "blue"), lty=c(1,1,1,1))

#' # Increase population size
#'
#'
#' Set up the simulation parameters
# Set the birth and death rates

disease.transmission.rate.d<-0.3
disease.recovery.rate.d <-0.1

# Starting population size

num.cattle.d<-500

# Set the initial number of infected and susceptible  individuals

initial.infecteds.d <- 2
initial.susceptibles.d <- num.cattle.d-initial.infecteds.d

# And setting times
start.time.d <- 0
end.time.d <- 100

# Timestep of our choice
this.timesteps.d<-1

#' ## Stochastic SIS (Scenario D)
#'
#'
# Our data fram
population.df.d<- data.frame(time=start.time.d,
                           susceptibles = initial.susceptibles.d,
                           infecteds=initial.infecteds.d)
# Run simulation once
final.populations.d <- run_simulation(timestep_stochastic_SIS,
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
                           infecteds=initial.infecteds.d)

first.graph<-TRUE
for (i in 1:20){
  population.df.d<- data.frame(time=start.time.d,
                             susceptibles = initial.susceptibles.d,
                             infecteds=initial.infecteds.d)

  timesteps.d<-seq(from=start.time.d + this.timesteps.d, to=end.time.d )
  final.populations.d <- run_simulation(timestep_stochastic_SIS,
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

#' ## Deterministic SIS (Scenario D)
#'
#'
population.df.d.det<- data.frame(time=start.time.d,
                           susceptibles = initial.susceptibles.d,
                           infecteds=initial.infecteds.d)


final.populations.d.det <- run_simulation(timestep_deterministic_SIS,
                                    population.df.d.det,
                                    end.time.d,
                                    transmission.rate = disease.transmission.rate.d,
                                    recovery.rate = disease.recovery.rate.d,
                                    timestep = this.timesteps.d)

#'** Plot together**
#'
#' By increasing the population size, the outbreak still happens, but at a later time than scenario A.
#'
replayPlot(plt)
plot_populations(final.populations.d.det, new.graph=FALSE, col=c("orange", "blue"),lwd=2)
legend("topright", legend = c("Stochastic= susc.", "Stochastic= infect." ,"Deterministic= susc.", "Deterministic=infect."),
       col = c("black", "red","orange", "blue"), lty=c(1,1,1,1))
