#' timestep_stochastic_SIR
#'
#' Run a timestep of a simple stochastic SIR model
#'
#' @param latest a data.frame containing the latest population of infecteds, susceptibles and recovereds, as well as time
#' (column is 'infecteds', 'susceptibles', 'recovereds' and 'time')
#' @param transmission.rate the transmission rate
#' @param recovery.rate the recovery rate
#' @param timestep the time interval
#'
#' @return Returns a data.frame containing the updated population of infecteds, susceptibles, recovereds and time.
#' @export
#'

timestep_stochastic_SIR <- function(latest, transmission.rate, recovery.rate, timestep) {
  #Calculate population changes
  population.size<-latest$susceptibles+ latest$infecteds + latest$recovereds
  effective.transmission.rate<- transmission.rate*timestep
  actual.infection.rate<-effective.transmission.rate * (latest$infected/population.size)
  effective.recovery.rate<-recovery.rate*timestep

  # Stop the function if transmission or recovery rate is bigger than 1
  if ((effective.transmission.rate >= 1) || (effective.recovery.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  #Calculate new recovered and new infected with random binomial numbers with a probability of transmission or recovery rate, to make it stochastic.
  new.recovered <- stats::rbinom(1,latest$infected, effective.recovery.rate)
  new.infected<-stats::rbinom(1,latest$susceptibles, actual.infection.rate)

  # Our final equation
  next.susceptibles <- latest$susceptibles - new.infected
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  next.recovereds<-latest$recovereds+new.recovered

  # Return data frame containing next population of infecteds, susceptibles and recovereds, as well as time
  next.population<- data.frame(susceptibles = next.susceptibles,
                               infecteds=next.infecteds,
                               recovereds=next.recovereds,
                               time=latest$time+timestep)


  #Determine whether the experiment is finished
  is.finished<-(next.infecteds==0)

  #Return both these pieces of information
  list(updated.pop = next.population, end.experiment = is.finished)

}


RPiR::assert_no_globals(timestep_stochastic_SIR)


