#' timestep_stochastic_SIS
#'
#' Run a timestep of a simple stochastic SIS model
#'
#' @param latest a data.frame containing the latest infecteds and susceptibles, as well as time
#' (column is 'infecteds', 'susceptibles' and 'time')
#' @param transmission.rate the transmission rate
#' @param recovery.rate the transmission rate
#' @param timestep the time interval
#'
#' @return Returns a data.frame containing the updated population of infecteds and susceptibles, as wel as time
#' @export
#'

timestep_stochastic_SIS <- function(latest, transmission.rate, recovery.rate, timestep) {
  #Calculate population changes
  population.size<-latest$susceptibles+ latest$infecteds
  effective.transmission.rate<- transmission.rate*timestep
  actual.infection.rate<-effective.transmission.rate * (latest$infected/population.size)
  effective.recovery.rate<-recovery.rate*timestep

  # Stop the function if transmission or recovery rate is bigger than 1
  if ((actual.infection.rate >= 1) || (effective.recovery.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  #Calculate new recovered and new infected with random binomial numbers with a probability of transmission or recovery rate, to make it stochastic.
  new.recovered <- stats::rbinom(1,latest$infected, effective.recovery.rate)
  new.infected<-stats::rbinom(1,latest$susceptibles, actual.infection.rate)

  # Our final equation
  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered

  # Return data frame containing next population of infecteds and susceptibles, as well as time
  next.population<- data.frame(susceptibles = next.susceptibles,
                               infecteds=next.infecteds,
                               time=latest$time+timestep)


  #Determine whether the experiment is finished
  is.finished<-(next.infecteds==0)

  #Return both these pieces of information
  list(updated.pop = next.population, end.experiment = is.finished)

}


RPiR::assert_no_globals(timestep_stochastic_SIS)



