#' timestep_deterministic_SIS
#'
#' Run a timestep of a deterministic SIS model
#'
#' @param latest a data.frame containing the latest infected and susceptible population, as well as time
#' (column is 'infecteds', 'susceptibles' and 'time')
#' @param transmission.rate the transmission rate
#' @param recovery.rate the recovery rate
#' @param timestep the time interval
#'
#' @return Returns a data.frame containing the updated population of infecteds and susceptibles, as well as time
#' @export
#'

timestep_deterministic_SIS <- function(latest, transmission.rate, recovery.rate, timestep) {

  # Calculate population changes
  effective.transmission.rate<- transmission.rate*timestep
  effective.recovery.rate<-recovery.rate*timestep
  population.size<-latest$susceptibles+ latest$infecteds

  new.recovered <- effective.recovery.rate * latest$infecteds
  new.infected <- effective.transmission.rate * ((latest$susceptibles*latest$infecteds)/population.size)

  # Our final equation
  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered

  # Return data frame containing next population and time
  data.frame(susceptibles = next.susceptibles,
             infecteds=next.infecteds,
             time=latest$time+timestep)
}

RPiR::assert_no_globals(timestep_deterministic_SIS)





