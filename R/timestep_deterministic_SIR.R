#' timestep_deterministic_SIR
#' Run one step of a simple deterministic SIS model
#'
#' @param latest a data.frame containing the latest population count
#' (column is 'count')
#' @param transmission.rate the birth rate
#' @param recovery.rate the death rate
#' @param timestep
#'
#' @return Returns a data.frame containing the updated population
#' @export
#'


timestep_deterministic_SIR <- function(latest, transmission.rate, recovery.rate, timestep) {

  ## Calculate population changes
  effective.transmission.rate<- transmission.rate*timestep
  effective.recovery.rate<-recovery.rate*timestep

  population.size<-latest$susceptibles+ latest$infecteds+latest$recovereds
  new.recovered <- effective.recovery.rate * latest$infecteds
  new.infected <- effective.transmission.rate * ((latest$susceptibles*latest$infecteds)/population.size)

  next.susceptibles <- latest$susceptibles - new.infected
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  next.recovereds<-latest$recovereds+new.recovered

  ## Return data frame containing next population count
  data.frame(susceptibles = next.susceptibles,
             infecteds=next.infecteds,
             recovereds=next.recovereds,
             time=latest$time+timestep)
}

RPiR::assert_no_globals(timestep_deterministic_SIR)
