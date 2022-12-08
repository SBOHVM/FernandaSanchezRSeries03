#' timestep_stochastic_SIR
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

timestep_stochastic_SIR <- function(latest, transmission.rate, recovery.rate, timestep) {
  population.size<-latest$susceptibles+ latest$infecteds + latest$recovereds
  effective.transmission.rate<- transmission.rate*timestep
  actual.infection.rate<-effective.transmission.rate * (latest$infected/population.size)
  effective.recovery.rate<-recovery.rate*timestep

  if ((effective.transmission.rate >= 1) || (effective.recovery.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  new.recovered <- stats::rbinom(1,latest$infected, effective.recovery.rate)
  new.infected<-stats::rbinom(1,latest$susceptibles, actual.infection.rate)

  next.susceptibles <- latest$susceptibles - new.infected
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  next.recovereds<-latest$recovereds+new.recovered



  # Return data frame containing next population count
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


