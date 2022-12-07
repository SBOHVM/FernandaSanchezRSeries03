#' timestep_stochastic_SIS
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

timestep_stochastic_SIS <- function(latest, transmission.rate, recovery.rate, timestep) {
  population.size<-latest$susceptibles+ latest$infecteds
  effective.transmission.rate<- transmission.rate*timestep
  actual.infection.rate<-effective.transmission.rate * (latest$infected/population.size)
  effective.recovery.rate<-recovery.rate*timestep

  if ((effective.transmission.rate >= 1) || (effective.recovery.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  new.recovered <- stats::rbinom(1,latest$infected, effective.recovery.rate)
  new.infected<-stats::rbinom(1,latest$susceptibles, actual.infection.rate)

  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered

  # Return data frame containing next population count
  next.population<- data.frame(susceptibles = next.susceptibles,
                               infecteds=next.infecteds,
                               time=latest$time+timestep)


  #Determine whether the experiment is finished
  is.finished<-(next.infecteds==0)

  #Return both these pieces of information
  list(updated.pop = next.population, end.experiment = is.finished)

}






if (length(codetools::findGlobals(timestep_stochastic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function timestep_stochastic_SIS may not use global variable(s): ",
    codetools::findGlobals(timestep_stochastic_SIS, merge = FALSE)$variables
  )
}
