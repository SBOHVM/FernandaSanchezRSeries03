#' timestep_stochastic_birth_death
#'
#' Run one step of a simple deterministic exponential birth-death model
#'
#' @param latest a data.frame containing the latest population count
#' (column is 'count')
#' @param birth.rate the birth rate
#' @param death.rate the death rate
#' @param timestep
#'
#' @return Returns a data.frame containing the updated population
#' @export
#'
timestep_stochastic_birth_death <- function(latest,birth.rate, death.rate, timestep) {
  # Calculate population changes
  effective.birth.rate<-birth.rate*timestep
  effective.death.rate<-death.rate*timestep
  new.count<-latest$count
  if ((effective.birth.rate >= 1) || (effective.death.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  new.births <- stats::rbinom(1, new.count, effective.birth.rate)
  new.deaths <- stats::rbinom(1, new.count, effective.death.rate)

  next.count <- new.count + new.births - new.deaths

  # Return data frame containing next population count
  next.population<- data.frame(count = next.count,
                              time=latest$time+timestep)


  #Determine whether the experiment is finished
  is.finished<-(next.count==0)

  #Return both these pieces of information
  list(updated.pop = next.population, end.experiment = is.finished)

}



if (length(codetools::findGlobals(timestep_stochastic_birth_death,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function timestep_stochastic_birth_death may not use global variable(s): ",
    codetools::findGlobals(timestep_stochastic_birth_death, merge = FALSE)$variables
  )
}
