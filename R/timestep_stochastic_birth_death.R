#' timestep_stochastic_birth_death
#'
#' Run a timestep of a stochastic birth-death model
#'
#' @param latest a data.frame containing the latest population count and time
#' (column is 'count' and 'time')
#' @param birth.rate the birth rate
#' @param death.rate the death rate
#' @param timestep the time interval
#'
#' @return Returns a data.frame containing the updated population and time
#' @export
#'
timestep_stochastic_birth_death <- function(latest,birth.rate, death.rate, timestep) {
  # Calculate population changes
  effective.birth.rate<-birth.rate*timestep
  effective.death.rate<-death.rate*timestep
  new.count<-latest$count

  #Stop the function if birth or death rate is iqual or greater than 1
  if ((effective.birth.rate >= 1) || (effective.death.rate >= 1))
    stop("Effective rate too high, timestep must be too big")

  #Calculate new births and new deaths with random binomial numbers with a probability of birth or death rate
  new.births <- stats::rbinom(1, new.count, effective.birth.rate)
  new.deaths <- stats::rbinom(1, new.count, effective.death.rate)

  # Our final equation
  next.count <- new.count + new.births - new.deaths

  # Return data frame containing next population count and time
  next.population<- data.frame(count = next.count,
                              time=latest$time+timestep)


  #Determine whether the experiment is finished
  is.finished<-(next.count==0)

  #Return both these pieces of information
  list(updated.pop = next.population, end.experiment = is.finished)

}

RPiR::assert_no_globals(timestep_stochastic_birth_death)



