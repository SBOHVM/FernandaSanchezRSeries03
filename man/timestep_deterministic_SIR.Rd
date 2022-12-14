% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timestep_deterministic_SIR.R
\name{timestep_deterministic_SIR}
\alias{timestep_deterministic_SIR}
\title{timestep_deterministic_SIR}
\usage{
timestep_deterministic_SIR(latest, transmission.rate, recovery.rate, timestep)
}
\arguments{
\item{latest}{a data.frame containing the latest population of infecteds, susceptibles, recovereds, as well as time
(column is 'infecteds', 'susceptibles', 'recovereds' and 'time')}

\item{transmission.rate}{the transmission rate}

\item{recovery.rate}{the recovery rate}

\item{timestep}{the time interval}
}
\value{
Returns a data.frame containing the updated population of infecteds, susceptibles, recovereds, as well as time
}
\description{
Run a timestep of a deterministic SIR model
}
