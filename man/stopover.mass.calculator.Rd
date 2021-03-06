% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopover_mass_calculator.R
\name{stopover.mass.calculator}
\alias{stopover.mass.calculator}
\title{Stopover mass calculator}
\usage{
stopover.mass.calculator(bodyMass, fatMass, taxon, duration)
}
\arguments{
\item{bodyMass}{left-over after running function migrate}

\item{fatMass}{left-over after running function migrate}

\item{taxon}{(or order) classified into two categories (passerines and non-passerines)}

\item{duration}{number of hours spent at stop-over site. This must be an integer see example}
}
\value{
fat_mass, body_mass
}
\description{
During stop-overs birds replenish fat mass. Using simplifications
              from Lindström 1991. The implementation here is simplistic in that
              muscle mass is not restored as theory and field experiments have
              shown.
}
\examples{
stopover.mass.calculator(bodyMass = c(2.2, 3.4), fatMass = c(0.34, 0.42),
taxon = c(1,2), duration = 36L)
}
