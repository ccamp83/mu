% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.questionnaire.R
\name{round_and_bound_scores}
\alias{round_and_bound_scores}
\title{Rounds scores to integers and constrains them within specified min and max bounds.}
\usage{
round_and_bound_scores(scores, min_val, max_val)
}
\arguments{
\item{scores}{A numeric vector of scores.}

\item{min_val}{Minimum allowable score.}

\item{max_val}{Maximum allowable score.}
}
\value{
A numeric vector of rounded and bounded scores.
}
\description{
Rounds scores to integers and constrains them within specified min and max bounds.
}
