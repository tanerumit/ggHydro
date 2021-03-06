% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climateResponseMap.R
\name{climateResponseMap}
\alias{climateResponseMap}
\title{Generate a Climate Response Map}
\usage{
climateResponseMap(data = NULL, x.specs = list(name = NULL, label =
  NULL), y.specs = list(name = NULL, label = NULL), z.specs = list(name
  = NULL, label = NULL), z.bins = NULL, z.mid = NULL,
  color.low = "red", color.mid = "white", color.high = "blue",
  resolution = 300)
}
\arguments{
\item{data}{input dataset}

\item{x.specs}{x dimension specifications}

\item{y.specs}{y dimension specifications}

\item{z.specs}{z dimension specifications}

\item{z.bins}{z bins}

\item{z.mid}{critical threshold value}

\item{color.low}{first color}

\item{color.mid}{second color}

\item{color.high}{third color}

\item{resolution}{resolution for interpolation}
}
\description{
Generate a Climate Response Map
}
