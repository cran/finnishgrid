% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{fcr_d_hourlymarket_prices_down}
\alias{fcr_d_hourlymarket_prices_down}
\title{Frequency containment reserves for disturbances downwards regulation, hourly market prices}
\usage{
fcr_d_hourlymarket_prices_down(
  start_time_utc = NA,
  end_time_utc = NA,
  user_key = NA
)
}
\arguments{
\item{start_time_utc}{Start time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ}

\item{end_time_utc}{End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ}

\item{user_key}{Character array holding API-key.
Free from https://data.fingrid.fi/en/instructions}
}
\value{
A data frame object with time series data having period 1 h and unit type €/MW.
}
\description{
Hourly prices (/MW,h) of procured frequency containment reserve for disturbances downwards regulation (FCR-D down) in Finnish hourly market for each CET-timezone day is published previous evening at 22:45 (EET).FCR-D down is the frequency containment reserve used in the Nordic synchronous system that aims to keep the frequency below 50,5 Hz during disturbances.Hourly market is a reserve market operated by Fingrid. Procured volumes vary for each hour and price is the price of the most expensive procured bid.
}
\examples{
\dontrun{
library(finnishgrid)
start = "2024-06-01T00:00:00.000Z"  # UTC
end = "2024-06-03T00:00:00.000Z"    # UTC
key = "MY_SUPER_SECRET"
df <- fcr_d_hourlymarket_prices_down(start_time_utc = start,
         end_time_utc = end,
         user_key = key)
summary(df)
}
}
\seealso{
https://data.fingrid.fi/en/datasets/283
}
