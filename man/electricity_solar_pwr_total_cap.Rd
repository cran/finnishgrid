% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{electricity_solar_pwr_total_cap}
\alias{electricity_solar_pwr_total_cap}
\title{Total production capacity used in the solar power forecast}
\usage{
electricity_solar_pwr_total_cap(
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
A data frame object with time series data having period 1 h and unit type 1 MW.
}
\description{
This is the total solar power production capacity used in Fingrid's solar power forecast. It is based on the small scale production statistics gathered by the Energy authority. It is also updated with estimates based on information that's provided to Fingrid.This total capacity information can be used, for example, to calculate the rate of production of solar power, by comparing it to the forecasted solar production series by Fingrid. This capacity information cannot however be considered as the official amount of solar production capacity in Finland, as it is updated manually and by using estimates.
}
\examples{
\dontrun{
library(finnishgrid)
start = "2024-06-01T00:00:00.000Z"  # UTC
end = "2024-06-03T00:00:00.000Z"    # UTC
key = "MY_SUPER_SECRET"
df <- electricity_solar_pwr_total_cap(start_time_utc = start,
         end_time_utc = end,
         user_key = key)
summary(df)
}
}
\seealso{
https://data.fingrid.fi/en/datasets/267
}
