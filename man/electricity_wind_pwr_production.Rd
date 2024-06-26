% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{electricity_wind_pwr_production}
\alias{electricity_wind_pwr_production}
\title{Wind power generation - 15 min data}
\usage{
electricity_wind_pwr_production(
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
A data frame object with time series data having period 15 min and unit type MW.
}
\description{
The values in the data have been erroneously instantaneous values. From February 21, 2024, the data are correct 15-minute averages.The older average data can be found correctly on the European transparency platform: https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/showFinnish 15 min wind power generation is a sum of measurements from wind parks supplied to Fingrid and of the estimate Fingrid makes from non-measured wind parks. Non-measured wind parks are about two percent of the production capacity. The average shown every quarter is the average of the past quarter, i.e. the value coming with a time stamp of 14:15 is the average from 14:00-14:15. The Data before 13.06.2023 is in hourly resolution.
}
\examples{
\dontrun{
library(finnishgrid)
start = "2024-06-01T00:00:00.000Z"  # UTC
end = "2024-06-03T00:00:00.000Z"    # UTC
key = "MY_SUPER_SECRET"
df <- electricity_wind_pwr_production(start_time_utc = start,
         end_time_utc = end,
         user_key = key)
summary(df)
}
}
\seealso{
https://data.fingrid.fi/en/datasets/75
}
