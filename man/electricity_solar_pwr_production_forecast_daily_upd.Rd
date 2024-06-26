% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{electricity_solar_pwr_production_forecast_daily_upd}
\alias{electricity_solar_pwr_production_forecast_daily_upd}
\title{Solar power generation forecast - updated once a day}
\usage{
electricity_solar_pwr_production_forecast_daily_upd(
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
A data frame object with time series data having period 15 min and unit type MWh/h.
}
\description{
Solar power generation forecasts for the next day. Forecast is updated every day at 12 p.m. EET. Length of the forecast is 36 hours. Overlapping hours are overwrited.Solar forecasts are based on weather forecasts and estimates of installed PV capacity and location in Finland. Total PV capacity is based on yearly capacity statistics from the Finnish energy authority and estimates on installation rate of new capacity. Location information is a very rough estimate based on Finnish distribution grid operators information. The Data before 28.03.2024 is in hourly resolution.
}
\examples{
\dontrun{
library(finnishgrid)
start = "2024-06-01T00:00:00.000Z"  # UTC
end = "2024-06-03T00:00:00.000Z"    # UTC
key = "MY_SUPER_SECRET"
df <- electricity_solar_pwr_production_forecast_daily_upd(start_time_utc = start,
         end_time_utc = end,
         user_key = key)
summary(df)
}
}
\seealso{
https://data.fingrid.fi/en/datasets/247
}
