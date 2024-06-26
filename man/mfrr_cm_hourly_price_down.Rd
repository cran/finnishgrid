% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finnishgrid.R
\name{mfrr_cm_hourly_price_down}
\alias{mfrr_cm_hourly_price_down}
\title{Balancing Capacity (mFRR), down, hourly market, price}
\usage{
mfrr_cm_hourly_price_down(
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
Data before 30.11.2022 is test-data.The marginal price of downwards balancing capacity procured from the balancing capacity market, /MW,h. Fingrid procures mFRR capacity through the balancing capacity market auction, which is held when needed. Balance service provider pledges itself to leave regulating bids on the regulation market. For that the balance service provider is entitled to capacity payment.
}
\examples{
\dontrun{
library(finnishgrid)
start = "2024-06-01T00:00:00.000Z"  # UTC
end = "2024-06-03T00:00:00.000Z"    # UTC
key = "MY_SUPER_SECRET"
df <- mfrr_cm_hourly_price_down(start_time_utc = start,
         end_time_utc = end,
         user_key = key)
summary(df)
}
}
\seealso{
https://data.fingrid.fi/en/datasets/330
}
