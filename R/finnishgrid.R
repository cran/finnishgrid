# ----- HELPER API PAGINATION FUNCTION (GET_PAGE_DATA) -----
#
#' @title Returns one page of JSON data
#' @description This private function helps with JSON API pagination by
#'   returning a single page of observations at once.
#' @param page_num Integer which page to retrieve from API.
#' @param page_size Integer how many observations per page are collected.
#' @param api_number Integer related to the Fingrid Open Data API
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param api_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en.
#' @return A data frame object that contains a single page from API.
get_page_data <- function(page_num,
                          page_size,
                          api_number,
                          start_time_utc,
                          end_time_utc,
                          api_key) {
  if (is.na(page_num)) {
    stop("API paging number was not given.")
  }
  if (is.na(page_size)) {
    stop("API page size was not given.")
  }
  if (is.na(api_key)) {
    stop("The API user key was not given")
  }
  # api call using httr
  raw_df <-
    httr::GET(
      url = paste0(
        "http://data.fingrid.fi/api/datasets/",
        api_number,
        "/data"
      ),
      query = list(
        startTime = start_time_utc,
        endTime = end_time_utc,
        locale = "en",
        page = page_num,
        pageSize = page_size,
        sortBy = "startTime",
        sortOrder = "asc"
      ),
      httr::add_headers(`x-api-key` = api_key,
                        version = "Fingrid-R-CRAN-client 0.2"),
      httr::timeout(50)  # 50 secs
    )
  # some gracefullyness to the server
  Sys.sleep(1)
  # rough checks
  if (httr::http_type(raw_df) != "application/json") {
    stop("API did not return json")
  }
  if (httr::status_code(raw_df) != 200) {
    stop(
      paste0(
        "API (",
        api_number,
        ") did not succeed (",
        httr::http_status(raw_df)$message,
        ")"
      )
    )
  }
  jsonlite::fromJSON(httr::content(raw_df, "text"))
}

# ----- MAIN API CALL FUNCTION (GET_DATA) -----
#
#' @title Main logic forming the API call.
#' @description Main logic forming the API call. API key can be provided as
#'   function parameter or environment variable (in .Renviron as
#'   FINGRID_OPENDATA_API_KEY). Function parameter has precedence in case both
#'   are provided. For API spec see https://data.fingrid.fi/en/pages/api.
#' @param api_number Integer related to the Fingrid Open Data API
#' @param start_time_utc Start time in UTC with offset. Character array in
#'   ISO8601, YYYY-MM-ddTHH:mm:ssZ
#' @param end_time_utc End time in UTC with offset. Character array in ISO8601,
#'   YYYY-MM-ddTHH:mm:ssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en.
#' @param page_size Integer how many observations are in a single page. Defaults
#'   to API maximum 20000.
#' @return A data frame object that contains wanted open data.
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"
#' end = "2024-06-03T00:00:00.000Z"
#' key = "MY_SUPER_SECRET"
#' df <- get_data(api_number = 124,  # electricity consumption for Finland
#'                start_time_utc = start,
#'                end_time_utc = end,
#'                user_key = key,
#'                page_size = 20000)
#' summary(df)
#' }
#' @importFrom httr GET status_code http_type add_headers timeout
#' @importFrom jsonlite fromJSON
#' @export
get_data <- function(api_number = NA,
                     start_time_utc = NA,
                     end_time_utc = NA,
                     user_key = NA,
                     page_size = 20000) {
  if (is.na(api_number)) {
    stop("API number was not given.")
  }
  if (!is.numeric(page_size) ||
      page_size > 20000 || page_size < 0) {
    stop("page_size cannot be over 20000 due to API limits.")
  }
  if (is.na(start_time_utc)) {
    stop("UTC start time was not given. Format YYYY-MM-ddTHH:mm:ssZ")
  }
  if (is.na(end_time_utc)) {
    stop("UTC end time was not given. Format YYYY-MM-ddTHH:mm:ssZ")
  }
  api_key <- Sys.getenv('FINGRID_OPENDATA_API_KEY')
  if (is.na(user_key) && api_key == "") {
    stop("The API user key was not given")
  }
  # user provided key always overrides the environment variable
  if (!is.na(user_key))
  {
    api_key <- user_key
  }

  # first page with max 20.000 observations
  all_data <- data.frame()
  page_num <- 1
  while (TRUE) {
    tmp_df <- get_page_data(
      page_num = page_num,
      page_size = page_size,
      api_number = api_number,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      api_key = api_key
    )
    # did we get a return data frame
    if (!exists("tmp_df")) {
      break
    }
    # no more observations, all data is fetched.
    if (length(tmp_df$data) == 0) {
      break
    }
    # add new page data to the bigger frame.
    all_data <- rbind(all_data, tmp_df$data)
    # last page is over 0 but smaller than page_size, break out.
    if (nrow(tmp_df$data) < page_size) {
      break
    }
    # next page number, loop until no more new data.
    page_num <- page_num + 1
    # tell some process diagnostics to the end user.
    cat(
      'Please wait.. fetching',
      page_size,
      'more observations, total now:',
      nrow(all_data),
      '\n'
    )
  }

  # If data is empty (no data), return NULL
  if (nrow(all_data) == 0) {
    message(
      paste0(
        "No datapoints found for API number: ",
        api_number,
        "\nTimespan: ",
        start_time_utc,
        "-",
        end_time_utc,
        "\nReturning NULL for it"
      )
    )
    return(NULL)
  } else {
    all_data$start_time_utc <- strptime(all_data$startTime,
                                        format = "%Y-%m-%dT%H:%M:%OS")
    all_data$end_time_utc <- strptime(all_data$endTime,
                                      format = "%Y-%m-%dT%H:%M:%OS")
    all_data$id <- api_number
    # reorder: 1.date (start), 2.date (end), 3.value, 4.id
    all_data <- all_data[, c("start_time_utc", "end_time_utc", "value", "id")]
    return(unique(all_data))  # Duplicates are deleted as these cause problems
  }

}


# ----- API HELPER FUNCTIONS -----
#
# Convenient named functions for API calls.

#' @title Automatic Frequency Restoration Reserve, activated, down
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Hourly values can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/balancing/r2/activationAndActivatedBalancingReserves/showActivated
#'   automatic Frequency Restoration Reserve (aFRR) energy, down (MW). Value is
#'   activated average power. The amount of activated Automatic Frequency. The
#'   Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/53
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_activated_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_activated_down <- function(start_time_utc = NA,
                                end_time_utc = NA,
                                user_key = NA) {
  return(
    get_data(
      api_number = 53,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Automatic Frequency Restoration Reserve, activated, up
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Hourly values can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/balancing/r2/activationAndActivatedBalancingReserves/showActivated
#'   automatic Frequency Restoration Reserve (aFRR) energy, down (MW). Value is
#'   activated average power. The amount of activated Automatic Frequency
#'   Restoration Reserve (aFRR) is calculated based on the activation signal and
#'   reserve capacity maintained in Finland. The average shown every quarter is
#'   the average of the past quarter, i.e. the value coming with a time stamp of
#'   14:15 is the average from 14:00-14:15. The Data before 13.06.2023 is in
#'   hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/54
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_activated_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_activated_up <- function(start_time_utc = NA,
                              end_time_utc = NA,
                              user_key = NA) {
  return(
    get_data(
      api_number = 54,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Automatic Frequency Restoration Reserve, capacity, down
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Procured automatic Frequency Restoration Reserve (aFRR)
#'   capacity, down (MW)
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/2
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_capacity_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_capacity_down <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 2,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Automatic Frequency Restoration Reserve, capacity, up
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Procured automatic Frequency Restoration Reserve (aFRR)
#'   capacity, up (MW)
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/1
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_capacity_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_capacity_up <- function(start_time_utc = NA,
                             end_time_utc = NA,
                             user_key = NA) {
  return(
    get_data(
      api_number = 1,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Automatic Frequency Restoration Reserve, price, down
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Marginal price for procured automatic Frequency Restoration
#'   Reserve (aFRR) capacity for down-regulation (/MW)
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MW.
#' @seealso https://data.fingrid.fi/en/datasets/51
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_price_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_price_down <- function(start_time_utc = NA,
                            end_time_utc = NA,
                            user_key = NA) {
  return(
    get_data(
      api_number = 51,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Automatic Frequency Restoration Reserve, price, up
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Marginal price for procured upward automatic Frequency
#'   Restoration Reserve (aFRR) capacity (/MW)
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MW.
#' @seealso https://data.fingrid.fi/en/datasets/52
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- afrr_price_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
afrr_price_up <- function(start_time_utc = NA,
                          end_time_utc = NA,
                          user_key = NA) {
  return(
    get_data(
      api_number = 52,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Commercial transmission of electricity between FI-EE
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Commercial electricity flow (dayahead market and intraday
#'   market) between Finland (FI) and Estonia (EE) including system supportive
#'   trade between TSOs. Positive sign is export from Finland to Estonia.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/140
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_commercial_electricity_flow_FI_EE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_commercial_electricity_flow_FI_EE <- function(start_time_utc = NA,
                                                     end_time_utc = NA,
                                                     user_key = NA) {
  return(
    get_data(
      api_number = 140,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Commercial transmission of electricity between FI-SE1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Commercial transmission of electricity (dayahead market and
#'   intraday market) between Finland (FI) and Northern Sweden (SE1). Positive
#'   sign is export from Finland to Sweden.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/31
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_commercial_electricity_flow_FI_SE1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_commercial_electricity_flow_FI_SE1 <- function(start_time_utc = NA,
                                                      end_time_utc = NA,
                                                      user_key = NA) {
  return(
    get_data(
      api_number = 31,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Commercial transmission of electricity between FI-SE3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Commercial electricity flow (dayahead market and intraday
#'   market) between Finland (FI) and Central Sweden (SE3). Positive sign is
#'   export from Finland to Sweden.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/32
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_commercial_electricity_flow_FI_SE3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_commercial_electricity_flow_FI_SE3 <- function(start_time_utc = NA,
                                                      end_time_utc = NA,
                                                      user_key = NA) {
  return(
    get_data(
      api_number = 32,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity EE-FI - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from Estonia (EE) to Finland
#'   (FI). Transmission capacity is given hourly for every hour of the next day.
#'   Each hour is given one value. Day-ahead transmission capacity Fingrid will
#'   publish every day in the afternoon. This capacity will not changed after
#'   publication. Transmission capacity mean the capability of the electricity
#'   system to supply electricity to the market without compromising the system
#'   security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/112
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_EE_FI_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_EE_FI_official <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 112,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity FI-EE - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from Finland (FI) to Estonia
#'   (EE). Transmission capacity is given hourly for every hour of the next day.
#'   Each hour is given one value. Day-ahead transmission capacity Fingrid will
#'   publish every day in the afternoon. This capacity will not changed after
#'   publication. Transmission capacity mean the capability of the electricity
#'   system to supply electricity to the market without compromising the system
#'   security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/115
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_FI_EE_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_FI_EE_official <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 115,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity FI-SE1 - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from Finland (FI) to
#'   North-Sweden (SE1). Transmission capacity is given hourly for every hour of
#'   the next day. Each hour is given one value. Day-ahead transmission capacity
#'   Fingrid will publish every day in the afternoon. This capacity will not
#'   changed after publication. Transmission capacity mean the capability of the
#'   electricity system to supply electricity to the market without compromising
#'   the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/26
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_FI_SE1_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_FI_SE1_official <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 26,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity FI-SE1 - planned
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned day-ahead transmission capacity from Finland (FI) to
#'   North-Sweden (SE1). Transmission capacity is given hourly for every next
#'   week hour. Each week's hour is given one value. Planned weekly transmission
#'   capacity Fingrid will publish every Tuesday. Information will be updated if
#'   there are changes to the previous plan timetable or capacity. Transmission
#'   capacity mean the capability of the electricity system to supply
#'   electricity to the market without compromising the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/143
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_FI_SE1_planned(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_FI_SE1_planned <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 143,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity FI-SE3 - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from Finland (FI) to
#'   Central-Sweden (SE3). Transmission capacity is given hourly for every hour
#'   of the next day. Each hour is given one value. Day-ahead transmission
#'   capacity Fingrid will publish every day in the afternoon. This capacity
#'   will not changed after publication. Transmission capacity mean the
#'   capability of the electricity system to supply electricity to the market
#'   without compromising the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/27
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_FI_SE3_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_FI_SE3_official <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 27,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity FI-SE3 - planned
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned day-ahead transmission capacity from Finland (FI) to
#'   Central-Sweden (SE3). Transmission capacity is given hourly for every next
#'   week hour. Each week's hour is given one value. Planned weekly transmission
#'   capacity Fingrid will publish every Tuesday. Information will be updated if
#'   there are changes to the previous plan timetable or capacity. Transmission
#'   capacity mean the capability of the electricity system to supply
#'   electricity to the market without compromising the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/145
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_FI_SE3_planned(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_FI_SE3_planned <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 145,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity SE1-FI - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from North-Sweden (SE1) to
#'   Finland (FI). Transmission capacity is given hourly for every hour of the
#'   next day. Each hour is given one value. Day-ahead transmission capacity
#'   Fingrid will publish every day in the afternoon. This capacity will not
#'   changed after publication. Transmission capacity mean the capability of the
#'   electricity system to supply electricity to the market without compromising
#'   the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/24
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_SE1_FI_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_SE1_FI_official <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 24,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity SE1-FI - planned
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned day-ahead transmission capacity from North-Sweden (SE1)
#'   to Finland (FI). Transmission capacity is given hourly for every next week
#'   hour. Each week's hour is given one value. Planned weekly transmission
#'   capacity Fingrid will publish every Tuesday. Information will be updated if
#'   there are changes to the previous plan timetable or capacity. Transmission
#'   capacity mean the capability of the electricity system to supply
#'   electricity to the market without compromising the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/142
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_SE1_FI_planned(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_SE1_FI_planned <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 142,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity SE3-FI - official
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Day-ahead transmission capacity from Central-Sweden (SE3) to
#'   Finland (FI). Transmission capacity is given hourly for every hour of the
#'   next day. Each hour is given one value. Day-ahead transmission capacity
#'   Fingrid will publish every day in the afternoon. This capacity will not
#'   changed after publication. Transmission capacity mean the capability of the
#'   electricity system to supply electricity to the market without compromising
#'   the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/25
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_SE3_FI_official(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_SE3_FI_official <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 25,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Day-ahead transmission capacity SE3-FI  - planned
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned day-ahead transmission capacity from Central-Sweden
#'   (SE3) to Finland (FI). Transmission capacity is given hourly for every next
#'   week hour. Each week's hour is given one value. Planned weekly transmission
#'   capacity Fingrid will publish every Tuesday. Information will be updated if
#'   there are changes to the previous plan timetable or capacity. Transmission
#'   capacity mean the capability of the electricity system to supply
#'   electricity to the market without compromising the system security.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/144
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_da_trans_cap_SE3_FI_planned(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_da_trans_cap_SE3_FI_planned <- function(start_time_utc = NA,
                                               end_time_utc = NA,
                                               user_key = NA) {
  return(
    get_data(
      api_number = 144,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity EE-FI
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity to be given to intraday market EE - FI
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/110
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_EE_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_EE_FI <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 110,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity EE-FI - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity to be given to intraday market EE-FI.After
#'   Elspot trades have been closed, real time intraday capacity is equivalent
#'   to the allocated intraday capacity. The real time capacity is updated after
#'   each intraday trade so that it corresponds to real time situation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/111
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_EE_FI_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_EE_FI_RTD <- function(start_time_utc = NA,
                                          end_time_utc = NA,
                                          user_key = NA) {
  return(
    get_data(
      api_number = 111,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity FI-EE
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity to be given to intraday market FI-EE
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/113
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_FI_EE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_FI_EE <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 113,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity FI-EE - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity to be given to intraday market FI-EE.After
#'   Elspot trades have been closed, real time intraday capacity is equivalent
#'   to the allocated intraday capacity. The real time capacity is updated after
#'   each intraday trade so that it corresponds to real time situation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/114
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_FI_EE_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_FI_EE_RTD <- function(start_time_utc = NA,
                                          end_time_utc = NA,
                                          user_key = NA) {
  return(
    get_data(
      api_number = 114,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity FI - SE1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity for intraday market from Finland to
#'   Northern Sweden (FI - SE1). For intraday market capacity is given as free
#'   capacity after dayahead market. Capacity is published once a day and not
#'   updated.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/44
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_FI_SE1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_FI_SE1 <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 44,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity FI-SE3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity for intraday market from Finland to Mid
#'   Sweden (FI - SE3). For intraday market capacity is given as free capacity
#'   after dayahead market. Capacity is published once a day and not updated.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/45
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_FI_SE3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_FI_SE3 <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 45,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity SE1-FI
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity for intraday market from Northern Sweden
#'   to Finland (SE1-FI). For intraday market capacity is given as free capacity
#'   after dayahead market. Capacity is published once a day and not updated.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/38
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_SE1_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_SE1_FI <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 38,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Intraday transmission capacity SE3-FI
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Transmission capacity for intraday market from Mid Sweden to
#'   Finland (SE3-FI). Capacity for intraday market is given as free capacity
#'   after dayahead market. Capacity is published once a day and not updated.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/39
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_id_trans_cap_SE3_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_id_trans_cap_SE3_FI <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 39,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission of electricity between Finland and Estonia
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/transmission-domain/physicalFlow/show
#'   Measured electrical transmission between Finland and Estonia HVDC tile
#'   lines (Estlink 1 and Estlink 2). Positive sign means transmission from
#'   Finland to Estonia. Negative sign means transmission from Estonia to
#'   Finland. The value is updated once every 15 minutes after the hour shift.
#'   Each day before noon the values of the previous day are updated with more
#'   accurate measurement values. The average shown every quarter is the average
#'   of the past quarter, i.e. the value coming with a time stamp of 14:15 is
#'   the average from 14:00-14:15. The Data before 13.06.2023 is in hourly
#'   resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/55
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_measured_electricity_flow_FI_EE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_measured_electricity_flow_FI_EE <- function(start_time_utc = NA,
                                                   end_time_utc = NA,
                                                   user_key = NA) {
  return(
    get_data(
      api_number = 55,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission of electricity between Finland and Norway
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. The data will be corrected to a 15-minute average. We'll let you
#'   know when the fix is done.Average data can be found correctly on the
#'   European transparency platform:
#'   https://transparency.entsoe.eu/transmission-domain/physicalFlow/showMeasured
#'   electrical transmission between Finland and Norway 220kV tie line. Positive
#'   sign means transmission from Finland to Norway. Negative sign means
#'   transmission from Norway to Finland. The value is updated once every 15
#'   minutes after the hour shift. Each day before noon the values of the
#'   previous day are updated with more accurate measurement values. The Data
#'   before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/57
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_measured_electricity_flow_FI_NO(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_measured_electricity_flow_FI_NO <- function(start_time_utc = NA,
                                                   end_time_utc = NA,
                                                   user_key = NA) {
  return(
    get_data(
      api_number = 57,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission of electricity between Finland and Northern Sweden -
#'   measured every 15 minutes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/transmission-domain/physicalFlow/showMeasured
#'   transmission of electricity between Finland and Northern Sweden (SE1).
#'   Positive sign means transmission from Finland to Northern Sweden (SE1).
#'   Negative sign means transmission from Northern Sweden (SE1) to Finland. The
#'   average shown every quarter is the average of the past quarter, i.e. the
#'   value coming with a time stamp of 14:15 is the average from 14:00-14:15. he
#'   Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/60
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_measured_electricity_flow_FI_SE1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_measured_electricity_flow_FI_SE1 <- function(start_time_utc = NA,
                                                    end_time_utc = NA,
                                                    user_key = NA) {
  return(
    get_data(
      api_number = 60,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission of electricity between Finland and Central Sweden -
#'   measured every 15 minutes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/transmission-domain/physicalFlow/showMeasured
#'   transmission of electricity between Finland and Central Sweden (SE3).
#'   Positive sign means transmission from Finland to Central Sweden (SE3).
#'   Negative sign means transmission from Northern Sweden (SE1) to Finland. The
#'   average shown every quarter is the average of the past quarter, i.e. the
#'   value coming with a time stamp of 14:15 is the average from 14:00-14:15.
#'   The Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/61
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- border_measured_electricity_flow_FI_SE3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
border_measured_electricity_flow_FI_SE3 <- function(start_time_utc = NA,
                                                    end_time_utc = NA,
                                                    user_key = NA) {
  return(
    get_data(
      api_number = 61,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Congestion income between FI-EE
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Congestion income between Finland (FI) and Estonia (EE).
#'   __Congestion income is published on ENTSO-E's Transparency Platform, which
#'   can be founded here:
#'   https://transparency.entsoe.eu/transmission/r2/dailyImplicitAllocationsCongestionIncome/show
#'   .There are historical values to be found from Open Data until the beginning
#'   of February 2017. After February 2017 updated data as well as historical
#'   data can be founded from ENTSO-E's Transparency Platform.__Congestion
#'   income is calculated as follows:congestion income (/h) = commercial flow on
#'   day ahead market (MW) * area price difference (/MWh)Congestion originates
#'   in the situation where transmission capacity between bidding zones is not
#'   sufficient to fulfill the market demand and the congestion splits the
#'   bidding zones into separate price areas. Congestion income arises from the
#'   different prices that the sellers receive and the buyers pay when
#'   electricity flows from the higher price area to the lower price area. The
#'   power exchange receives the difference, which it then pays to the
#'   Transmission System Operators (TSOs). The TSOs spend the received
#'   congestion income on increasing the transmission capacity on its
#'   cross-border interconnectors according to the EU regulation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR.
#' @seealso https://data.fingrid.fi/en/datasets/48
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_EE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_EE <- function(start_time_utc = NA,
                                    end_time_utc = NA,
                                    user_key = NA) {
  return(
    get_data(
      api_number = 48,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Congestion income between FI-SE1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Congestion income between Finland (FI) and Northern Sweden
#'   (SE1). __Congestion income is published on ENTSO-E's Transparency Platform,
#'   which can be founded here:
#'   https://transparency.entsoe.eu/transmission/r2/dailyImplicitAllocationsCongestionIncome/show
#'   .There are historical values to be found from Open Data until the beginning
#'   of February 2017. After February 2017 updated data as well as historical
#'   data can be founded from ENTSO-E's Transparency Platform.__Congestion
#'   income is calculated as follows:congestion income (/h) = commercial flow on
#'   day ahead market (MW) * area price difference (/MWh)Congestion originates
#'   in the situation where transmission capacity between bidding zones is not
#'   sufficient to fulfill the market demand and the congestion splits the
#'   bidding zones into separate price areas. Congestion income arises from the
#'   different prices that the sellers receive and the buyers pay when
#'   electricity flows from the higher price area to the lower price area. The
#'   seller acting in a lower price area receives lower price for electricity
#'   compared to the price the other party pays for electricity in the higher
#'   price area, and the power exchange receives surplus income, which it then
#'   pays to the Transmission System Operators (TSOs). The TSOs spend the
#'   received congestion income on increasing the transmission capacity on its
#'   cross-border interconnectors according to the EU regulation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR.
#' @seealso https://data.fingrid.fi/en/datasets/70
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_SE1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_SE1 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 70,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Congestion income between FI-SE3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Congestion income between Finland (FI) and Central Sweden (SE3).
#'   __Congestion income is published on ENTSO-E's Transparency Platform, which
#'   can be founded here:
#'   https://transparency.entsoe.eu/transmission/r2/dailyImplicitAllocationsCongestionIncome/show
#'   .There are historical values to be found from Open Data until the beginning
#'   of February 2017. After February 2017 updated data as well as historical
#'   data can be founded from ENTSO-E's Transparency Platform.__Congestion
#'   income = commercial flow between FI and SE3 on the day ahead market (MWh/h)
#'   * absolute value of price difference between FI and SE3 (/MWh).Congestion
#'   originates in the situation where transmission capacity between bidding
#'   zones is not sufficient to fulfill the market demand and the congestion
#'   splits the bidding zones into separate price areas. Congestion income
#'   arises from the different prices that the sellers receive and the buyers
#'   pay when electricity flows from the higher price area to the lower price
#'   area. The seller acting in a lower price area receives lower price for
#'   electricity compared to the price the other party pays for electricity in
#'   the higher price area, and the power exchange receives surplus income,
#'   which it then pays to the Transmission System Operators (TSOs). The TSOs
#'   spend the received congestion income on increasing the transmission
#'   capacity on its cross-border interconnectors according to the EU
#'   regulation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR.
#' @seealso https://data.fingrid.fi/en/datasets/71
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- congestion_income_FI_SE3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
congestion_income_FI_SE3 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 71,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity consumption in Finland
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/showElectricity
#'   consumption in Finland is based on Fingrid's production measurements. Minor
#'   part of production which is not measured is estimated. The consumption is
#'   calculated as follows:Consumption = Production + Import - Export. The
#'   average shown every quarter is the average of the past quarter, i.e. the
#'   value coming with a time stamp of 14:15 is the average from 14:00-14:15.
#'   The Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/124
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_FI <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 124,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity consumption forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Electricity consumption forecast of Finland. The forecast is
#'   made by Fingrid. The Data before 21.04.2024 is in 5 minute resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/166
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_forecast_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_forecast_FI <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 166,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity consumption forecast - next 24 hours
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description A consumption forecast for the next 24 hours made by Fingrid.
#'   Forecast is published on previous day at 12:00 EET. The Data before
#'   21.04.2024 is in 5 minute resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/165
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_consumption_forecast_FI_DA(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_consumption_forecast_FI_DA <- function(start_time_utc = NA,
                                                   end_time_utc = NA,
                                                   user_key = NA) {
  return(
    get_data(
      api_number = 165,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production in Finland
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/show
#'   Electricity production in Finland are based on Fingrid's measurements.
#'   Minor part of production which is not measured is estimated.The average
#'   shown every quarter is the average of the past quarter, i.e. the value
#'   coming with a time stamp of 14:15 is the average from 14:00-14:15. The Data
#'   before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/74
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_FI <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 74,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production prediction - premilinary
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description 15 minutes electricity generation forecast is based on the
#'   production plans that balance responsible parties have reported to Fingrid.
#'   The forecast is published daily by 6.00 pm for the next day, and it is not
#'   updated to match the updated production plans that balance responsible
#'   parties send to Fingrid. The Data before 10.06.2023 is in hourly
#'   resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/242
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_forecast_FI_DA(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_forecast_FI_DA <- function(start_time_utc = NA,
                                                  end_time_utc = NA,
                                                  user_key = NA) {
  return(
    get_data(
      api_number = 242,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production prediction - updated every 15 minutes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The calculation of production forecast in Finland is based on
#'   the production plans that balance responsible parties has reported to
#'   Fingrid. Production forecast is updated every 15 minutes. The Data before
#'   03.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/241
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_production_forecast15_FI_DA(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_production_forecast15_FI_DA <- function(start_time_utc = NA,
                                                    end_time_utc = NA,
                                                    user_key = NA) {
  return(
    get_data(
      api_number = 241,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Solar power generation forecast - updated once a day
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Solar power generation forecasts for the next day. Forecast is
#'   updated every day at 12 p.m. EET. Length of the forecast is 36 hours.
#'   Overlapping hours are overwrited.Solar forecasts are based on weather
#'   forecasts and estimates of installed PV capacity and location in Finland.
#'   Total PV capacity is based on yearly capacity statistics from the Finnish
#'   energy authority and estimates on installation rate of new capacity.
#'   Location information is a very rough estimate based on Finnish distribution
#'   grid operators information. The Data before 28.03.2024 is in hourly
#'   resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/247
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_solar_pwr_production_forecast_daily_upd(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_solar_pwr_production_forecast_daily_upd <- function(start_time_utc = NA,
                                                                end_time_utc = NA,
                                                                user_key = NA) {
  return(
    get_data(
      api_number = 247,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Solar power generation forecast - updated every 15 minutes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Solar power generation forecast for the next 36 hours. Updated
#'   every 15 minutes. Solar forecasts are based on weather forecasts and
#'   estimates of installed PV capacity and location in Finland. Total PV
#'   capacity is based on yearly capacity statistics from the Finnish energy
#'   authority and estimates on installation rate of new capacity. Location
#'   information is a very rough estimate based on Finnish distribution grid
#'   operators information. The Data before 31.05.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/248
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_solar_pwr_production_forecast_quart_upd(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_solar_pwr_production_forecast_quart_upd <- function(start_time_utc = NA,
                                                                end_time_utc = NA,
                                                                user_key = NA) {
  return(
    get_data(
      api_number = 248,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Total production capacity used in the solar power forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description This is the total solar power production capacity used in
#'   Fingrid's solar power forecast. It is based on the small scale production
#'   statistics gathered by the Energy authority. It is also updated with
#'   estimates based on information that's provided to Fingrid.This total
#'   capacity information can be used, for example, to calculate the rate of
#'   production of solar power, by comparing it to the forecasted solar
#'   production series by Fingrid. This capacity information cannot however be
#'   considered as the official amount of solar production capacity in Finland,
#'   as it is updated manually and by using estimates.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 MW.
#' @seealso https://data.fingrid.fi/en/datasets/267
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_solar_pwr_total_cap(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_solar_pwr_total_cap <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 267,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Wind power generation - 15 min data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute averages.The
#'   older average data can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/showFinnish
#'   15 min wind power generation is a sum of measurements from wind parks
#'   supplied to Fingrid and of the estimate Fingrid makes from non-measured
#'   wind parks. Non-measured wind parks are about two percent of the production
#'   capacity. The average shown every quarter is the average of the past
#'   quarter, i.e. the value coming with a time stamp of 14:15 is the average
#'   from 14:00-14:15. The Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/75
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_wind_pwr_production(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_wind_pwr_production <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 75,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Wind power generation forecast - updated once a day
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Finnish wind power generation forecasts for the next day.
#'   Forecast is updated every day at 12 p.m. EET. Length of the forecast is 36
#'   hours. Overlapping hours are overwritten.The forecast is based on weather
#'   forecasts and data about the location, size and capacity of wind turbines.
#'   The weather data sourced from multiple providers. The Data before
#'   28.03.2024 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/246
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_wind_pwr_production_forecast_daily_upd(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_wind_pwr_production_forecast_daily_upd <- function(start_time_utc = NA,
                                                               end_time_utc = NA,
                                                               user_key = NA) {
  return(
    get_data(
      api_number = 246,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Wind power generation forecast - updated every 15 minutes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Finnish wind power generation forecast for the next 36 hours.
#'   Updated every 15 minutes. The forecast is based on weather forecasts and
#'   data about the location, size and capacity of wind turbines. The weather
#'   data sourced from multiple providers. The Data before 31.05.2023 is in
#'   hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/245
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_wind_pwr_production_forecast_quart_upd(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_wind_pwr_production_forecast_quart_upd <- function(start_time_utc = NA,
                                                               end_time_utc = NA,
                                                               user_key = NA) {
  return(
    get_data(
      api_number = 245,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Total production capacity used in the wind power forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description This is the total wind production capacity used in Fingrid's
#'   wind power forecast. It is based capacity information gathered by
#'   Fingrid.This total capacity information can be used, for example, to
#'   calculate the rate of production of wind power, by comparing it to the
#'   actual wind production series by Fingrid. This capacity information cannot
#'   however be considered as the official amount of wind production capacity in
#'   Finland, as it is updated manually.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 MW.
#' @seealso https://data.fingrid.fi/en/datasets/268
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- electricity_wind_pwr_total_cap(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
electricity_wind_pwr_total_cap <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 268,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Emission factor for electricity consumed in Finland - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Estimate of carbon dioxide of produced electricity, which is
#'   consumed in Finland. The emissions are estimated by taking FInland's
#'   electricity production, electricity import as well as electricity export
#'   into account. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type gCO2/kWh.
#' @seealso https://data.fingrid.fi/en/datasets/265
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- esg_emission_factor_elec_consumption_FI_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
esg_emission_factor_elec_consumption_FI_RTD <- function(start_time_utc = NA,
                                                        end_time_utc = NA,
                                                        user_key = NA) {
  return(
    get_data(
      api_number = 265,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Emission factor of electricity production in Finland - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Near in real time calculated carbon dioxide emission estimate of
#'   electricity production in Finland. The emissions are estimated by summing
#'   each product of different electricity production type and their emission
#'   factor together, and by dividing the sum by Finland's total electricity
#'   production. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type gCO2/kWh.
#' @seealso https://data.fingrid.fi/en/datasets/266
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- esg_emission_factor_elec_production_FI_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
esg_emission_factor_elec_production_FI_RTD <- function(start_time_utc = NA,
                                                       end_time_utc = NA,
                                                       user_key = NA) {
  return(
    get_data(
      api_number = 266,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserve for disturbances downwards regulation,
#'   received bids in hourly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of received frequency containment reserve for
#'   disturbances downwards regulation (FCR-D down) bids. The volume of bids
#'   will be published 22:00 (EET) on previous evening.FCR-D downwards
#'   regulation is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency below 50,5 Hz during
#'   disturbances.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/282
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_bidsum_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_bidsum_down <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 282,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment  reserve for disturbances upwards regulation,
#'   received bids in hourly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of received frequency containment reserve for
#'   disturbances upwards regulation (FCR-D up) bids. The volume of bids will be
#'   published 22:45 (EET) on previous evening.FCR-D (up) is the frequency
#'   containment reserve used in the Nordic synchronous system that aims to keep
#'   the frequency above 49,5 Hz during disturbances.Hourly market is a reserve
#'   market operated by Fingrid. Procured volumes vary for each hour and price
#'   is the price of the most expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/286
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_bidsum_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_bidsum_up <- function(start_time_utc = NA,
                                         end_time_utc = NA,
                                         user_key = NA) {
  return(
    get_data(
      api_number = 286,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances downwards regulation,
#'   hourly market prices
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly prices (/MW,h) of procured frequency containment reserve
#'   for disturbances downwards regulation (FCR-D down) in Finnish hourly market
#'   for each CET-timezone day is published previous evening at 22:45
#'   (EET).FCR-D down is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency below 50,5 Hz during
#'   disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MW.
#' @seealso https://data.fingrid.fi/en/datasets/283
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_prices_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_prices_down <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 283,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances upwards regulation,
#'   hourly market prices
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly prices (/MW,h) of procured frequency containment reserve
#'   for disturbances upwards regulation (FCR-D up) in Finnish hourly market for
#'   each CET-timezone day is published previous evening at 22:45 (EET).FCR-D
#'   (up) is the frequency containment reserve used in the Nordic synchronous
#'   system that aims to keep the frequency above 49,5 Hz during
#'   disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MW.
#' @seealso https://data.fingrid.fi/en/datasets/318
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_prices_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_prices_up <- function(start_time_utc = NA,
                                         end_time_utc = NA,
                                         user_key = NA) {
  return(
    get_data(
      api_number = 318,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserve for disturbances downwards regulation,
#'   procured volumes in hourly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly volume of procured frequency containment reserve for
#'   disturbances downwards regulation (FCR-D down) in Finnish hourly market for
#'   each CET-timezone day is published previous evening at 22:45 (EET).FCR-D
#'   downwards regulation is the frequency containment reserve used in the
#'   Nordic synchronous system that aims to keep the frequency below 50,5 Hz
#'   during disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/281
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_procured_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_procured_down <- function(start_time_utc = NA,
                                             end_time_utc = NA,
                                             user_key = NA) {
  return(
    get_data(
      api_number = 281,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserve for disturbances upwards regulation,
#'   procured volumes in hourly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly volume of procured frequency containment reserve for
#'   disturbances upwards regulation (FCR-D up) in Finnish hourly market for
#'   each CET-timezone day is published previous evening at 22:45 (EET).FCR-D
#'   (up) is the frequency containment reserve used in the Nordic synchronous
#'   system that aims to keep the frequency above 49,5 Hz during
#'   disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/315
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_hourlymarket_procured_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_hourlymarket_procured_up <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 315,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances downward regulation,
#'   nordic trade
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of the nordic trade of frequency containment reserve
#'   for disturbances downward regulation (FCR-D down) capacity. Positive
#'   numbers indicate import of capacity to Finland and negative numbers
#'   indicate export of capacity from Finland. The data contains the traded
#'   capacity for Sweden and Norway. The data will be published 22:45 (EET) on
#'   previous evening.FCR-D down is the frequency containment reserve used in
#'   the Nordic synchronous system that aims to keep the frequency below 50,5 Hz
#'   during disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/320
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_nordictrade_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_nordictrade_down <- function(start_time_utc = NA,
                                   end_time_utc = NA,
                                   user_key = NA) {
  return(
    get_data(
      api_number = 320,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances upwards regulation,
#'   nordic trade
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of the nordic trade of frequency containment reserve
#'   for disturbances upwards regulation (FCR-D up) capacity. Positive numbers
#'   indicate import of capacity to Finland and negative numbers indicate export
#'   of capacity from Finland. The data contains the traded capacity for Sweden
#'   and Norway. The data will be published 22:45 (EET) on previous
#'   evening.FCR-D (up) is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency above 49,5 Hz during
#'   disturbances.Hourly market is a reserve market operated by Fingrid.
#'   Procured volumes vary for each hour and price is the price of the most
#'   expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/289
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_nordictrade_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_nordictrade_up <- function(start_time_utc = NA,
                                 end_time_utc = NA,
                                 user_key = NA) {
  return(
    get_data(
      api_number = 289,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances downward regulation,
#'   reserve plans in the yearly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The hourly sum of reserve plans for frequency containment
#'   reserve for disturbances downwards regulation (FCR-D down) in the yearly
#'   market. The data will be published 22:45 (EET) on previous evening.FCR-D
#'   downwards regulation is the frequency containment reserve used in the
#'   Nordic synchronous system that aims to keep the frequency below 50,5 Hz
#'   during disturbances.Yearly market is a reserve market operated by Fingrid.
#'   Hourly procured volumes vary according to the reserve plans submitted by
#'   the balancing service providers and the price is constant over the whole
#'   year.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/321
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_yearlymarket_plans_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_yearlymarket_plans_down <- function(start_time_utc = NA,
                                          end_time_utc = NA,
                                          user_key = NA) {
  return(
    get_data(
      api_number = 321,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency containment reserves for disturbances upwards regulation,
#'   reserve plans in the yearly market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The hourly sum of reserve plans for frequency containment
#'   reserve for disturbances upwards regulation (FCR-D up) in the yearly
#'   market. The data will be published 22:45 (EET) on previous evening.FCR-D
#'   (up) is the frequency containment reserve used in the Nordic synchronous
#'   system that aims to keep the frequency above 49,5 Hz during
#'   disturbances.Yearly market is a reserve market operated by Fingrid. Hourly
#'   procured volumes vary according to the reserve plans submitted by the
#'   balancing service providers and the price is constant over the whole year.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/290
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_d_yearlymarket_plans_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_d_yearlymarket_plans_up <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 290,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, activated
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The values in the data have been erroneously instantaneous
#'   values. From February 21, 2024, the data are correct 15-minute
#'   averages.Hourly values can be found correctly on the European transparency
#'   platform:
#'   https://transparency.entsoe.eu/balancing/r2/activationAndActivatedBalancingReserves/show
#'   Activated Frequency Containment Reserve for Normal operation (FCR-N) is
#'   published one hour after the hour in question, for example the value for
#'   hour 07-08 is published at 9 o'clock. FCR-N is the frequency containment
#'   reserve used in the Nordic synchronous system that aims to keep the
#'   frequency in normal frequency range between 49,9 - 50,1 Hz. Activated FCR-N
#'   volume (MWh) is calculated on the basis of the frequency in the Nordic
#'   synchronous system and maintained Finnish FCR-N capacity. Value is
#'   activated net average power. Positive value means that the frequency has
#'   been in average below 50,0 Hz during the hour, and reserve has been
#'   activated as up-regulation. Respectively, negative value means that the
#'   frequency has been in average above 50,0 Hz, and reserve has been activated
#'   as down-regulation. The Data before 13.06.2023 is in hourly resolution.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/123
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_activated(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_activated <- function(start_time_utc = NA,
                            end_time_utc = NA,
                            user_key = NA) {
  return(
    get_data(
      api_number = 123,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, activated
#'   down-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description FCR-N is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency in normal frequency
#'   range between 49,9 - 50,1 Hz. Activated FCR-N volume down (MW) is
#'   calculated on the basis of the frequency in the Nordic synchronous system
#'   and maintained Finnish FCR-N capacity. Value is the average activated
#'   down-regulation power. Negative value means that the frequency has been
#'   above 50,0 Hz, and reserve has been activated as down-regulation.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/343
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_activated_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_activated_down <- function(start_time_utc = NA,
                                 end_time_utc = NA,
                                 user_key = NA) {
  return(
    get_data(
      api_number = 343,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, activated
#'   up-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description FCR-N is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency in normal frequency
#'   range between 49,9 - 50,1 Hz. Activated FCR-N volume up (MW) is calculated
#'   on the basis of the frequency in the Nordic synchronous system and
#'   maintained Finnish FCR-N capacity. Value is the average activated
#'   up-regulation power. Positive value means that the frequency has been below
#'   50,0 Hz during the quarter, and reserve has been activated as
#'   up-regulation.
#' @return A data frame object with time series data having period 15 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/344
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_activated_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_activated_up <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 344,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, foreign trade
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of the foreign trade of frequency containment reserve
#'   for normal operation (FCR-N) capacity. Positive numbers indicate import of
#'   capacity to Finland and negative numbers indicate export of capacity from
#'   Finland. The data contains the traded capacity for Sweden, Norway, Estonia
#'   and Russia*. The data will be published 22:45 (EET) on previous
#'   evening.FCR-N is the frequency containment reserve used in the Nordic
#'   synchronous system that aims to keep the frequency in normal frequency
#'   range between 49,9 - 50,1 Hz.Hourly market is a reserve market operated by
#'   Fingrid. Procured volumes vary for each hour and price is the price of the
#'   most expensive procured bid.*Procuring reserves from Russia has ended
#'   14.5.2022
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/287
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_foreign_trade(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_foreign_trade <- function(start_time_utc = NA,
                                end_time_utc = NA,
                                user_key = NA) {
  return(
    get_data(
      api_number = 287,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, hourly market bids
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of received Frequency Containment Reserves for Normal
#'   operation (FCR-N) bids. The volume of bids will be published 22:45 (EET) on
#'   previous evening.FCR-N is the frequency containment reserve used in the
#'   Nordic synchronous system that aims to keep the frequency in normal
#'   frequency range between 49,9 - 50,1 Hz.Hourly market is a reserve market
#'   operated by Fingrid. Procured volumes vary for each hour and price is the
#'   price of the most expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/285
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_hourlymarket_bidsum(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_hourlymarket_bidsum <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 285,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, hourly market
#'   prices
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly prices (/MW,h) of procured frequency containment reserve
#'   for normal operation (FCR-N) in Finnish hourly market for each CET-timezone
#'   day is published previous evening at 22:45 (EET). FCR-N is the frequency
#'   containment reserve used in the Nordic synchronous system that aims to keep
#'   the frequency in normal frequency range between 49,9 - 50,1 Hz. Hourly
#'   market is a reserve market operated by Fingrid. Procured volumes vary for
#'   each hour and price is the price of the most expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MW.
#' @seealso https://data.fingrid.fi/en/datasets/317
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_hourlymarket_prices(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_hourlymarket_prices <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 317,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, hourly market
#'   volumes
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hourly volume of procured frequency containment reserve for
#'   normal operation (FCR-N) in Finnish hourly market for each CET-timezone day
#'   is published previous evening at 22:45 (EET).FCR-N is the frequency
#'   containment reserve used in the Nordic synchronous system that aims to keep
#'   the frequency in normal frequency range between 49,9 - 50,1 Hz. Hourly
#'   market is a reserve market operated by Fingrid. Procured volumes vary for
#'   each hour and price is the price of the most expensive procured bid.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/316
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_hourlymarket_volumes(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_hourlymarket_volumes <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 316,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency Containment Reserve for Normal operation, yearly market
#'   plans
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The hourly sum of reserve plans for frequency containment
#'   reserve for normal operation (FCR-N) in the yearly market. The data will be
#'   published 22:45 (EET) on previous evening.FCR-N is the frequency
#'   containment reserve used in the Nordic synchronous system that aims to keep
#'   the frequency in normal frequency range between 49,9 - 50,1 Hz.Yearly
#'   market is a reserve market operated by Fingrid. Hourly procured volumes
#'   vary according to the reserve plans submitted by the balancing service
#'   providers and the price is constant over the whole year.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/288
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- fcr_n_yearlymarket_plans(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
fcr_n_yearlymarket_plans <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 288,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Fast Frequency Reserve FFR, price
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The price of procured Fast Frequency Reserve (FFR) (/MW). The
#'   price will be published 22:00 (EET) on previous evening. The price is
#'   determined by the price of the most expensive procured bid (marginal
#'   pricing).The Fast Frequency Reserve (FFR) is procured to handle low-inertia
#'   situations. The needed volume of Fast Frequency Reserve depends on the
#'   amount of inertia in the power system and the size of the reference
#'   incident.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MW.
#' @seealso https://data.fingrid.fi/en/datasets/277
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- ffr_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
ffr_price <- function(start_time_utc = NA,
                      end_time_utc = NA,
                      user_key = NA) {
  return(
    get_data(
      api_number = 277,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Fast Frequency Reserve FFR, procured volume
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of procured Fast Frequency Reserve (FFR). The
#'   procured volume will be published 22:00 (EET) on previous evening.The Fast
#'   Frequency Reserve (FFR) is procured to handle low-inertia situations. The
#'   needed volume of Fast Frequency Reserve depends on the amount of inertia in
#'   the power system and the size of the reference incident.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 MW.
#' @seealso https://data.fingrid.fi/en/datasets/276
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- ffr_procured(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
ffr_procured <- function(start_time_utc = NA,
                         end_time_utc = NA,
                         user_key = NA) {
  return(
    get_data(
      api_number = 276,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Fast Frequency Reserve FFR, procurement forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The procurement prognosis for Fast Frequency Reserve (FFR) (MW).
#'   Fingrid procures FFR based on the procurement prognosis. The prognosis is
#'   updated once a day, typically at 11:00 (EET).The Fast Frequency Reserve
#'   (FFR) is procured to handle low-inertia situations. The needed volume of
#'   Fast Frequency Reserve depends on the amount of inertia in the power system
#'   and the size of the reference incident.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/278
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- ffr_procurement_forecast(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
ffr_procurement_forecast <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 278,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Fast Frequency Reserve FFR, received bids
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of received Fast Frequency Reserve (FFR) bids. The
#'   volume of bids will be published 22:00 (EET) on previous evening.The Fast
#'   Frequency Reserve (FFR) is procured to handle low-inertia situations. The
#'   needed volume of Fast Frequency Reserve depends on the amount of inertia in
#'   the power system and the size of the reference incident.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/275
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- ffr_received_bids(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
ffr_received_bids <- function(start_time_utc = NA,
                              end_time_utc = NA,
                              user_key = NA) {
  return(
    get_data(
      api_number = 275,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title The price of comsumption imbalance electricity
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The price of consumption imbalance power is the price for which
#'   Fingrid both purchases imbalance power from a balance responsible party and
#'   sells it to one. In the case of regulating hour, the regulation price is
#'   used. If no regulation has been made, the Elspot FIN price is used as the
#'   purchase and selling price of consumption imbalance power. Data gathering
#'   to Excel-sheet or XML format is possible in periods not longer that one
#'   year due to limitations in data transmission. Separate consumption
#'   imbalance ended when 1.11.2021 01.00 settlement model was changed to single
#'   imbalance.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/92
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_consumption_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_consumption_price <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 92,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) DK1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) DK1More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/303
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_DK1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_DK1 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 303,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) DK2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) DK2More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/304
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_DK2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_DK2 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 304,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) FI
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) FIMore information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/305
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_FI(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_FI <- function(start_time_utc = NA,
                                    end_time_utc = NA,
                                    user_key = NA) {
  return(
    get_data(
      api_number = 305,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) NO1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) NO1More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). VoAA
#'   and IC are not implemented in Norway as in the other Nordic countries, but
#'   corresponding values are published for informational purposes.In case VoAA
#'   and IC cannot be calculated according to the set rules, the value 99 999
#'   will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/306
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_NO1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_NO1 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 306,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) NO2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) NO2More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). VoAA
#'   and IC are not implemented in Norway as in the other Nordic countries, but
#'   corresponding values are published for informational purposes. In case VoAA
#'   and IC cannot be calculated according to the set rules, the value 99 999
#'   will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/307
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_NO2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_NO2 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 307,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) NO3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) NO3More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). VoAA
#'   and IC are not implemented in Norway as in the other Nordic countries, but
#'   corresponding values are published for informational purposes. In case VoAA
#'   and IC cannot be calculated according to the set rules, the value 99 999
#'   will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/308
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_NO3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_NO3 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 308,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) NO4
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) NO4More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). VoAA
#'   and IC are not implemented in Norway as in the other Nordic countries, but
#'   corresponding values are published for informational purposes. In case VoAA
#'   and IC cannot be calculated according to the set rules, the value 99 999
#'   will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/309
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_NO4(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_NO4 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 309,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) NO5
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) NO5More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). VoAA
#'   and IC are not implemented in Norway as in the other Nordic countries, but
#'   corresponding values are published for informational purposes. In case VoAA
#'   and IC cannot be calculated according to the set rules, the value 99 999
#'   will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/310
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_NO5(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_NO5 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 310,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) SE1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) SE1More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/311
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_SE1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_SE1 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 311,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC)SE2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC)SE2More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/312
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_SE2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_SE2 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 312,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising Component (IC) SE3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) SE3More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/313
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_SE3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_SE3 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 313,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Incentivising  Component (IC) SE4
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Incentivising Component (IC) SE4More information about VoAA and
#'   IC can be found in eSett Handbook (definitions and calculation rules). In
#'   case VoAA and IC cannot be calculated according to the set rules, the value
#'   99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/314
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_incentcomp_SE4(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_incentcomp_SE4 <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 314,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Imbalance power between Finland and Sweden
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The volume of power equals to the difference between measured
#'   and commercial transmission between Finland and Sweden. The tradetypes of
#'   commercial flow include day ahead, intraday and trades between Fingrid and
#'   Svenska Kraftnät during the operational hour.When the value of imbalance
#'   power volume is positive Fingrid has sold imbalance power to Sweden. When
#'   the value of imbalance power volume is negative Fingrid has bought
#'   imbalance power from Sweden.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/176
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_power_FI_SE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_power_FI_SE <- function(start_time_utc = NA,
                                  end_time_utc = NA,
                                  user_key = NA) {
  return(
    get_data(
      api_number = 176,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Imbalance price
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Imbalance price for balance responsible party's imbalance in
#'   Single price-single position settlement from 1.11.2021 01.00. Prices are
#'   updated hourly.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/319
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_price <- function(start_time_utc = NA,
                            end_time_utc = NA,
                            user_key = NA) {
  return(
    get_data(
      api_number = 319,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title The buying price of production imbalance electricity
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The down-regulating price of the hour is the price of production
#'   imbalance power purchased by Fingrid from a balance responsible party. If
#'   no down-regulation has been made or if the hour has been defined as an
#'   up-regulation hour, the Elspot FIN price is used as the purchase price of
#'   production imbalance power. Separate production balance ended when
#'   1.11.2021 01.00 setllement model was changed to single imbalance.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/96
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_production_purchase_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_production_purchase_price <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 96,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title The sales price of production imbalance electricity
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The up-regulating price of the hour is the price of production
#'   imbalance power sold by Fingrid to a balance responsible party. If no up
#'   regulation has been made or if the hour has been defined as a
#'   down-regulation hour, the day ahead spot price of Finland is used as the
#'   selling price of production imbalance power. Separate production balance
#'   ended when 1.11.2021 01.00 settlement model was changed to single
#'   imbalance.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/93
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_production_sales_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_production_sales_price <- function(start_time_utc = NA,
                                             end_time_utc = NA,
                                             user_key = NA) {
  return(
    get_data(
      api_number = 93,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) DK1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) DK1More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/291
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_dk1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_dk1 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 291,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) DK2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) DK2More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/292
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_dk2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_dk2 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 292,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) FI
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) FIMore information about VoAA
#'   and IC can be found in eSett Handbook (definitions and calculation rules).
#'   In case VoAA and IC cannot be calculated according to the set rules, the
#'   value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/293
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_fi(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_fi <- function(start_time_utc = NA,
                              end_time_utc = NA,
                              user_key = NA) {
  return(
    get_data(
      api_number = 293,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) NO1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) NO1More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). VoAA and IC are not implemented in Norway as in the other Nordic
#'   countries, but corresponding values are published for informational
#'   purposes. In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/294
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_no1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_no1 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 294,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) NO2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) NO2More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). VoAA and IC are not implemented in Norway as in the other Nordic
#'   countries, but corresponding values are published for informational
#'   purposes. In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/295
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_no2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_no2 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 295,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) NO3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) NO3More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). VoAA and IC are not implemented in Norway as in the other Nordic
#'   countries, but corresponding values are published for informational
#'   purposes. In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/296
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_no3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_no3 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 296,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) NO4
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) NO4More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). VoAA and IC are not implemented in Norway as in the other Nordic
#'   countries, but corresponding values are published for informational
#'   purposes. In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/297
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_no4(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_no4 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 297,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) NO5
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) NO5More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). VoAA and IC are not implemented in Norway as in the other Nordic
#'   countries, but corresponding values are published for informational
#'   purposes.In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/298
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_no5(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_no5 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 298,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) SE1
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) SE1More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/299
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_se1(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_se1 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 299,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) SE2
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) SE2More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/300
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_se2(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_se2 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 300,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) SE3
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) SE3More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/301
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_se3(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_se3 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 301,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Value of Avoided Activation (VoAA) SE4
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Value of Avoided Activation (VoAA) SE4More information about
#'   VoAA and IC can be found in eSett Handbook (definitions and calculation
#'   rules). In case VoAA and IC cannot be calculated according to the set
#'   rules, the value 99 999 will be displayed for both.
#' @return A data frame object with time series data having period 1 h and unit
#'   type 1 €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/302
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- imbalance_voaa_se4(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
imbalance_voaa_se4 <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 302,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), down, hourly market, bids
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The amount of downwards
#'   balancing capacity bids in the balancing capacity market, MW/h. Fingrid
#'   procures mFRR capacity through the balancing capacity market, which is held
#'   when needed. Balance service provider pledges itself to leave regulating
#'   bids on the regulation market. For that the balance service provider is
#'   entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/331
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_bids_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_bids_down <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 331,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), up, hourly market, bids
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The amount of upwards
#'   balancing capacity bids in the balancing capacity market, MW/h. Fingrid
#'   procures mFRR capacity through the balancing capacity market, which is held
#'   when needed. Balance service provider pledges itself to leave regulating
#'   bids on the regulation market. For that the balance service provider is
#'   entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/332
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_bids_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_bids_up <- function(start_time_utc = NA,
                                   end_time_utc = NA,
                                   user_key = NA) {
  return(
    get_data(
      api_number = 332,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), down, hourly market, price
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The marginal price of
#'   downwards balancing capacity procured from the balancing capacity market,
#'   /MW,h. Fingrid procures mFRR capacity through the balancing capacity market
#'   auction, which is held when needed. Balance service provider pledges itself
#'   to leave regulating bids on the regulation market. For that the balance
#'   service provider is entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MW.
#' @seealso https://data.fingrid.fi/en/datasets/330
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_price_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_price_down <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 330,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity Market (mFRR), up, hourly market, price
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The marginal price of
#'   upwards balancing capacity procured from the balancing capacity market,
#'   /MW,h. Fingrid procures mFRR capacity through the balancing capacity market
#'   auction, which is held when needed. Balance service provider pledges itself
#'   to leave regulating bids on the regulation market. For that the balance
#'   service provider is entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MW.
#' @seealso https://data.fingrid.fi/en/datasets/329
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_price_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_price_up <- function(start_time_utc = NA,
                                    end_time_utc = NA,
                                    user_key = NA) {
  return(
    get_data(
      api_number = 329,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), down, hourly market, procurement forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The forecasted amount of
#'   downwards balancing capacity procurement (MW/h).Fingrid procures mFRR
#'   capacity through the balancing capacity market, which is held when needed.
#'   Balance service provider pledges itself to leave regulating bids on the
#'   regulation market. For that the balance service provider is entitled to
#'   capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/335
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_procuforecast_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_procuforecast_down <- function(start_time_utc = NA,
                                              end_time_utc = NA,
                                              user_key = NA) {
  return(
    get_data(
      api_number = 335,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), up, hourly market, procurement forecast
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The forecasted amount of
#'   upwards balancing capacity procurement (MW/h). Fingrid procures mFRR
#'   capacity through the balancing capacity market, which is held when needed.
#'   Balance service provider pledges itself to leave regulating bids on the
#'   regulation market. For that the balance service provider is entitled to
#'   capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/334
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_procuforecast_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_procuforecast_up <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 334,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), down, hourly market, procured volume
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The amount of downwards
#'   balancing capacity procured from the balancing capacity market, MW/h.
#'   Fingrid procures mFRR capacity through the balancing capacity market
#'   auction, which is held when needed. Balance service provider pledges itself
#'   to leave regulating bids on the regulation market. For that the balance
#'   service provider is entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW/h.
#' @seealso https://data.fingrid.fi/en/datasets/328
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_procured_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_procured_down <- function(start_time_utc = NA,
                                         end_time_utc = NA,
                                         user_key = NA) {
  return(
    get_data(
      api_number = 328,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), hourly market, procurement from Estonia
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data. In addition to the national
#'   weekly market, Fingrid also has the possibility to procure mFRR balancing
#'   capacity from Estonia. This dataset includes the procured balancing
#'   capacity amounts from Estonia, MW/week. The procured amount is published at
#'   latest on Friday of the week before the procurement week at 12:00 (EET).
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW/h.
#' @seealso https://data.fingrid.fi/en/datasets/333
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_procured_EE(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_procured_EE <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 333,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity (mFRR), up, hourly market, procured volume
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Data before 30.11.2022 is test-data.The amount of upwards
#'   balancing capacity procured from the balancing capacity market, MW/h.
#'   Fingrid procures mFRR capacity through the balancing capacity market
#'   auction, which is held when needed. Balance service provider pledges itself
#'   to leave regulating bids on the regulation market. For that the balance
#'   service provider is entitled to capacity payment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/327
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_hourly_procured_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_hourly_procured_up <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 327,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity Market price
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The price of capacity procured from the balancing capacity
#'   market, /MW,h. Fingrid procures mFRR capacity throught the balancing
#'   capacity market on a weekly auction, which is held when needed. Balance
#'   service provider pledges itself to leave regulating bids on the regulation
#'   market. For that the balance service provider is entitled to capacity
#'   payment. The price is published at latest on Friday on the week before the
#'   procurement week at 12:00 (EET)
#' @return A data frame object with time series data having period 1 vk and unit
#'   type €.
#' @seealso https://data.fingrid.fi/en/datasets/262
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_weekly_price(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_weekly_price <- function(start_time_utc = NA,
                                 end_time_utc = NA,
                                 user_key = NA) {
  return(
    get_data(
      api_number = 262,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Balancing Capacity Market results
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The amount of capacity procured from the balancing capacity
#'   market, MW/week. Fingrid procures mFRR capacity throught the balancing
#'   capacity market on a weekly auction, which is held when needed. Balance
#'   service provider pledges itself to leave regulating bids on the regulation
#'   market. For that the balance service provider is entitled to capacity
#'   payment. The procured amount is published at latest on Friday on the week
#'   before the procurement week at 12:00 (EET)
#' @return A data frame object with time series data having period 1 vk and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/261
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_cm_weekly_procured(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_cm_weekly_procured <- function(start_time_utc = NA,
                                    end_time_utc = NA,
                                    user_key = NA) {
  return(
    get_data(
      api_number = 261,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Down-regulation bids, price of the last activated - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The price of the last activated down-regulation bid. The price
#'   is published real-time when Finland is a separate regulation area.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/251
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_bids_price_last_activated_down_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_bids_price_last_activated_down_RTD <- function(start_time_utc = NA,
                                                       end_time_utc = NA,
                                                       user_key = NA) {
  return(
    get_data(
      api_number = 251,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title The sum of the down-regualtion bids in the Balancing energy market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The hourly sum of the down-regulation offers given by Finnish
#'   parties to the Balancing energy market is published hourly with one hour
#'   delay, eg. information from hour 07-08 is published at 9 o'clock.Balancing
#'   energy market is market place for manual freqeuncy restoration reserve
#'   (mFRR) which is used to balance the electricity generation and consumption
#'   in real time. The Balancing energy market organized by Fingrid is part of
#'   the Nordic Balancing energy market that is called also Regulating power
#'   market. Fingrid orders up- or down-regulation from the Balancing energy
#'   market. Down-regulation considers increasing of consumption or reducing of
#'   generation. Down-regulation bids have negative sign.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MW.
#' @seealso https://data.fingrid.fi/en/datasets/105
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_bids_sum_regulation_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_bids_sum_regulation_down <- function(start_time_utc = NA,
                                             end_time_utc = NA,
                                             user_key = NA) {
  return(
    get_data(
      api_number = 105,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title The sum of the up-regulation bids in the balancing energy market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The hourly sum of the up-regulation offers given by Finnish
#'   parties to the Balancing energy market is published hourly with one hour
#'   delay, eg. information from hour 07-08 is published at 9 o'clock.Balancing
#'   energy market is market place for manual freqeuncy restoration reserve
#'   (mFRR) which is used to balance the electricity generation and consumption
#'   in real time. The Balancing energy market organized by Fingrid is part of
#'   the Nordic Balancing energy market that is called also Regulating power
#'   market. Fingrid orders up- or down-regulation from the Balancing energy
#'   market. Up-regulation considers increasing of production or reducing of
#'   consumption.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/243
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_bids_sum_regulation_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_bids_sum_regulation_up <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 243,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Hour change regulation, up-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description In order to reduce problems encountered at the turn of the hour
#'   in the Nordic countries or in Finland, the planned production changes will
#'   be transfered to begin 15 minutes before or after the planned moment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/240
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_hour_change_regulation_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_hour_change_regulation_up <- function(start_time_utc = NA,
                                              end_time_utc = NA,
                                              user_key = NA) {
  return(
    get_data(
      api_number = 240,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Ordered down-regulations from Balancing energy market in Finland
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Ordered down-regulations from Balancing energy market in
#'   Finland. The volume of ordered down-regulations from Balancing energy
#'   market in Finland is published hourly with two hours delay, eg. information
#'   from hour 06-07 is published at 9 o'clock.Balancing energy market is market
#'   place for manual freqeuncy restoration reserve (mFRR) which is used to
#'   balance the electricity generation and consumption in real time. The
#'   Balancing energy market organized by Fingrid is part of the Nordic
#'   Balancing energy market that is called also Regulating power market.
#'   Fingrid orders up- or down-regulation from the Balancing energy market.
#'   Down-regulation considers increasing of consumption or reducing of
#'   generation. Down-regulation volume has negative sign.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/33
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_ordered_regulations_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_ordered_regulations_down <- function(start_time_utc = NA,
                                             end_time_utc = NA,
                                             user_key = NA) {
  return(
    get_data(
      api_number = 33,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Ordered up-regulations from Balancing energy market in Finland
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Ordered up-regulations from Balancing energy market in Finland.
#'   The volume of ordered up-regulations from Balancing energy market in
#'   Finland is published hourly with two hours delay, eg. information from hour
#'   06-07 is published at 9 o'clock.Balancing energy market is market place for
#'   manual freqeuncy restoration reserve (mFRR) which is used to balance the
#'   electricity generation and consumption in real time. The Balancing energy
#'   market organized by Fingrid is part of the Nordic Balancing energy market
#'   that is called also Regulating power market. Fingrid orders up- or
#'   down-regulation from the Balancing energy market. Up-regulation considers
#'   increasing of generation or reducing of consumption.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/34
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_ordered_regulations_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_ordered_regulations_up <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 34,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Other power transactions, down-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Other power transactions which are necessary in view of the
#'   power system.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/213
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_other_pwr_regulation_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_other_pwr_regulation_down <- function(start_time_utc = NA,
                                              end_time_utc = NA,
                                              user_key = NA) {
  return(
    get_data(
      api_number = 213,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Other power transactions, up-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Other power transactions which are necessary in view of the
#'   power system.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/214
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_other_pwr_regulation_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_other_pwr_regulation_up <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 214,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Price of the last activated up-regulation bid - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description The price of the last activated up-regulation bid. The price is
#'   published real-time when Finland is a separate regulation area.
#' @return A data frame object with time series data having period 1 h and unit
#'   type €/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/22
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_price_last_activated_bid_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_price_last_activated_bid_RTD <- function(start_time_utc = NA,
                                                 end_time_utc = NA,
                                                 user_key = NA) {
  return(
    get_data(
      api_number = 22,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Down-regulation price in the Balancing energy market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Down-regulation price in the Balancing energy market. The price
#'   of the cheapest regulating bid used in the balancing power market during
#'   the particular hour; however, at the most the price for price area Finland
#'   in Nord Pool Spot (Elspot FIN).Down-regulating price in Finland is the
#'   price of the most expensive down-regulating bid used in the Balancing
#'   energy market during the hour in question; however, it is at the most the
#'   day ahead market price for the price area Finland. Down-regulating price
#'   for each hour is published hourly with one hour delay, eg. information from
#'   hour 07-08 is published at 9 o'clock.Balancing energy market is market
#'   place for manual freqeuency restoration reserve (mFRR) which is used to
#'   balance the electricity generation and consumption in real time. The
#'   Balancing energy market organized by Fingrid is part of the Nordic
#'   Balancing energy market that is called also Regulating power market.
#'   Fingrid orders up- or down-regulation from the Balancing energy market.
#'   Down-regulation considers increasing of consumption or reducing of
#'   generation.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/106
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_price_regulation_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_price_regulation_down <- function(start_time_utc = NA,
                                          end_time_utc = NA,
                                          user_key = NA) {
  return(
    get_data(
      api_number = 106,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Up-regulating price in the Balancing energy market
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Up-regulating price in Finland is the price of the most
#'   expensive up-regulating bid used in the Balancing energy market during the
#'   hour in question; however, it is at least the day ahead market price for
#'   the price area Finland. Up-regulating price for each hour is published
#'   hourly with one hour delay, eg. information from hour 07-08 is published at
#'   9 o'clock.Balancing energy market is market place for manual freqeuncy
#'   restoration reserve (mFRR) which is used to balance the electricity
#'   generation and consumption in real time. The Balancing energy market
#'   organized by Fingrid is part of the Nordic Balancing energy market that is
#'   called also Regulating power market. Fingrid orders up- or down-regulation
#'   from the Balancing energy market. Up-regulation considers increasing of
#'   production or reducing of consumption.
#' @return A data frame object with time series data having period 1 h and unit
#'   type EUR/MWh.
#' @seealso https://data.fingrid.fi/en/datasets/244
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_price_regulation_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_price_regulation_up <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 244,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Special regulation, down-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Regulation which takes place in the regulating power market by
#'   Fingrid for reasons other than the needs of national balance management
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/118
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_special_regulation_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_special_regulation_down <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 118,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Special regulation, up-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Regulation which takes place in the regulating power market by
#'   Fingrid for reasons other than the needs of national balance management
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/119
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_special_regulation_up(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_special_regulation_up <- function(start_time_utc = NA,
                                          end_time_utc = NA,
                                          user_key = NA) {
  return(
    get_data(
      api_number = 119,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Hour change regulation, down-regulation
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description In order to reduce problems encountered at the turn of the hour
#'   in the Nordic countries or in Finland, the planned production changes will
#'   be transfered to begin 15 minutes before or after the planned moment.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/239
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- mfrr_em_hour_change_regulation_down(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
mfrr_em_hour_change_regulation_down <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 239,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Measured transmission of electricity in Finland from north to south
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Measured electricity flow in North-South cut in Finland (cut
#'   P1). In the graph flow from North to South is positive. The Data before
#'   28.03.2024 is in hourly resolution.
#' @return A data frame object with time series data having period 1 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/30
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- nscut_measured_flow(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
nscut_measured_flow <- function(start_time_utc = NA,
                                end_time_utc = NA,
                                user_key = NA) {
  return(
    get_data(
      api_number = 30,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Planned weekly capacity from north to south
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned weekly capacity on North-South cut in Finland (cut P1)
#'   from North to South. Planned outages are included in the weekly capacity,
#'   information is not updated after disturbances.
#' @return A data frame object with time series data having period 1 h and unit
#'   type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/28
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- nscut_weekly_plan_cap_NS(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
nscut_weekly_plan_cap_NS <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 28,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Planned weekly capacity from south to north
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Planned weekly capacity on North-South cut in Finland (cut P1)
#'   from South to North. Planned outages are included in the weekly capacity,
#'   information is not updated after disturbances.
#' @return A data frame object with time series data having period 1 h and unit
#'   type Mwh/h.
#' @seealso https://data.fingrid.fi/en/datasets/29
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- nscut_weekly_plan_cap_SN(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
nscut_weekly_plan_cap_SN <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 29,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Cogeneration of district heating - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Cogeneration of district heating based on the real-time
#'   measurements in Fingrid's operation control system. The data is updated
#'   every 3 minutes.Cogeneration means power plants that produce both
#'   electricity and district heating or process steam (combined heat and power,
#'   CHP).
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/201
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_cogeneration_district_heating_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_cogeneration_district_heating_RTD <- function(start_time_utc = NA,
                                                       end_time_utc = NA,
                                                       user_key = NA) {
  return(
    get_data(
      api_number = 201,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Surplus/deficit, cumulative - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Information is based on the real time measurements in Fingrid's
#'   power control system.Power deficit/surplus represents the balance between
#'   production and consumption in Finland, taking into account imports and
#'   exports. It is calculated as the difference between the measured net
#'   import/export and the confirmed net exchange program between Finland and
#'   the other Nordic countries. The cumulative production deficit/surplus is
#'   the hourly energy generated from the difference.Sign convention: production
#'   deficit -, surplus +The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/186
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_cumulative_surplus_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_cumulative_surplus_RTD <- function(start_time_utc = NA,
                                            end_time_utc = NA,
                                            user_key = NA) {
  return(
    get_data(
      api_number = 186,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity consumption in Finland - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Electricity consumption in Finland is calculated based on
#'   production and import/export. The data is updated every 3 minutes.
#'   Production information and import/export are based on the real-time
#'   measurements in Fingrid's operation control system.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/193
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_electricity_consumption_FI_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_electricity_consumption_FI_RTD <- function(start_time_utc = NA,
                                                    end_time_utc = NA,
                                                    user_key = NA) {
  return(
    get_data(
      api_number = 193,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Net import/export of electricity - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Net import to Finland and net export from Finland. The data is
#'   updated every 3 minutes.Production information and import/export are based
#'   on the real-time measurements in Fingrid's operation control system.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/194
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_electricity_netimport(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_electricity_netimport <- function(start_time_utc = NA,
                                           end_time_utc = NA,
                                           user_key = NA) {
  return(
    get_data(
      api_number = 194,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production in Finland - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Electricity production in Finland based on the real-time
#'   measurements in Fingrid's operation control system The data is updated
#'   every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/192
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_electricity_production_FI_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_electricity_production_FI_RTD <- function(start_time_utc = NA,
                                                   end_time_utc = NA,
                                                   user_key = NA) {
  return(
    get_data(
      api_number = 192,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production, reserve power plants and small-scale
#'   production - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Reserve power plants electrical production is based on the
#'   real-time measurements in Fingrid's operation control system. Estimated
#'   small-scale production is added, of which there are no measurements
#'   available. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/205
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_electricity_production_reservetotal_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_electricity_production_reservetotal_RTD <- function(start_time_utc = NA,
                                                             end_time_utc = NA,
                                                             user_key = NA) {
  return(
    get_data(
      api_number = 205,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity shortage status
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Electricity shortage status. An electricity shortage occurs when
#'   electricity production and imports are not enough to cover electricity
#'   consumption. In such cases, it is necessary to restrict consumption to stop
#'   the power system from crashing altogether. Fingrid informs citizens on its
#'   (website)(https://www.fingrid.fi/en/grid/information-regarding-electricity-shortages/)
#'   and with a press release in accordance with the three-step procedure when
#'   the situation possibly escalates. 0 = Normal 1 = Electricity shortage
#'   possible 2 = High risk of electricity shortage 3 = Electricity shortage
#'   The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type 0-3.
#' @seealso https://data.fingrid.fi/en/datasets/336
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_electricity_shortage_status_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_electricity_shortage_status_RTD <- function(start_time_utc = NA,
                                                     end_time_utc = NA,
                                                     user_key = NA) {
  return(
    get_data(
      api_number = 336,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Frequency - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Frequency of the power system based on the real-time
#'   measurements in Fingrid's operation control system. The data is updated
#'   every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type Hz.
#' @seealso https://data.fingrid.fi/en/datasets/177
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_frequency_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_frequency_RTD <- function(start_time_utc = NA,
                                   end_time_utc = NA,
                                   user_key = NA) {
  return(
    get_data(
      api_number = 177,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Hydro power production - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Hydro power production in Finland based on the real-time
#'   measurements in Fingrid's operation control system. The data is updated
#'   every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/191
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_hydro_power_production_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_hydro_power_production_RTD <- function(start_time_utc = NA,
                                                end_time_utc = NA,
                                                user_key = NA) {
  return(
    get_data(
      api_number = 191,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Industrial cogeneration - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Cogeneration of industry based on the real-time measurements in
#'   Fingrid's operation control system. The data is updated every 3
#'   minutes.Cogeneration means power plants that produce both electricity and
#'   district heating or process steam (combined heat and power, CHP).
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/202
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_industrial_cogeneration_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_industrial_cogeneration_RTD <- function(start_time_utc = NA,
                                                 end_time_utc = NA,
                                                 user_key = NA) {
  return(
    get_data(
      api_number = 202,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Kinetic energy of the Nordic power system - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Real-time estimate of the kinetic energy of the Nordic power
#'   system calculated by the Nordic transmission system operators. The data is
#'   updated every 1 minute. Historical data as of 27.3.2015 available.
#' @return A data frame object with time series data having period 1 min and
#'   unit type 1 GWs.
#' @seealso https://data.fingrid.fi/en/datasets/260
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_kinetic_energy_nordic_pwr_sys_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_kinetic_energy_nordic_pwr_sys_RTD <- function(start_time_utc = NA,
                                                       end_time_utc = NA,
                                                       user_key = NA) {
  return(
    get_data(
      api_number = 260,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Nuclear power production - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Nuclear power production in Finland based on the real-time
#'   measurements in Fingrid's operation control system. The data is updated
#'   every 3 minutes.Due to the fire on our Olkiluoto substation the total
#'   amount of nuclear power measurement has been incorrect between 18 July at
#'   09:00 to 20 July at 13:00. Data corrected 25.1.2019.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/188
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_nuclear_power_production_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_nuclear_power_production_RTD <- function(start_time_utc = NA,
                                                  end_time_utc = NA,
                                                  user_key = NA) {
  return(
    get_data(
      api_number = 188,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Peak load power - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Activated peak load power based on the real-time measurements in
#'   Fingrid's operation control system including peak load reserve activations
#'   and trial runs during winter period. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/183
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_peak_load_power_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_peak_load_power_RTD <- function(start_time_utc = NA,
                                         end_time_utc = NA,
                                         user_key = NA) {
  return(
    get_data(
      api_number = 183,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Electricity production, surplus/deficit - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Finland's energy production surplus/deficit. Information is
#'   based on the real time measurements in Fingrid's power control system.Power
#'   deficit/surplus represents the balance between power production and
#'   consumption in Finland, taking into account imports and exports. Power
#'   deficit/surplus is calculated as the difference between the measured net
#'   import/export and the confirmed net exchange program between Finland and
#'   the other Nordic countries.Sign convention: production deficit -, surplus
#'   +The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/198
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_production_surplus(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_production_surplus <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 198,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Power system state - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Different states of the power system - traffic lights:1=green,
#'   2=yellow, 3=red, 4=black, 5=blue* Green: Power system is in normal secure
#'   state.* Yellow: Power system is in endangered state. The adequacy of the
#'   electricity is endangered or the power system doesn't fulfill the security
#'   standards.* Red: Power system is in disturbed state. Load shedding has
#'   happened in order to keep the adequacy and security of the power system or
#'   there is a remarkable risk to a wide black out. * Black: An extremely
#'   serious disturbance or a wide black out in Finland.* Blue: The network is
#'   being restored after an extremely serious disturbance or a wide
#'   blackout.The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type no.
#' @seealso https://data.fingrid.fi/en/datasets/209
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_state_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_state_RTD <- function(start_time_utc = NA,
                               end_time_utc = NA,
                               user_key = NA) {
  return(
    get_data(
      api_number = 209,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Temperature in Helsinki - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Outside air temperature measurement at Tammisto substation. The
#'   data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type C.
#' @seealso https://data.fingrid.fi/en/datasets/178
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_temp_helsinki_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_temp_helsinki_RTD <- function(start_time_utc = NA,
                                       end_time_utc = NA,
                                       user_key = NA) {
  return(
    get_data(
      api_number = 178,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Temperature in Jyväskylä - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Outside air temperature measurement at Petäjävesi substation.
#'   The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type C.
#' @seealso https://data.fingrid.fi/en/datasets/182
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_temp_jyvaskyla_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_temp_jyvaskyla_RTD <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 182,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Temperature in Oulu - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Outside air temperature measurement at Leväsuo substation. The
#'   data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type C.
#' @seealso https://data.fingrid.fi/en/datasets/196
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_temp_oulu_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_temp_oulu_RTD <- function(start_time_utc = NA,
                                   end_time_utc = NA,
                                   user_key = NA) {
  return(
    get_data(
      api_number = 196,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Temperature in Rovaniemi - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Outside air temperature measurement at Valajaskoski substation.
#'   The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type C.
#' @seealso https://data.fingrid.fi/en/datasets/185
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_temp_rovaniemi_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_temp_rovaniemi_RTD <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 185,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Time deviation - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Time deviation is the time difference in seconds between a clock
#'   running according to the frequency of the grid and a reference clock
#'   independent of the frequency of the grid. The data is updated every 3
#'   minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type sec.
#' @seealso https://data.fingrid.fi/en/datasets/206
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_time_deviation_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_time_deviation_RTD <- function(start_time_utc = NA,
                                        end_time_utc = NA,
                                        user_key = NA) {
  return(
    get_data(
      api_number = 206,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission between Finland and Estonia - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Power transmission between Finland and Estonia HVDC tie lines
#'   (Estlink 1 and Estlink 2). Data is based on the real-time measurements in
#'   Fingrid's operation control system. Positive sign means transmission from
#'   Finland to Estonia. Negative sign means transmission from Estonia to
#'   Finland. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/180
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_trans_FI_EE_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_trans_FI_EE_RTD <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 180,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission between Finland and Norway - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Power transmission between Finland and Norway 220kV AC tie line.
#'   Data is based on the real-time measurements in Fingrid's operation control
#'   system. Positive sign means transmission from Finland to Norway. Negative
#'   sign means transmission from Norway to Finland. The data is updated every 3
#'   minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/187
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_trans_FI_NO_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_trans_FI_NO_RTD <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 187,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission between Sweden and Åland - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Power transmission between Åland and Sweden based on the
#'   real-time measurements in Fingrid's operation control system. Åland is a
#'   part of SE3 (Central-Sweden) bidding zone. Positive sign means transmission
#'   from Åland to Sweden. Negative sign means transmission from Sweden to
#'   Åland. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/90
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_trans_FI_OO_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_trans_FI_OO_RTD <- function(start_time_utc = NA,
                                     end_time_utc = NA,
                                     user_key = NA) {
  return(
    get_data(
      api_number = 90,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission between Finland and Northern Sweden - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Power transmission between Northern Sweden (SE1) and Finland
#'   (FI) 400kV AC tie line. Data is based on the real-time measurements in
#'   Fingrid's operation control system. Positive sign means transmission from
#'   Finland to Northern Sweden (SE1). Negative sign means transmission from
#'   Northern Sweden (SE1) to Finland. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/87
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_trans_FI_SE1_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_trans_FI_SE1_RTD <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 87,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Transmission between Finland and Central Sweden - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Power transmission between Central Sweden (SE3) and Finland (FI)
#'   HVDC tie lines. Data is based on the real-time measurements in Fingrid's
#'   operation control system. Positive sign means transmission from Finland to
#'   Central Sweden (SE3). Negative sign means transmission from Central Sweden
#'   (SE3) to Finland. The data is updated every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MWh/h.
#' @seealso https://data.fingrid.fi/en/datasets/89
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_trans_FI_SE3_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_trans_FI_SE3_RTD <- function(start_time_utc = NA,
                                      end_time_utc = NA,
                                      user_key = NA) {
  return(
    get_data(
      api_number = 89,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}

#' @title Wind power production - real time data
#' @param start_time_utc Start time in UTC. Character array
#'   YYYY-MM-ddTHH:mm:ss.sssZ
#' @param end_time_utc End time in UTC. Character array YYYY-MM-ddTHH:mm:ss.sssZ
#' @param user_key Character array holding API-key. Free from
#'   https://data.fingrid.fi/en/instructions
#' @description Wind power production based on the real-time measurements in
#'   Fingrid's operation control system. About two percent of the production
#'   capacity is estimated as measurements aren't available. The data is updated
#'   every 3 minutes.
#' @return A data frame object with time series data having period 3 min and
#'   unit type MW.
#' @seealso https://data.fingrid.fi/en/datasets/181
#' @examples
#' \dontrun{
#' library(finnishgrid)
#' start = "2024-06-01T00:00:00.000Z"  # UTC
#' end = "2024-06-03T00:00:00.000Z"    # UTC
#' key = "MY_SUPER_SECRET"
#' df <- powersys_wind_pwr_production_RTD(start_time_utc = start,
#'          end_time_utc = end,
#'          user_key = key)
#' summary(df)
#' }
#' @export
powersys_wind_pwr_production_RTD <- function(start_time_utc = NA,
                                             end_time_utc = NA,
                                             user_key = NA) {
  return(
    get_data(
      api_number = 181,
      start_time_utc = start_time_utc,
      end_time_utc = end_time_utc,
      user_key = user_key
    )
  )
}
