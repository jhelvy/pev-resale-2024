source(here::here('code', '0-functions.R'))

# Read in model counts

models <- read_csv(here::here('data', 'model_counts_top.csv'))

# Open arrow dataset

ds <- open_dataset(PATH_DB_RAW) %>%
  filter(!is.na(state)) %>% 
  filter(state != "") %>% 
  filter(state != " ") %>% 
  filter(model %in% models$model) %>% 
  select(
    vehicle_id, year, make, model, trim, age_years,
    price, miles, carfax_1_owner,
    powertrain, vehicle_type, inventory_type,
    state, zip, listing_year, status_date, dom_listing, listing_index
  )

# Read in all listings

# Can't do them both at once, memory limits
for (i in 1:3) {

    if (i == 1) {
        dt <- ds %>% 
            filter(inventory_type == 'new') %>%
            collect()
    } else if (i == 2) {
        dt <- ds %>% 
            filter(inventory_type == 'used' & vehicle_type != 'car') %>%
            collect()
    } else {
        dt <- ds %>% 
            filter(inventory_type == 'used' & vehicle_type == 'car') %>%
            collect()
    }

dim(dt)
# count(dt, powertrain, vehicle_type)

# Add gas price data

# Here we just join on the gas price at the state level during the month 
# the vehicle was listed as a proxy for the local gas prices at about the 
# time the vehicle was on the market

# Read in gas price data

gas_prices <- read_parquet(here::here('data', 'gasoline-prices.parquet')) %>%
    select(state = state_code, date, gas_price = price) %>% 
    mutate(date = ymd(date))

# Get the status date month, then create a joining variable as the 15th
# of that month in that year

dt[, date := ymd(paste(listing_year, month(status_date), "15", sep = "-")), ]

# Join the gas prices 

dt <- merge(dt, gas_prices, by = c('state', 'date'), all.x = TRUE)

# Drop the date variable, drop missing gas prices

dt <- dt %>%
    select(-date) %>% 
    filter(!is.na(gas_price))

# Electricity prices ----

elec_prices <- read_csv(here::here('data', 'electricity_prices_tidy.csv')) %>% 
  select(-price_raw) %>% 
  rename(elec_price = price)

# Add electricity prices in listing year

dt <- merge(dt, elec_prices, by = c('year', 'state'), all.x = TRUE)

# Fuel Economy ----

dt <- compute_fuel_cost(dt)

# Final fixes

# Remove small n cars
dt <- dt %>%
  group_by(powertrain, vehicle_type, make, model) %>%
  mutate(n = n()) %>%
  filter(n >= 1000) %>%
  select(-n) %>%
  ungroup()

# Drop models not in top models

dt <- merge(
  dt, 
  models %>% 
    select(powertrain, vehicle_type, make, model) %>% 
    mutate(keep = TRUE), 
  by = c('powertrain', 'vehicle_type', 'make', 'model'), 
  all.x = TRUE
) %>% 
  filter(!is.na(keep)) %>%
  select(-keep)

# Write to disc
dt %>%
    arrow::write_dataset(
        path = PATH_DB_FOLDER,
        partitioning = c("inventory_type", "powertrain", "vehicle_type"),
        format = "parquet"
    )

rm(dt)

}

# Finally, combine the partitioned dataset into a single parquet file

dt <- collect(open_dataset(PATH_DB_FOLDER)) %>% 
  as.data.table()

# Drop crazy cost per mile cars

dt <- dt[cents_per_mile < 30]

# Inflation adjust the listing prices and MSRPs

cpi <- read_csv(here::here('data', 'inflation-cpi-tidy.csv'))

dt <- dt %>% 
  mutate(month = month(status_date)) %>% 
  # Join CPI factors
  left_join(
    cpi %>% 
      rename(
        listing_year = year, 
        cpi_listing = cpi
      ) %>% 
      select(-date), 
    by = c('listing_year', 'month')
  ) %>% 
  left_join(
    cpi %>% 
      rename(cpi_year = cpi) %>% 
      select(-date), 
    by = c('year', 'month')
  ) %>% 
  mutate(
    price_raw = price,
    price = price_raw / cpi_listing, 
    msrp_raw = msrp, 
    msrp = msrp_raw / cpi_year 
  ) 

write_parquet(dt, PATH_DB)
