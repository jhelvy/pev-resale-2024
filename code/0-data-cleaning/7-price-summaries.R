# Load functions and path
source(here::here('code', '0-functions.R'))

ds <- open_dataset(path_depreciation)

# NEW listings ----

# Actually need to collect first here and do the calc in R because 
# medians are only approximate in arrow

dt <- ds %>%
  filter(inventory_type == "new") %>%
  select(powertrain, vehicle_type, year, make, model, trim, price, msrp) %>%
  collect() %>% 
  group_by(powertrain, vehicle_type, year, make, model, trim) %>% 
  summarise(
    n_listings = n(),
    price_mean = mean(price, na.rm = TRUE),
    price_median = median(price, na.rm = TRUE),
    price_sd = sd(price, na.rm = TRUE),
    msrp_mean = mean(msrp, na.rm = TRUE),
    msrp_median = median(msrp, na.rm = TRUE),
    msrp_sd = sd(msrp, na.rm = TRUE)
  ) %>% 
  ungroup()

write_parquet(dt, here::here('data', 'prices-new-national.parquet'))
