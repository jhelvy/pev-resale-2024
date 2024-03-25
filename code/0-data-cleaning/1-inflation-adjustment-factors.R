source(here::here('code', '0-functions.R'))

# Read in and format CPI data

cpi <- read_csv(here::here('data', 'inflation-cpi.csv')) %>% 
  clean_names() %>% 
  pivot_longer(
    names_to = 'month', 
    values_to = 'cpi',
    cols = -year
  ) %>% 
  # Convert month to number and add a date
  mutate(
    date = mdy(paste(month, '01,', year)), 
    month = month(date)
  )

# Adjust to 2019 as reference year
cpi2019 <- cpi %>% 
  filter(year == 2019, month == 1) %>% 
  pull(cpi)
cpi$cpi <- cpi$cpi / cpi2019

# Save
write_csv(cpi, here::here('data', 'inflation-cpi-tidy.csv'))


# Adjust electricity price data

states_dict <- read_csv(here::here('data', 'states.csv')) %>% 
  select('State', 'State Code')
elec_prices <- readxl::read_excel(
  here::here('data', 'us_electricity_price.xlsx')) %>% 
  left_join(states_dict, by = 'State') %>%
  filter(!is.na(`State Code`)) %>%
  select(year, elec_price = Residential, state = `State Code`) %>% 
  as.data.table() %>% 
  # inflation adjust
  left_join(
    cpi %>% 
      group_by(year) %>% 
      summarise(cpi = mean(cpi)), 
    by = 'year'
  ) %>% 
  mutate(
    price_raw = elec_price,
    price = price_raw / cpi
  ) %>% 
  select(-cpi, -elec_price)

write_csv(elec_prices, here::here('data', 'electricity_prices_tidy.csv'))
