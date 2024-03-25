source(here::here('code', '0-functions.R'))

# Compute RR for all cars

dt_car <- load_dt_car() %>% 
    mutate(age_months = round(age_years * 12))

# Compute quantiles of RR

quantiles <- dt_car %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    summarise(
        rr25 = fquantile(rr, 0.25),
        rr50 = fquantile(rr, 0.5),
        rr75 = fquantile(rr, 0.75)
    )
    
# Save

write_csv(quantiles, here::here('data', 'quantiles.csv'))

# Separately compute the quantiles for BEVs only, separating out Tesla

quantiles_bev <- dt_car %>%
    filter(powertrain == "bev", vehicle_type == 'car') %>% 
    mutate(tesla = ifelse(make == "tesla", 1, 0)) %>% 
    group_by(age_months, tesla) %>% 
    summarise(
        rr25 = fquantile(rr, 0.25),
        rr50 = fquantile(rr, 0.5),
        rr75 = fquantile(rr, 0.75),
        arr25 = fquantile(arr, 0.25),
        arr50 = fquantile(arr, 0.5),
        arr75 = fquantile(arr, 0.75)
    )

# Save

write_csv(quantiles_bev, here::here('data', 'quantiles_bev.csv'))
