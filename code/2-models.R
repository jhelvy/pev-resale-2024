rm(list = ls()) # Remove all objects
.rs.restartR()  # Restart R

# Basic exponential decline model: r = a*exp(b1*x1 + b2*x2)
# where
#   r = Retention rate
#   a = Intercept
#   x1, x2 = Covariates (e.g. age, miles, etc.)
#   b1, b2 = Covariate coefficients
#
# Log transformation: ln(r) = ln(a) + b1*x1 + b2*x2
#
# To convert log-space estimated coefficients back to original model:
# a = exp(int)

# Load functions and path
source(here::here('code', '0-functions.R'))

# A | Multiple vehicle types - CVs only ----

dt_cv <- load_dt_cv() %>%
  select(rr, age_years, my, miles, cents_per_mile, vehicle_type, make, model)

# a1

tictoc::tic()
a1 <- feols(
  fml = log(rr) ~ 
    age_years*vehicle_type,
  data = dt_cv
)
tictoc::toc()

summary(a1)
compute_age_effects(a1)
compute_ints(a1, 'vehicle_type')

# a2

tictoc::tic()
a2 <- feols(
  fml = log(rr) ~ 
    age_years*vehicle_type + 
    miles + 
    cents_per_mile,
  data = dt_cv
)
tictoc::toc()

summary(a2)
compute_age_effects(a2)
compute_ints(a2, 'vehicle_type')

# View effects
model <- a1
age_coefs <- coef(model)[str_detect(names(coef(model)), 'age_years')]
age_effect_car <- exp(age_coefs['age_years']) - 1
age_effect_suv <- exp(age_coefs['age_years'] + age_coefs['age_years:vehicle_typesuv']) - 1
age_effect_pickup <- exp(age_coefs['age_years'] + age_coefs['age_years:vehicle_typepickup']) - 1
cat('1 year increase, Car:', age_effect_car)
cat('1 year increase, SUV:', age_effect_suv)
cat('1 year increase, Pickup:', age_effect_pickup)

# Save and clear results
save_model_results(a1)
save_model_results(a2)
rm(dt_cv)
rm(a1, a2)
gc()

# B  | Multiple powertrains - cars only ----

dt_car <- load_dt_car() %>%
  select(rr, age_years, miles, cents_per_mile, powertrain, make, model)

# b1 

tictoc::tic()
b1 <- feols(
  fml = log(rr) ~ 
    age_years*powertrain,
  data = dt_car
)
tictoc::toc()

summary(b1)
compute_age_effects(b1)
compute_ints(b1, 'powertrain')
rm(dt_car)

# b2 

dt_car_tesla <- load_dt_car() %>% 
  separate_bev_tesla() %>% 
  select(rr, age_years, my, miles, cents_per_mile, powertrain, make, model)

tictoc::tic()
b2 <- feols(
  fml = log(rr) ~ 
    age_years*powertrain,
  data = dt_car_tesla
)
tictoc::toc()

summary(b2)
compute_age_effects(b2)
compute_ints(b2, 'powertrain')

# b3

tictoc::tic()
b3 <- feols(
  fml = log(rr) ~ 
    age_years*powertrain*my + 
    miles + 
    cents_per_mile,
  data = dt_car_tesla
)
tictoc::toc()

summary(b3)
compute_age_effects(b3)
compute_ints(b3, 'powertrain')

# Save and clear results
save_model_results(b1)
save_model_results(b2)
save_model_results(b3)
rm(b1, b2, b3)
rm(dt_car_tesla)
gc()





# C | BEV powertrain, non-Tesla cars only ----

dt_bev_car_nontesla <- load_dt_bev_car_nontesla()

# c1 - base model, no model year effects

tictoc::tic()
c1 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    miles +
    dom_listing + 
    cents_per_mile + 
    range + 
    subsidy_total_listing, 
  data = dt_bev_car_nontesla
)
tictoc::toc()

summary(c1)
print_effects_summary(c1)
age_years_coefs(c1)
compute_age_effects(c1)
compute_ints(c1, 'model')
save_model_results(c1)

# c2 - MY effects

tictoc::tic()
c2 <- feols(
  fml = log(rr) ~
    age_years*model + 
    my +
    miles +
    dom_listing + 
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_bev_car_nontesla
)
tictoc::toc()

summary(c2)
print_effects_summary(c2)
compute_age_effects(c2)
compute_ints(c2, 'model')
compute_ints(c2, 'my')
save_model_results(c2)


rm(c1, c2)
rm(dt_bev_car_nontesla)
gc()


# D  | PHEV powertrain, cars only ----

dt_phev_car <- load_dt_phev_car() 

# d1

tictoc::tic()
d1 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    miles +
    dom_listing + 
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_phev_car
)
tictoc::toc()

summary(d1)
print_effects_summary(d1)
age_years_coefs(d1)
compute_age_effects(d1)
compute_ints(d1, 'model')
save_model_results(d1)

# d2

tictoc::tic()
d2 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my +
    miles +
    dom_listing + 
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_phev_car
)
tictoc::toc()

summary(d2)
print_effects_summary(d2)
age_years_coefs(d2)
compute_age_effects(d2)
compute_ints(d2, 'model')
save_model_results(d2)

# Save and clear results
rm(d1, d2)
rm(dt_phev_car)
gc()




# E  | HEV powertrain, cars only ----

dt_hev_car <- load_dt_hev_car() 

# e1

tictoc::tic()
e1 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_hev_car
)
tictoc::toc()

summary(e1)
print_effects_summary(e1)
age_years_coefs(e1)
compute_age_effects(e1)
compute_ints(e1, 'model')
save_model_results(e1)

# e2

tictoc::tic()
e2 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_hev_car
)
tictoc::toc()

summary(e2)
print_effects_summary(e2)
age_years_coefs(e2)
compute_age_effects(e2)
save_model_results(e2)

# Save and clear results
rm(e1, e2)
rm(dt_hev_car)
gc()


# F  | CV powertrain - cars only ----

dt_cv_car <- load_dt_cv_car() %>%
  select(rr, age_years, model, miles, dom_listing, cents_per_mile, my, state)

# f1 

tictoc::tic()
f1 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_cv_car
)
tictoc::toc()

summary(f1)
print_effects_summary(f1)
age_years_coefs(f1)
compute_age_effects(f1)
compute_ints(f1, 'model')

# Save and clear results
save_model_results(f1)
rm(f1)
gc()


# f2

tictoc::tic()
f2 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_cv_car
)
tictoc::toc()

summary(f2)
print_effects_summary(f2)
age_years_coefs(f2)
compute_age_effects(f2)

# Save and clear results
save_model_results(f2)
rm(f2)
gc()



# G | BEV powertrain, Tesla cars only ----

dt_bev_car_tesla <- load_dt_bev_car_tesla()
  # filter(! my %in% factor(2018)) %>% 
  # mutate(
  #   my = as.numeric(as.character(my)), 
  #   my = as.factor(my)
  # )

# dt_bev_car_tesla %>% 
#   ggplot() + 
#   geom_point(aes(x = age_years, y = rr, color = my), alpha = 0.2)

# g1 - base model, no model year effects

tictoc::tic()
g1 <- feols(
  fml = log(rr) ~
    age_years*model +
    miles +
    dom_listing +
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_bev_car_tesla
)
tictoc::toc()

summary(g1)
print_effects_summary(g1)
age_years_coefs(g1)
compute_age_effects(g1)
compute_ints(g1, 'model')
save_model_results(g1)

# g2 - MY effects

tictoc::tic()
g2 <- feols(
  fml = log(rr) ~
    age_years*model +
    my +
    miles +
    dom_listing +
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_bev_car_tesla
)
tictoc::toc()

summary(g2)
print_effects_summary(g2)
compute_age_effects(g2)
compute_ints(g2, 'model')
compute_ints(g2, 'my')
save_model_results(g2)


rm(g1, g2)
rm(dt_bev_car_tesla)
gc()




# H | COVID years ----

dt_car <- load_dt_covid() %>%
  filter(vehicle_type == 'car') %>% 
  separate_bev_tesla() %>% 
  select(
    rr, powertrain, make, model, age_years, my, miles, cents_per_mile, 
    dom_listing, range, subsidy_total_listing
  )

tictoc::tic()
h1 <- feols(
  fml = log(rr) ~ 
    age_years*powertrain,
  data = dt_car
)
tictoc::toc()

summary(h1)
print_effects_summary(h1)
compute_age_effects(h1)
compute_ints(h1, 'powertrain')

save_model_results(h1)
rm(h1)
gc()

# BEV - non-Tesla

dt_bev_car_nontesla <- dt_car %>% 
  filter(powertrain == 'bev')

tictoc::tic()
h2 <- feols(
  fml = log(rr) ~
    age_years*model + 
    my +
    miles +
    dom_listing + 
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_bev_car_nontesla
)
tictoc::toc()

summary(h2)
print_effects_summary(h2)
compute_age_effects(h2)
compute_ints(h2, 'model')
compute_ints(h2, 'my')
save_model_results(h2)
rm(h2)
gc()

# BEV - Tesla

dt_bev_car_tesla <- dt_car %>% 
  filter(powertrain == 'bev_tesla')

tictoc::tic()
h3 <- feols(
  fml = log(rr) ~
    age_years*model +
    my +
    miles +
    dom_listing +
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_bev_car_tesla
)
tictoc::toc()

summary(h3)
print_effects_summary(h3)
compute_age_effects(h3)
compute_ints(h3, 'model')
compute_ints(h3, 'my')
save_model_results(h3)
rm(h3)
gc()


# PHEV

dt_phev_car <- dt_car %>% 
  filter(powertrain == 'phev')

tictoc::tic()
h4 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my +
    miles +
    dom_listing + 
    cents_per_mile +
    range +
    subsidy_total_listing,
  data = dt_phev_car
)
tictoc::toc()

summary(h4)
print_effects_summary(h4)
compute_age_effects(h4)
compute_ints(h4, 'model')
compute_ints(h4, 'my')
save_model_results(h4)
rm(h4)
gc()

# HEV

dt_hev_car <- dt_car %>% 
  filter(powertrain == 'hybrid')

tictoc::tic()
h5 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_hev_car
)
tictoc::toc()

summary(h5)
print_effects_summary(h5)
compute_age_effects(h5)
compute_ints(h5, 'model')
compute_ints(h5, 'my')
save_model_results(h5)
rm(h5)
gc()

# CV

dt_cv_car <- dt_car %>% 
  filter(powertrain == 'conventional')

tictoc::tic()
h6 <- feols(
  fml = log(rr) ~ 
    age_years*model + 
    my + 
    miles +
    dom_listing +
    cents_per_mile,
  data = dt_cv_car
)
tictoc::toc()

summary(h6)
print_effects_summary(h6)
compute_age_effects(h6)
compute_ints(h6, 'model')
compute_ints(h6, 'my')
save_model_results(h6)
rm(h6)
gc()
