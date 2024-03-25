source(here::here('code', '0-functions.R'))

# 2 | Summary statistics for cars only ----

dt_car <- load_dt_car() %>% 
    mutate(
        powertrain = as.character(powertrain),
        powertrain = ifelse(
            make == 'tesla', 'bev-tesla', ifelse(
                powertrain == 'bev' & make != 'tesla', 'bev-other', powertrain))
    ) %>% 
    select(powertrain, make, model, my, miles, age_years, price, msrp, range)

# Count of models
mc <- dt_car %>%
  distinct(powertrain, model) %>%
  group_by(powertrain) %>%
  summarise(n_models = n())

# Set factor levels for powertrain
dt_car$powertrain <- factor(
  dt_car$powertrain, levels = c(
    "conventional", "hybrid", "phev", "bev-tesla", "bev-other"
  )
)

# Reformat for word

table2 <- dt_car %>%
  tbl_summary(
    by = powertrain,
    include = c("my", "miles", "age_years", "price", "msrp", "range"),
    label = list(
      my ~ "Model Year", 
      miles ~ "Mileage (1,000)", age_years ~ "Age (years)", 
      price ~ "Listing price (Used $USD)", 
      msrp ~ "MSRP (New $USD)", 
      range ~ "Electric Range (mi)"
    ),
    # type = age_years ~ "continuous",
    digits = starts_with("age") ~ 2,
    statistic = list(
      miles ~ c("{mean}", "{median}", "{sd}"), 
      age_years ~ c("{mean}", "{median}", "{sd}"), 
      price ~ c("{mean}", "{median}", "{sd}"), 
      msrp ~ c("{mean}", "{median}", "{sd}"),
      range ~ c("{mean}", "{median}", "{sd}", "{min}", "{max}")
    ),
    type = list(
      miles ~ "continuous2", 
      age_years ~ "continuous2",
      price ~ "continuous2", 
      msrp ~ "continuous2", 
      range ~ "continuous2"
    ),
    missing_text = "(NA)",
    sort = list(everything() ~ "alphanumeric")
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Key Sample Stats**")

# Print to word doc
table2 %>%
  gtsummary::as_flex_table() %>%
  print( preview = "docx")

# 3 | Multiple powertrains ----

# read in models with just powertrain coefs
b1 <- qread(here::here('models-temp', 'b1.qs'))
b2 <- qread(here::here('models-temp', 'b2.qs'))
h1 <- qread(here::here('models-temp', 'h1.qs'))

# Table 3
int_slope_table(b1) %>% 
  rename(int1 = int, slope1 = slope) %>% 
  full_join(
    int_slope_table(b2) %>% 
      rename(int2 = int, slope2 = slope) %>% 
      mutate(
        var = ifelse(
          var == 'powertrainbev', 'powertrainbev_nontesla', 
          var
        )
      ), 
    by = 'var'
  ) %>% 
  select(powertrain = var, starts_with('int'), starts_with('slope')) %>% 
  flextable::flextable() %>% 
  theme_box() %>%
  print(preview = "docx")

# S5 ----
int_slope_table(b2) %>% 
  rename(int1 = int, slope1 = slope) %>% 
  mutate(
    var = ifelse(var == 'powertrainbev', 'powertrainbev_nontesla', var)
  ) %>% 
  full_join(
    int_slope_table(h1) %>% 
      rename(int2 = int, slope2 = slope) %>% 
      mutate(
        var = ifelse(var == 'powertrainbev', 'powertrainbev_nontesla', var)
      ),
    by = 'var'
  ) %>% 
  select(powertrain = var, starts_with('int'), starts_with('slope')) %>% 
  flextable::flextable() %>% 
  theme_box() %>%
  print(preview = "docx")

# 4 | Individual powertrains ----

c2 <- qread(here::here('models-temp', 'c2.qs'))
d2 <- qread(here::here('models-temp', 'd2.qs'))
e2 <- qread(here::here('models-temp', 'e2.qs'))
f2 <- qread(here::here('models-temp', 'f2.qs'))
g2 <- qread(here::here('models-temp', 'g2.qs'))

model_effects_table(c2) %>% 
  rename(bev_nontesla = est) %>% 
  full_join(
    model_effects_table(g2) %>% 
      rename(bev_tesla = est), 
    by = c('var', 'cat')
  ) %>% 
  full_join(
    model_effects_table(d2) %>% 
      rename(phev = est),    
    by = c('var', 'cat')
  ) %>% 
  full_join(
    model_effects_table(e2, ev = FALSE) %>% 
      rename(hev = est),
    by = c('var', 'cat')
  ) %>% 
  full_join(
    model_effects_table(f2, ev = FALSE) %>% 
      rename(cv = est), 
    by = c('var', 'cat')
  ) %>%
  select(var, cat, bev_nontesla, bev_tesla, phev, hev, cv) %>% 
  # Rearrange rows
  mutate(
    order = case_when(
      cat == 'other' ~ 1,
      var == 'int' ~ 2,
      cat == 'int_my' ~ 3,
      cat == 'int_model' ~ 4,
      TRUE ~ 5
    )
  ) %>%
  arrange(order, desc(cat)) %>%
  select(-order) %>% 
  # # Drop select rows
  # filter(
  #   ! var %in% c(
  #     # BEV
  #     'Fortwo', 'Spark', 'Focus', 'e-Golf', 'Soul', 
  #     # PHEV
  #     'Prius Plug-In', 'Sonata Plug-In Hybrid',
  #     # HEV
  #     'CT', 'Insight', 'MKZ', 'Niro', 'Optima', 'Prius C', 'Prius V',
  #     'Sonata Hybrid',
  #     # CV
  #     "Accord", "Altima", "Camaro", "Charger", "Cruze", "Elantra", "ES", 
  #     "Forte", "Jetta", "Malibu", "Maxima", "Mazda3", "Mustang", "Optima",
  #     "Sentra", "Sonata", "Civic", "Corrolla", "5 Series"
  #   )
  # ) %>% 
  flextable::flextable() %>%
  theme_box() %>%
  print(preview = "docx")

# S6 ----

h2 <- qread(here::here('models-temp', 'h2.qs'))
h3 <- qread(here::here('models-temp', 'h3.qs'))
h4 <- qread(here::here('models-temp', 'h4.qs'))
h5 <- qread(here::here('models-temp', 'h5.qs'))
h6 <- qread(here::here('models-temp', 'h6.qs'))

model_effects_table(h2) %>%
  rename(bev_nontesla = est) %>%
  full_join(
    model_effects_table(h3) %>%
      rename(bev_tesla = est),
    by = c('var', 'cat')
  ) %>%
  full_join(
    model_effects_table(h4) %>%
      rename(phev = est),
    by = c('var', 'cat')
  ) %>%
  full_join(
    model_effects_table(h5, ev = FALSE) %>%
      rename(hev = est),
    by = c('var', 'cat')
  ) %>%
  full_join(
    model_effects_table(h6, ev = FALSE) %>%
      rename(cv = est),
    by = c('var', 'cat')
  ) %>%
  select(var, cat, bev_nontesla, bev_tesla, phev, hev, cv) %>% 
  mutate(
    order = case_when(
      cat == 'other' ~ 1,
      var == 'int' ~ 2,
      cat == 'int_my' ~ 3,
      cat == 'int_model' ~ 4,
      TRUE ~ 5
    )
  ) %>%
  arrange(order, desc(cat)) %>%
  select(-order) %>% 
  flextable::flextable() %>%
  theme_box() %>%
  print(preview = "docx")

# S1 List of all vehicle models in dataset ----

models <- read_csv(here::here('data', 'model_counts_top.csv')) %>% 
    # Fix e-tron (it's an SUV)
    mutate(
        model = ifelse(str_detect(model, 'e-tron'), 'e-tron', model),
        vehicle_type = ifelse(str_detect(model, 'e-tron'), 'suv', vehicle_type)
    ) %>% 
    group_by(powertrain, vehicle_type, make, model) %>% 
    mutate(
        n = sum(n), 
        p = sum(p)
    ) %>% 
    group_by(powertrain, vehicle_type) %>% 
    mutate(cumsum = cumsum(p)) %>% 
    ungroup() %>% 
    mutate(
        n = round(n, 3),
        p = round(p, 3),
        cumsum = round(cumsum, 3)
    )

car_models <- models %>% 
    filter(vehicle_type == 'car') %>% 
    select(powertrain, everything()) %>% 
    select(-vehicle_type)
car_models$powertrain <- factor(
    car_models$powertrain, levels = c("conventional", "hybrid", "phev",'bev')
)
car_models <- car_models %>% 
    arrange(desc(powertrain)) %>% 
    mutate(
        n = scales::comma(n), 
        powertrain = str_to_upper(powertrain), 
        make = ifelse(
            make == 'bmw', 'BMW', ifelse(
                make == 'kia', 'KIA', str_to_title(make))), 
        model = str_replace(model, " plug-in hybrid", ""),
        model = str_replace(model, " hybrid", ""),
        model = ifelse(
            model == 'cr-z', 'CR-Z', ifelse(
            model == 'ct', 'ct', ifelse(
            model == 'elr', 'ELR', ifelse(
            model == 'e-golf', 'e-Golf', ifelse(
            model == 'i8', 'i8', ifelse(
            model == 'bolt ev', 'Bolt', ifelse(
            model == 'es', 'ES', ifelse(
            model == 'mkz', 'MKZ', str_to_title(model))))))))))

# Print in word doc
car_models %>%
  flextable::flextable(car_models) %>%
  theme_box() %>%
  print(preview = "docx")


# Regression tables ----

# S2 ---- 
# Multiple powertrains

# read in models with just powertrain coefs
b1 <- qread(here::here('models-temp', 'b1.qs'))
b2 <- qread(here::here('models-temp', 'b2.qs'))

# make coef tables
summary_b1 <- make_coef_table2(b1) 
summary_b2 <- make_coef_table2(b2)

summary_b1 %>%
  rename(model1 = final) %>% 
  full_join(
    summary_b2 %>% rename(model2 = final), 
    by = "coefficients"
  ) %>%
  mutate(
    order = ifelse(
      coefficients == '(Intercept)', 1, ifelse(
      str_detect(coefficients, "^power"), 2, ifelse(
      coefficients == "age_years", 3, 4
      )))
  ) %>% 
  arrange(order) %>% 
  select(-order) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  add_footer_lines(
    values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1"
  ) %>% 
  align(align = "right", part = "body") %>% 
  print(preview = "docx")

# S3 ----
# Separate powertrains 

# Read in models all powertrains
c2 <- qread(here::here('models-temp', 'c2.qs'))
d2 <- qread(here::here('models-temp', 'd2.qs'))
e2 <- qread(here::here('models-temp', 'e2.qs'))
f2 <- qread(here::here('models-temp', 'f2.qs'))
g2 <- qread(here::here('models-temp', 'g2.qs'))

summary_c2 <- make_coef_table2(c2)
summary_d2 <- make_coef_table2(d2)
summary_e2 <- make_coef_table2(e2)
summary_f2 <- make_coef_table2(f2)
summary_g2 <- make_coef_table2(g2)

summary_c2 %>%
  rename(bev_non_tesla = final) %>% 
  full_join(
    summary_g2 %>% rename(bev_tesla = final), 
    by = "coefficients"
  ) %>%
  full_join(
    summary_d2 %>% rename(phev = final), 
    by = "coefficients"
  ) %>%
  full_join(
    summary_e2 %>% rename(hev = final), 
    by = "coefficients"
  ) %>%
  full_join(
    summary_f2 %>% rename(cv = final), 
    by = "coefficients"
  ) %>% 
  mutate(
    order = ifelse(
      coefficients == '(Intercept)', 1, ifelse(
      str_detect(coefficients, "^model"), 2, ifelse(
      str_detect(coefficients, "^my20"), 3, ifelse(
      str_detect(coefficients, "^subsidy"), 4, ifelse(
      coefficients == "age_years", 5, ifelse(
      coefficients == "miles", 6, ifelse(
      coefficients == "cents_per_mile", 7, ifelse(
      coefficients == "range", 8, 9
    ))))))))
  ) %>% 
  arrange(order) %>% 
  select(-order) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  add_footer_lines(
    values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1"
  ) %>% 
  align(align = "right", part = "body") %>% 
  print(preview = "docx")
