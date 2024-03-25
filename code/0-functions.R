set.seed(456178954)

# Packages

library(tictoc)
library(arrow)
library(tidyverse)
library(data.table)
library(cowplot)
library(scattermore)
library(fixest)
library(broom)
library(logitr)
library(readxl)
library(ggrepel)
library(zipcodeR)
library(lubridate)
library(texreg)
library(here)
library(janitor)
library(qs)
library(viridis)
library(zoo)
library(scales)
options(dplyr.width = Inf)
library(gtsummary)
library(Hmisc)
library(flextable)
library(ggtext)

# Settings

options(dplyr.width = Inf)
options(scipen = 999)
# set_cpu_count(1)
# set_io_thread_count(1)

# Global parameters
source(here::here('code', '0-globals.R'))

save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}

# Functions for making figs ----

# General theme for all plots

plot_theme <- function() {
  return(
    theme_minimal_grid(
      font_family = 'Roboto Condensed',
      font_size = 12
    ) + 
      panel_border() +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "grey80"),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(size = 0.3, colour = "grey90"),
        axis.line.x = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(
          hjust = 1, size = 11, face = "italic"),
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      ) 
  )
}

save_fig <- function(fig, height, width) {
  name <- deparse(substitute(fig))  
  ggsave(
    here::here('figs', paste0(name, '.png')), fig, 
    height = height, width = width
  )
  ggsave(
    here::here('figs', paste0(name, '.pdf')), fig, 
    height = height, width = width, device = cairo_pdf
  )
}

# Functions for cleaning raw data ----

get_match <- function(ymmt, matches) {
  
  # First do perfect join on model_trim
  match_model_trim <- ymmt %>%
    left_join(
      matches %>%
        rename(model_trim = model),
      by = 'model_trim'
    ) %>%
    mutate(matching = "model_trim")
  
  # If all matches are perfect, we're done!
  if (!any(is.na(match_model_trim$key))) {
    return(match_model_trim)
  }
  
  # Now for any missed matches, try match on just 'model'
  match_model <- match_model_trim %>%
    filter(is.na(key)) %>%
    select(year, make, model, trim, powertrain, model_trim) %>%
    left_join(matches, by = 'model') %>%
    mutate(matching = "model")
  
  # If all remaining matches are perfect, we're done!
  if (!any(is.na(match_model$key))) {
    result <- rbind(match_model_trim, match_model) %>%
      filter(!is.na(key))
    return(result)
  }
  
  # Now for any missed matches, try match on just 'trim'
  match_trim <- match_model %>%
    filter(is.na(key)) %>%
    select(year, make, model, trim, powertrain, model_trim) %>%
    left_join(
      matches %>%
        rename(trim = model),
      by = 'trim'
    ) %>%
    mutate(matching = "trim")
  
  # If all remaining matches are perfect, we're done!
  if (!any(is.na(match_model$key))) {
    result <- rbind(match_model_trim, match_model, match_trim) %>%
      filter(!is.na(key))
    return(result)
  }
  
  # Finally, for any remaining missed matches, try fuzzy join on 'model'
  match_fuzzy <- stringdist_join(
    x = match_trim %>%
      filter(is.na(key)) %>%
      select(year, make, model, trim, powertrain, model_trim),
    y = matches,
    by = 'model',
    mode = 'left',
    method = 'jw',
    max_dist = 99,
    distance_col = 'dist'
  ) %>%
    arrange(model.x, dist) %>%
    filter(dist <= 0.3) %>% # Match threshold for similarity
    select(-dist, -model.y) %>%
    rename(model = model.x) %>%
    group_by(year, make, model, trim) %>%
    slice(1) %>%
    mutate(matching = "fuzzy")
  result <- rbind(
    match_model_trim, match_model, match_trim, match_fuzzy) %>%
    filter(!is.na(key))
  return(result)
}

compute_fuel_cost <- function(dt) {
  
  # Join range, mpg, and motor efficiencies 
  
  dt <- dt %>% 
    left_join(read_parquet(here::here('data', 'dict_final.parquet')), by = c(
      'powertrain', 'vehicle_type', 'year', 'make', 'model', 
      'trim')
    )
  
  # Compute cents_per_mile
  
  dt <- dt %>% 
    mutate(
      cents_per_mile = 100 * gas_price / mpg, 
      cents_per_mile = ifelse(
        powertrain == 'phev',
        phev_uf*((elec_price*kwhp100mi / 100) + (gas_price*gal100mi / 100)) + 
          (1 - phev_uf)*cents_per_mile,
        cents_per_mile
      ),
      cents_per_mile = ifelse(
        powertrain == 'bev', 
        elec_price*kwhp100mi / 100, 
        cents_per_mile
      )
    ) %>% 
    filter(!is.na(cents_per_mile)) %>% 
    filter(cents_per_mile != Inf) %>%
    select(-kwhp100mi, -gal100mi, -phev_uf)
  
  return(dt)
  
}

# Function to harmonize factor levels for powertrain

set_powertrain_levels <- function(df) {
  df$powertrain_label <- df$powertrain
  df <- df %>%
    mutate(
      powertrain_label = fct_recode(
        powertrain_label,
        "Conventional" = 'conventional',
        'Hybrid' = 'hybrid',
        "Plug-in Hybrid" = 'phev',
        "Battery Electric" = "bev"
      ),
      powertrain_label = fct_relevel(
        powertrain_label, 
        c("Conventional", "Hybrid", "Plug-in Hybrid", "Battery Electric"))
    )
  return(df)
}

# Functions for formatting effects for tables ----

make_coef_table2 <- function(model) {
  
  summary <- as.data.frame(coeftable((summary(model)))) %>%
    round(3) %>%
    rownames_to_column() %>%
    rename(
      "coefficients" = "rowname",
      "prob" = "Pr(>|t|)") %>%
    mutate(
      sig = ifelse(
        prob <= 0.001,'***', ifelse(
          prob > 0.001 & prob <= 0.01, '**', ifelse(
            prob > 0.01 & prob <= 0.05, '*', ifelse(
              prob > 0.05 & prob <= 0.1, '.', '   ')))),
      sig2 = ifelse(
        prob <= 0.001,'***', ifelse(
          prob > 0.001 & prob <= 0.01, '**', ifelse(
            prob > 0.01 & prob <= 0.05, '*', ifelse(
              prob > 0.05 & prob <= 0.1, '.', "XXX"))))
    ) %>%
    mutate(
      estimate = sprintf("%.3f", Estimate),
      se = sprintf("%2.3f", `Std. Error`),
      se = paste0("(", se, ")"),
      final = paste0(estimate, sig, '\n', se)
    ) %>%
    select(coefficients, final)
  return(summary)
}

format_int_slope_effects <- function(df) {
  df <- df %>% 
    rownames_to_column(var = 'var') %>% 
    mutate(
      estimate = round(100*mean, 2), 
      se = round(100*sd, 2), 
      est = paste0(estimate, '\n(', se, ')')
    ) %>% 
    select(var, est)
  return(df)
}

int_slope_table <- function(model) {
  df <- format_int_slope_effects(compute_ints(model, 'powertrain')) %>% 
    rename(int = est) %>% 
    left_join(
      format_int_slope_effects(compute_age_effects(model)) %>% 
        rename(slope = est), 
      by = 'var'
    )
  return(df)
}

# Functions for examining and computing estimated effects ----

age_years_coefs <- function(m) {
  coefs <- coef(m)[which(str_detect(names(coef(m)), 'age_years'))]
  return(data.frame(coefs))
}

get_coef_draws <- function(model, n_draws = 10^4) {
  coefs <- coef(model)
  covariance <- vcov(model)
  return(as.data.frame(MASS::mvrnorm(n_draws, coefs, covariance)))
}

print_effects_summary <- function(model) {
  x <- exp(coef(model)) - 1
  cat('10k miles increase:', get_effect(x, 'miles', 10), '\n') 
  cat('1 cent per mile increase:', get_effect(x, 'cents_per_mile', 1), '\n')
  cat('10 miles range increase:', get_effect(x, 'range', 10), '\n')
  cat('10 more days on market:', get_effect(x, 'dom_listing', 10), '\n')
  cat('$7500 subsidy:', get_effect(x, 'subsidy_total_listing', 7.5), '\n')
}

get_effect <- function(x, name, multiplier = 1) {
  return(scales::percent(multiplier*x[name], accuracy = 0.01))
}

compute_age_effects <- function(model) {
  draws <- get_coef_draws(model)
  vars <- names(draws)
  age_vars <- vars[str_detect(vars, 'age_years:')]
  age_base <- draws['age_years']
  age_base_mat <- as.data.frame(repmat(as.matrix(age_base), 1, length(age_vars)))
  age_effects <- age_base_mat + draws[,age_vars]
  age_effects <- cbind(age_base, age_effects)
  names(age_effects) <- c('base', str_replace(age_vars, 'age_years:', ''))
  age_effects <- 1 - exp(age_effects)
  result <- ci(age_effects)
  result$sd <- apply(age_effects, 2, sd)
  return(result)
}

compute_ints <- function(model, intname) {
  draws <- get_coef_draws(model)
  vars <- names(draws)
  int_vars <- c(
    vars[str_detect(vars, intname) & !str_detect(vars, 'age_years:')]
  )
  int_base <- draws['(Intercept)']
  int_base_mat <- as.data.frame(repmat(as.matrix(int_base), 1, length(int_vars)))
  int_effects <- int_base_mat + draws[,int_vars]
  int_effects <- cbind(int_base, int_effects)
  names(int_effects) <- c('base', int_vars)
  int_effects <- exp(int_effects)
  result <- ci(int_effects)
  result$sd <- apply(int_effects, 2, sd)
  return(result)
}

format_age_effects <- function(model, base_name, pt, vehicles) {
  m <- qread(here::here('models-temp', paste0(model, '.qs')))
  age <- compute_age_effects(m)
  age$model <- row.names(age)
  age$model[1] <- base_name
  row.names(age) <- NULL
  age <- age %>% 
    mutate(model = str_replace(age$model, 'model', '')) %>% 
    left_join(vehicles %>% filter(powertrain == pt)) %>% 
    make_vehicle_var()
  return(age)
}

format_int_effects <- function(model, base_name, pt, vehicles) {
  m <- qread(here::here('models-temp', paste0(model, '.qs')))
  int <- compute_ints(m, 'model')
  int$model <- row.names(int)
  int$model[1] <- base_name
  row.names(int) <- NULL
  int <- int %>% 
    mutate(model = str_replace(int$model, 'model', '')) %>% 
    left_join(vehicles %>% filter(powertrain == pt)) %>% 
    make_vehicle_var()
  return(int)
}

adjust_var_labels <- function(df) {
  df <- df %>% 
    mutate(
      var = str_replace(var, 'model', ''), 
      var = str_to_title(var), 
      var = str_replace(var, 'Ev', 'EV'), 
      var = str_replace(var, 'E-Golf', 'e-Golf'),
      var = str_replace(var, 'I3', 'i3'), 
      var = str_replace(var, 'My', 'MY '), 
      var = str_replace(var, 'Es', 'ES'), 
      var = str_replace(var, 'Ct', 'CT'), 
      var = str_replace(var, 'Mkz', 'MKZ')
    )
  return(df)
}

model_effects_table <- function(model, ev = TRUE) {
  
  # Age
  age <- compute_age_effects(model) %>% 
    format_int_slope_effects() %>% 
    adjust_var_labels()
  
  # Ints model
  int_effects_model <- compute_ints(model, 'model') %>% 
    filter()
  int <- format_int_slope_effects(int_effects_model)[1,]
  int$var <- 'int'
  int_effects_model$mean <- int_effects_model$mean - int_effects_model$mean[1]
  int_effects_model <- int_effects_model[2:nrow(int_effects_model),] 
  ints_model <- format_int_slope_effects(int_effects_model) %>% 
    adjust_var_labels()
  
  # Ints model year
  int_effects_my <- compute_ints(model, 'my')
  int_effects_my$mean <- int_effects_my$mean - int_effects_my$mean[1]
  ints_my <- format_int_slope_effects(int_effects_my)[2:nrow(int_effects_my),] %>% 
    adjust_var_labels()
  
  # Other effects
  draws <- get_coef_draws(model)
  effects <- (exp(draws) - 1) %>% 
    select(
      -`(Intercept)`,
      -starts_with('age_years'), 
      -starts_with('model'), 
      -starts_with('my')
    ) %>% 
    mutate(
      miles = 10*miles, 
      dom_listing = 10*dom_listing
    )
  if (ev) {
    effects <- effects %>% 
      mutate(
        range = 10*range,
        subsidy_total_listing = 7.5*subsidy_total_listing
      )
  }  
  other <- ci(effects)
  other$sd <- apply(effects, 2, sd) 
  other <- format_int_slope_effects(other)
  
  # Combine
  int$cat <- 'int'
  age$cat <- 'age_slope'
  ints_model$cat <- 'int_model'
  ints_my$cat <- 'int_my'
  other$cat <- 'other'
  
  rbind(int, age, ints_model, ints_my, other)
}

make_vehicle_var <- function(df) {
  df <- df %>% 
    mutate(
      make = ifelse(
        make == 'bmw', 'BMW', ifelse(
          make == 'kia', 'KIA', str_to_title(make))), 
      model = ifelse(
        model == 'es', 'ES', ifelse(
          model == 'ct', 'CT', ifelse(
            model == 'bolt ev', 'Bolt', ifelse(
              model == 'mkz', 'MKZ', str_to_title(model))))), 
      make = ifelse(powertrain == 'conventional', paste0(' ', make), make),
      vehicle = paste0(make, " ", model)
    )
  return(df)
}

format_powertrain_var <- function(df) {
  df <- df %>% 
    mutate(
      powertrain = str_to_title(powertrain),
      powertrain = ifelse(
        powertrain %in% c('Bev', 'Phev'), str_to_upper(powertrain),
        powertrain
      ), 
      powertrain = fct_relevel(powertrain, c(
        'Conventional', 'Hybrid', 'PHEV', 'BEV'
      )) 
    )
  return(df)
}

make_predictions <- function(model, df) {
  m <- qread(here::here('models-temp', paste0(model, '.qs')))  
  pred <- cbind(df, exp(predict(m, df, interval = 'confidence')))
  return(pred)
}

# Function for saving results of a large model ----

save_model_results <- function(model) {
  
  name <- deparse(substitute(model))
  
  if(!dir.exists(here::here('models'))) {
    dir.create(here::here('models'))
  }
  
  # Save full model in temp dir (ignored on github)
  qsave(model, here::here('models-temp', paste0(name, '.qs')))
  
  # Save only model results in models dir (pushed to github)
  qsave(
    list(
      coefs = coef(model), 
      stats = glance(model), 
      summary = tidy(model)
    ),
    file = here::here('models', paste0(name, '.qs'))
  )
  
}

# Functions for loading data ----

load_dt <- function(
    pt = c('bev', 'phev', 'hybrid', 'conventional'),
    vt = c('car', 'suv', 'pickup')
) {
  dt <- ds <- open_dataset(PATH_DB) %>%
      filter(listing_year < 2020) %>%
      filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) %>%
      filter(inventory_type == 'used') %>%
      filter(powertrain %in% pt) %>%
      filter(vehicle_type %in% vt) %>%
      select(
            zip, state, make, model, trim, year, powertrain, vehicle_type,
            listing_year, status_date, dom_listing, age_years, miles, 
            price, msrp, cents_per_mile, class, range
      ) %>%
      collect() %>% 
      common_cleaning()
  return(dt)
}

sample_cv <- function(dt, p_cv) {
    dt_cv <- dt %>%
        filter(powertrain == 'conventional') %>%
        group_by(model) %>%
        sample_frac(p_cv)
    dt <- dt %>%
        filter(powertrain != 'conventional') %>%
        rbind(dt_cv)
    return(dt)
}

load_dt_car <- function() {
    dt <- load_dt(vt = 'car')
}

load_dt_pev_car <- function() {
    return(load_dt(pt = c('bev', 'phev'), vt = 'car'))
}

load_dt_bev <- function() {
    return(load_dt(pt = 'bev', vt = c('car', 'suv')))
}

load_dt_bev_car <- function() {
    dt <- load_dt(pt = 'bev', vt = 'car')

    # Set model levels based on lowest to highest range
    dt$model <- factor(
        dt$model, levels = c(
            "leaf", "fortwo", "spark", "focus", "i3", "e-golf", "500e",
            "soul", "bolt ev", "model 3",  "model s"
        )
    )
    return(dt)
}

load_dt_bev_car_nontesla <- function() {
  dt <- load_dt_bev_car() %>% 
    filter(tesla == 0)
  dt$model <- factor(
    dt$model, 
    levels = c(
      "leaf", "fortwo", "spark", "focus", "i3", 
      "e-golf", "500e", "soul", "bolt ev"    
    )
  )
  return(dt)
}

load_dt_bev_car_tesla <- function() {
  dt <- load_dt_bev_car() %>% 
    filter(tesla == 1)
  dt$model <- factor(
    dt$model, 
    levels = c("model s", "model 3")
  )
  return(dt)
}

load_dt_hev <- function() {
    return(load_dt(pt = 'hybrid'))
}

load_dt_hev_car <- function() {
    return(load_dt(pt = 'hybrid', vt = 'car'))
}

load_dt_phev <- function() {
    return(load_dt(pt = 'phev'))
}

load_dt_phev_car <- function() {
  dt <- load_dt(pt = 'phev', vt = 'car')
  
  # Set model levels based on lowest to highest range
  dt$model <- factor(
    dt$model, levels = c(
      "prius prime", "prius plug-in", "sonata plug-in hybrid", "volt"
    )
  )
  return(dt)
}

load_dt_cv <- function() {
    return(load_dt(pt = 'conventional'))
}

load_dt_cv_car <- function() {
    return(load_dt(pt = 'conventional', vt = 'car'))
}

load_dt_cv_car_top4 <- function() {
  return(
    load_dt(pt = 'conventional', vt = 'car') %>% 
      # Only including top 4 models
      filter(model %in% c('camry', 'corolla', 'civic', 'accord'))
  )
}

separate_bev_tesla <- function(dt) {
  dt <- dt %>% 
    mutate(
      powertrain = as.character(powertrain),
      powertrain = ifelse(make == 'tesla', 'bev_tesla', powertrain)
    )
  dt$powertrain <- factor(
    dt$powertrain, levels = c(
      "conventional", "hybrid", "phev", 'bev', 'bev_tesla'
    )
  )  
  return(dt)
}

# Common cleaning steps across any datasets

common_cleaning <- function(dt) {

    # Change miles to thousands
    dt$miles <- dt$miles / 1000

    # Make Tesla dummy
    dt <- dt %>%
        mutate(tesla = ifelse(make == "tesla", 1, 0))
    
    # Remove MY 2009 - 2011 as they have no observations for BEVs
    dt <- dt %>% 
      filter(year > 2011)
    
    # Make model year variable as factor
    dt$my <- as.factor(dt$year)
    # dt$my <- factor(dt$my, levels = rev(levels(dt$my)))

    # Compute RR and ARR (for PEVs that have a subsidy)
    dt <- compute_rr(dt)
    
    # Filter out luxury cars
    dt <- dt %>%  
      filter(make != 'porsche')

    # Fix Fusion coding error 
    dt <- dt %>% 
      mutate(
        model = ifelse(
          (powertrain == 'hybrid' & model == 'fusion') | 
          (powertrain == 'hybrid' & model == 'fusion hybrid'), 
          'fusion', 
          model
        )
      )
    
    # Set common reference levels for factor variables
    dt <- set_reference_levels(dt)

    return(dt)
}

compute_rr <- function(dt) {

  dt <- dt %>%
    filter(!is.na(msrp)) %>%
    mutate(rr = price / msrp)
  
  if (has_pevs(dt)) {
    dt <- add_arr(dt)
  }
  
  return(dt)
}

has_pevs <- function(dt) {
  powertrains <- unique(dt$powertrain)
  return(('bev' %in% powertrains) | ('phev' %in% powertrains))
}

add_arr <- function(dt) {
  
  # Separate out PHEVs (only ones that get a subsidy)
  
  dt_pev <- dt %>% filter(powertrain %in% c('bev', 'phev'))
  dt_other <- dt %>% filter(! powertrain %in% c('bev', 'phev'))
  
  # Compute federal subsidy
  
  dt_pev <- compute_fed_sub(dt_pev)

  # Read in state subsidy data

  state_subsidy_data <- read_csv(here("data", "pev_state_policies.csv")) %>%
    select(
      statecode, year, bevvehsubdum_year, bevamt_year,  bevvehsub_MSRPmax,
      bevvehsub_minmiles, bevannualfeedum_year, bevannualfeeamt_year
    ) %>%
    mutate(
      bevvehsub_MSRPmax = ifelse(
        is.na(bevvehsub_MSRPmax) | bevvehsub_MSRPmax == 0,
        NA,
        bevvehsub_MSRPmax
      )
    )

  # Join subsidies and compute ARR

  dt_pev <- dt_pev %>%
    # Join with state and federal subsidy document
    left_join(state_subsidy_data, by = c("state" = "statecode", "year")) %>%
    # Calculate total subsidy
    mutate(
      subsidy_state = ifelse(
        is.na(bevvehsub_MSRPmax) | bevvehsub_MSRPmax == 0,
        bevamt_year, ifelse(
        msrp < bevvehsub_MSRPmax, bevamt_year, 0)
      ),
      subsidy_state = ifelse(is.na(subsidy_state), 0, subsidy_state),
      subsidy_total = subsidy_state + subsidy_fed,
      # Calculate ARR
      arr = price / (msrp - subsidy_total)
    ) %>% 
    select(
      -bevvehsubdum_year, -bevamt_year, -bevvehsub_MSRPmax, 
      -bevvehsub_minmiles, -bevannualfeedum_year, -bevannualfeeamt_year
    )
  
  # Re-join subsidies for computing subsidy at listing
  
  dt_pev <- dt_pev %>%
    # Join with state and federal subsidy document
    left_join(state_subsidy_data, by = c(
      "state" = "statecode", "listing_year" = "year")) %>%
    # Calculate total subsidy
    mutate(
      subsidy_state_listing = ifelse(
        is.na(bevvehsub_MSRPmax) | bevvehsub_MSRPmax == 0,
        bevamt_year, ifelse(
          msrp < bevvehsub_MSRPmax, bevamt_year, 0)
      ),
      subsidy_state_listing = ifelse(
        is.na(subsidy_state_listing), 0, subsidy_state_listing),
      subsidy_total_listing = subsidy_state_listing + subsidy_fed_listing,
    ) %>% 
    select(
      -bevvehsubdum_year, -bevamt_year, -bevvehsub_MSRPmax, 
      -bevvehsub_minmiles, -bevannualfeedum_year, -bevannualfeeamt_year
    )
  
  # Convert units
  dt_pev <- dt_pev %>% 
    mutate(
      subsidy_total_listing = subsidy_total_listing / 10^3,
      subsidy_state = subsidy_state / 10^3,
      subsidy_fed = subsidy_fed / 10^3,
      subsidy_total = subsidy_total / 10^3,
      subsidy_state_listing = subsidy_state_listing / 10^3,
      subsidy_fed_listing = subsidy_fed_listing / 10^3
    )
  
  # Add missing vars to dt_other (if any)
  
  if (nrow(dt_other) > 1) {
    dt_other$subsidy_state <- 0
    dt_other$subsidy_fed <- 0
    dt_other$subsidy_total <- 0
    dt_other$subsidy_state_listing <- 0
    dt_other$subsidy_fed_listing <- 0
    dt_other$subsidy_total_listing <- 0
    dt_other$arr <- dt_other$rr
    dt <- rbind(dt_pev, dt_other)
  } else {
    dt <- dt_pev
  }

  return(dt)

}

compute_fed_sub <- function(dt) {
  
  # First need to estimate the battery capacity to get subsidy:
  # $417*capacity up to max of $7500
  dt <- dt %>% 
    left_join(
    read_parquet(here::here('data', 'dict_final.parquet')) %>% 
        select(
          powertrain, vehicle_type, year, make, model, trim, kwhp100mi,
          phev_uf
        ), 
      by = c('powertrain', 'vehicle_type', 'year', 'make', 'model', 'trim')
    ) %>% 
    mutate(
      battery_cap_est = kwhp100mi*(range/100)*(1+(1-phev_uf)),
      subsidy_fed = 417*battery_cap_est, 
      subsidy_fed = ifelse(is.na(subsidy_fed), 7500, subsidy_fed),
      subsidy_fed = ifelse(subsidy_fed > 7500, 7500, subsidy_fed), 
      subsidy_fed_listing = subsidy_fed
    )
  
  # Now check cases of federal subsidy phase outs:
  
  # chevrolet	2019-04-01	3750
  # chevrolet	2019-10-01	1875
  # chevrolet	2020-03-31	0
  # tesla   	2019-01-01	3750
  # tesla   	2019-06-01	1875
  # tesla   	2019-12-31	0
  
  dt <- dt %>% 
    mutate(subsidy_fed = case_when(
      (make == 'chevrolet') & (year == 2019) ~ subsidy_fed / 2, 
      (make == 'chevrolet') & (year > 2019) ~ 0, 
      (make == 'tesla') & (year == 2019) ~ subsidy_fed / 2, 
      (make == 'chevrolet') & (year > 2019) ~ 0,
      TRUE ~ subsidy_fed
    )) %>% 
    mutate(subsidy_fed_listing = case_when(
      (make == 'chevrolet') & 
        between(status_date, ymd('2019-04-01'), ymd('2019-09-30')) ~ subsidy_fed / 2, 
      (make == 'chevrolet') & 
        between(status_date, ymd('2019-10-01'), ymd('2020-03-31')) ~ subsidy_fed / 4, 
      (make == 'chevrolet') & (status_date > ymd('2020-04-01')) ~ 0, 
      (make == 'tesla') & 
        between(status_date, ymd('2019-01-01'), ymd('2019-05-31')) ~ subsidy_fed / 2, 
      (make == 'tesla') & 
        between(status_date, ymd('2019-06-01'), ymd('2019-12-31')) ~ subsidy_fed / 4, 
      (make == 'tesla') & (status_date > ymd('2020-01-01')) ~ 0, 
      TRUE ~ subsidy_fed_listing
    ))

  return(dt %>% select(-kwhp100mi, -phev_uf, -battery_cap_est))
}

set_reference_levels <- function(dt) {
  
  # Set "car" as the reference level for vehicle_type
  
  dt$vehicle_type <- factor(
    dt$vehicle_type, levels = c("car", "suv", "pickup")
  )
  
  # Set "small" as the reference level for class
  
  dt$class <- factor(
    dt$class, levels = c("small", "midsize", "large", "suv")
  )
  
  # Set "conventional" as the reference level for powertrain
  
  dt$powertrain <- factor(
    dt$powertrain, levels = c("conventional", "hybrid", "phev",'bev')
  )
  
  return(dt)
  
}

# R equivalent of matlab's repmat function
repmat <- function(X, m, n) {
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T))
}

load_dt_all <- function(
    pt = c('bev', 'phev', 'hybrid', 'conventional'),
    vt = c('car', 'suv', 'pickup')
) {
  dt <- ds <- open_dataset(PATH_DB) %>%
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) %>%
    filter(inventory_type == 'used') %>%
    filter(powertrain %in% pt) %>%
    filter(vehicle_type %in% vt) %>%
    select(
      zip, state, make, model, trim, year, powertrain, vehicle_type,
      listing_year, status_date, dom_listing, age_years, miles, 
      price, msrp, cents_per_mile, class, range
    ) %>%
    collect() %>% 
    common_cleaning()
  return(dt)
}

load_dt_covid <- function(
    pt = c('bev', 'phev', 'hybrid', 'conventional'),
    vt = c('car', 'suv', 'pickup')
) {
  dt <- ds <- open_dataset(PATH_DB) %>%
    filter(listing_year >= 2020) %>%
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) %>%
    filter(inventory_type == 'used') %>%
    filter(powertrain %in% pt) %>%
    filter(vehicle_type %in% vt) %>%
    select(
      zip, state, make, model, trim, year, powertrain, vehicle_type,
      listing_year, status_date, dom_listing, age_years, miles, 
      price, msrp, cents_per_mile, class, range
    ) %>%
    collect() %>% 
    common_cleaning()
  return(dt)
}
