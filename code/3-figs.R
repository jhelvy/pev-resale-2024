source(here::here('code', '0-functions.R'))

# FIG 1A ----

color_cv <- "grey42"
color_ev <- "#00BA38" # "forestgreen"
color_tesla <- "#619CFF" # "dodgerblue"

# Read in quantile data, quick formatting

quantiles <- read_csv(here::here('data', 'quantiles.csv')) %>% 
    mutate(age_years = age_months / 12) %>% 
    filter(between(age_years, 1, 8))

# Plot of all powertrain-type combos 

fig1a <- quantiles %>% 
    ggplot() +  
    geom_ribbon(
        aes(
            x = age_years, 
            ymin = rr25, 
            ymax = rr75
        ), 
        fill = color_cv,
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years, 
            y = rr50, 
            group = powertrain
        ),
        color = color_cv
    ) + 
    facet_wrap(vars(powertrain)) + 
    scale_x_continuous(
        breaks = seq(1, 8, 1),
        limits = c(1, 8)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 1, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    plot_theme() + 
    labs(
        x = "Vehicle age (years)", 
        y = 'Vehicle value retention rate'
    )

save_fig(fig1a, width = 11, height = 8)

# FIG 1B ----

quantiles_car <- quantiles %>% 
    filter(vehicle_type == 'car')
quantiles_conventional <- quantiles_car %>% 
    filter(powertrain == 'conventional')
quantiles_other <- quantiles_car %>% 
    filter(powertrain %in% c('hybrid', 'phev')) %>% 
    arrange(powertrain) %>% 
    mutate(cv = FALSE, type = powertrain)
quantiles_bev <- read_csv(here::here('data', 'quantiles_bev.csv')) %>% 
    mutate(
        age_years = age_months / 12, 
        powertrain = 'bev', 
        vehicle_type = 'car',
        cv = FALSE,
        type = ifelse(tesla == 1, 'tesla', 'ev')
    ) %>% 
    filter(age_years >= 1) %>% 
    select(tesla, names(quantiles_other))
quantiles_bev_tesla <- quantiles_bev %>% 
    filter(tesla == 1) %>% 
    select(-tesla)
quantiles_bev_nontesla <- quantiles_bev %>% 
    filter(tesla == 0) %>% 
    select(-tesla)
quantiles_other <- rbind(quantiles_other, quantiles_bev_nontesla, quantiles_bev_tesla)
rep_length <- nrow(quantiles_conventional)
quantiles_conventional <- quantiles_conventional[rep(1:rep_length, 3),]
quantiles_conventional$powertrain <- rep(
    c('bev', 'hybrid', 'phev'), each = rep_length)
quantiles_conventional$cv <- TRUE
quantiles_conventional$type <- 'conventional'
df_fig1b <- rbind(quantiles_other, quantiles_conventional) %>% 
    set_powertrain_levels()

fig1b <- df_fig1b %>%
    ggplot() +  
    geom_ribbon(
        aes(
            x = age_years, 
            ymin = rr25, 
            ymax = rr75, 
            fill = type
        ), 
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years,
            y = rr50,
            color = type, 
            group = type
        )
    ) +
    facet_wrap(vars(powertrain_label)) + 
    scale_x_continuous(
        breaks = seq(1, 8, 1),
        limits = c(1, 8)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 1, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_fill_manual(values = c(
        color_cv, color_ev, color_ev, color_ev, color_tesla)
    ) +
    scale_color_manual(values = c(
        color_ev, color_tesla, color_cv, color_cv, color_ev, 
        color_ev, color_ev, color_ev, color_ev, color_tesla)
    ) +
    plot_theme() +
    labs(
        x = "Vehicle age (years)", 
        y = 'Vehicle value retention rate'
    ) +
    geom_label(
        data = data.frame(
            x = c(5, 3.5, 3, 5, 3, 5, 3), 
            y = c(0.74, 0.95, 0.2, 0.74, 0.35, 0.74, 0.45),
            label = c(
                'Conventional', 'BEV (Tesla)', 'BEV (Non-Tesla)',
                'Conventional', 'PHEV',
                'Conventional', 'Hybrid'
            ),
            powertrain_label = as.factor(c(
                rep('Battery Electric', 3), rep('Plug-in Hybrid', 2),
                rep('Hybrid', 2))
            )
        ),
        mapping = aes(x = x, y = y, label = label, color = label),
        size = 4,
        family = 'Roboto Condensed'
    )

save_fig(fig1b, width = 11, height = 3.5)

# FIG 1C ----

# Fig 1b with only BEVs

fig1c <- df_fig1b %>%
    filter(powertrain == 'bev') %>% 
    ggplot() +
    geom_ribbon(
        aes(
            x = age_years,
            ymin = rr25,
            ymax = rr75,
            fill = type
        ),
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years,
            y = rr50,
            color = type,
            group = type
        )
    ) +
    scale_x_continuous(
        breaks = seq(1, 8, 1),
        limits = c(1, 8)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 1, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    plot_theme() +
    scale_fill_manual(values = c(
        color_cv, color_ev, color_tesla)
    ) +
    scale_color_manual(values = c(
        color_ev, color_tesla, color_cv,
        color_cv, color_ev, color_tesla)
    ) +
    labs(
        x = "Vehicle age (years)",
        y = 'Vehicle value retention rate'
    ) +
    geom_label(
        data = data.frame(
            x = c(5, 3.5, 3), 
            y = c(0.74, 0.95, 0.2),
            label = c(
                'Conventional', 'BEV (Tesla)', 'BEV (Non-Tesla)'
            )
        ),
        mapping = aes(x = x, y = y, label = label, color = label),
        size = 4,
        family = 'Roboto Condensed'
    )

save_fig(fig1c, width = 5, height = 3.8)


# FIG 1D ----

# Fig 1c with ARR instead of RR

quantiles <- read_csv(here::here('data', 'quantiles.csv')) %>% 
    mutate(age_years = age_months / 12) %>% 
    filter(between(age_years, 1, 8)) %>% 
    filter(powertrain == 'conventional') %>% 
    select(-age_months, -vehicle_type)
df_fig1d <- read_csv(here::here('data', 'quantiles_bev.csv')) %>% 
    select(-starts_with("rr")) %>% 
    mutate(age_years = age_months / 12) %>% 
    rename(rr25 = arr25, rr50 = arr50, rr75 = arr75) %>% 
    mutate(powertrain = ifelse(tesla == 1, "bev-tesla", "bev-nontesla")) %>% 
    select(names(quantiles)) %>% 
    rbind(quantiles)

fig1d <- df_fig1d %>%
    ggplot() +
    geom_ribbon(
        aes(
            x = age_years,
            ymin = rr25,
            ymax = rr75,
            fill = powertrain
        ),
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years,
            y = rr50,
            color = powertrain,
            group = powertrain
        )
    ) +
    scale_x_continuous(
        breaks = seq(1, 8, 1),
        limits = c(1, 8)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 1, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    plot_theme() +
    scale_fill_manual(values = c(
        color_ev, color_tesla, color_cv)
    ) +
    scale_color_manual(values = c(
        color_ev, color_tesla, color_ev, color_tesla, color_cv, color_cv)
    ) +
    labs(
        x = "Vehicle age (years)",
        y = 'Vehicle value retention rate'
    ) +
    geom_label(
        data = data.frame(
            x = c(5, 3.7, 3), 
            y = c(0.78, 0.95, 0.35),
            label = c(
                'Conventional', 'BEV (Tesla)', 'BEV (Non-Tesla)'
            )
        ),
        mapping = aes(x = x, y = y, label = label, color = label),
        size = 4,
        family = 'Roboto Condensed'
    )

save_fig(fig1d, width = 5, height = 3.8)


# TYO -----

# Compute the Two-Year-Out RR

dt_tyo <- load_dt_car() %>% 
  filter(between(age_years, 1.75, 2.25)) %>% 
  mutate(
    powertrain = str_to_title(powertrain),
    powertrain = ifelse(make == 'tesla', 'BEV Tesla', as.character(powertrain)), 
    powertrain = ifelse(powertrain == 'Bev', 'BEV non-Tesla', powertrain),
    powertrain = ifelse(powertrain == 'Phev', 'PHEV', powertrain), 
    powertrain = fct_relevel(powertrain, c(
      "Conventional", "Hybrid", 'PHEV', 'BEV non-Tesla', 'BEV Tesla'
    ))
  ) %>% 
  select(year, powertrain, rr)

tyo_dots <- dt_tyo %>% 
  group_by(powertrain, year) %>% 
  summarise(
    rr_median = median(rr),
    rr_75 = fquantile(rr, 0.75),
    rr_25 = fquantile(rr, 0.25)
  ) %>% 
  ggplot(aes(x = as.factor(year), y = rr_median)) + 
  geom_point() +
  geom_errorbar(aes(ymin = rr_25, ymax = rr_75), width = 0.2) +
  facet_wrap(vars(powertrain), nrow = 1) + 
  labs(
    x = "Model Year",
    y = "Median RR at Two Years Old"
  ) + 
  plot_theme()

ggsave(tyo_dots, width = 11, height = 2.75)


tyo_box <- dt_tyo %>% 
  ggplot(aes(x = as.factor(year), y = rr)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(vars(powertrain), nrow = 1) + 
  coord_cartesian(ylim = c(0, 1.25)) +
  labs(
    x = "Model Year",
    y = "Median RR at Two Years Old"
  ) + 
  plot_theme()

save_fig(tyo_box, width = 11, height = 2.75)



# FIG 2 ----

# Comparison of annual depreciation rates by vehicle model

vehicles <- load_dt_car() %>% 
  distinct(powertrain, make, model)
age_bev_nontesla <- format_age_effects('c2', 'leaf', 'bev', vehicles)
age_bev_tesla <- format_age_effects('g2', 'modelmodel s', 'bev', vehicles)
age_phev <- format_age_effects('d2', 'prius prime', 'phev', vehicles)
age_hev <- format_age_effects('e2', 'accord', 'hybrid', vehicles)
age_cv <- format_age_effects('f2', '3 series', 'conventional', vehicles)
age_effects <- rbind(age_bev_nontesla, age_bev_tesla, age_phev, age_hev, age_cv) %>% 
  mutate(vehicle = fct_reorder2(vehicle, powertrain, mean)) %>% 
  format_powertrain_var()

fig2 <- ggplot(age_effects, aes(x = mean, y = vehicle)) +
  geom_point() +
  geom_segment(aes(x = lower, xend = upper, yend = vehicle)) +
  facet_grid(powertrain ~ ., scales = "free_y", space = "free") +
  plot_theme() +
  labs(
    x = 'Annual depreciation rate', 
    y = NULL, 
    title = 'Annual depreciation rate by vehicle model and powertrain (sedans)',
  )

save_fig(fig2, height = 11, width = 7)

# # FIG 3 ----
# 
# # Comparison of initial one-year depreciation drop by vehicle model
# 
# vehicles <- load_dt_car() %>% 
#   distinct(powertrain, make, model)
# int_bev <- format_int_effects('c1', 'leaf', 'bev', vehicles)
# int_phev <- format_int_effects('d1', 'prius prime', 'phev', vehicles)
# int_hev <- format_int_effects('e1', 'accord', 'hybrid', vehicles)
# int_cv <- format_int_effects('f1', '3 series', 'conventional', vehicles)
# df_fig3 <- rbind(int_bev, int_phev, int_hev, int_cv) %>% 
#   mutate(vehicle = fct_reorder2(vehicle, powertrain, -mean)) %>% 
#   format_powertrain_var()
# 
# fig3 <- ggplot(df_fig3, aes(x = mean, y = vehicle)) +
#   geom_point() +
#   geom_segment(aes(x = lower, xend = upper, yend = vehicle)) +
#   facet_grid(powertrain ~ ., scales = "free_y", space = "free") +
#   plot_theme() +
#   scale_x_reverse(limits = c(1.1, 0.2), breaks = seq(1, 0.2, -0.2)) +
#   labs(
#     x = 'Retention rate', 
#     y = NULL, 
#     title = 'Initial sale retention rate by vehicle model and powertrain (sedans)',
#   )
# 
# save_fig(fig3, height = 11, width = 7)


# FIG 4 ----

# Predicted depreciation by powertrain for every make model

# To make a prediction, must first load the original data
# Then create the prediction data
# Then make the prediction 

# BEV Non-Tesla

dt_bev_car_nontesla <- load_dt_bev_car_nontesla()
pred_dt_bev_car_nontesla <- dt_bev_car_nontesla %>% 
  group_by(make, model) %>% 
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing), 
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>% 
  mutate(
    age_years = 1,
    powertrain = 'BEV', 
    miles = 0
  )
pred_dt_bev_car_nontesla <- pred_dt_bev_car_nontesla %>%
  rbind(pred_dt_bev_car_nontesla %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_bev_car_nontesla %>% mutate(age_years = 3))
pred_bev_nontesla <- make_predictions('c1', pred_dt_bev_car_nontesla)
rm(dt_bev_car_nontesla)

# BEV Tesla

dt_bev_car_tesla <- load_dt_bev_car_tesla()
pred_dt_bev_car_tesla <- dt_bev_car_tesla %>% 
  group_by(make, model) %>% 
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing), 
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>% 
  mutate(
    age_years = 1,
    powertrain = 'BEV', 
    miles = 0
  )
pred_dt_bev_car_tesla <- pred_dt_bev_car_tesla %>%
  rbind(pred_dt_bev_car_tesla %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_bev_car_tesla %>% mutate(age_years = 3))
pred_bev_tesla <- make_predictions('g1', pred_dt_bev_car_tesla)
rm(dt_bev_car_tesla)

pred_bev <- rbind(pred_bev_nontesla, pred_bev_tesla)

# PHEV

dt_phev_car <- load_dt_phev_car()
pred_dt_phev_car <- dt_phev_car %>% 
  group_by(make, model) %>% 
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing), 
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>% 
  mutate(
    age_years = 1,
    powertrain = 'PHEV', 
    miles = 0,
  )
pred_dt_phev_car <- pred_dt_phev_car %>%
  rbind(pred_dt_phev_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_phev_car %>% mutate(age_years = 3))
pred_phev <- make_predictions('d1', pred_dt_phev_car)
rm(dt_phev_car)

# HEV

dt_hev_car <- load_dt_hev_car()
pred_dt_hev_car <- dt_hev_car %>% 
  group_by(make, model) %>% 
  summarise(
    dom_listing = mean(dom_listing), 
    cents_per_mile = median(cents_per_mile),
  ) %>% 
  mutate(
    age_years = 1,
    miles = 0,
    powertrain = 'Hybrid')
pred_dt_hev_car <- pred_dt_hev_car %>%
  rbind(pred_dt_hev_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_hev_car %>% mutate(age_years = 3))
pred_hev <- make_predictions('e1', pred_dt_hev_car)
rm(dt_hev_car)

# CV

dt_cv_car <- load_dt_cv_car()
pred_dt_cv_car <- dt_cv_car %>% 
  group_by(make, model) %>% 
  summarise(
    dom_listing = mean(dom_listing), 
    cents_per_mile = median(cents_per_mile),
  ) %>% 
  mutate(
    age_years = 1,
    miles = 0,
    powertrain = 'Conventional')
pred_dt_cv_car <- pred_dt_cv_car %>%
  rbind(pred_dt_cv_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_cv_car %>% mutate(age_years = 3))
pred_cv <- make_predictions('f1', pred_dt_cv_car)
rm(dt_cv_car)

# Make the data for the figure

df_fig4 <- rbind(pred_bev, pred_phev, pred_hev, pred_cv) %>% 
  mutate(
    make = ifelse(powertrain == 'Conventional', paste0(' ', make), make)
  ) %>% 
  make_vehicle_var() %>% 
  ungroup() %>% 
  mutate(
    powertrain = fct_relevel(powertrain, c(
      'Conventional', 'Hybrid', 'PHEV', 'BEV'
    )),
    age_years = as.factor(age_years)
  ) %>%
  arrange(powertrain, age_years, fit) %>% 
  mutate(vehicle = fct_inorder(vehicle)) %>% 
  select(mean = fit, everything()) %>% 
  select(
    -se.fit, -ci_low, -ci_high, -range, -cents_per_mile,
    -subsidy_total_listing
  ) 

# Save figure data

write_parquet(df_fig4, here::here('data', 'df_fig4.parquet'))

# Make the figure

df_fig4 <- read_parquet(here::here('data', 'df_fig4.parquet'))
plotColors <- RColorBrewer::brewer.pal(3, "Set1")[c(1, 2, 3)]
fig4 <- df_fig4 %>% 
  pivot_wider(values_from = mean, names_from = age_years) %>% 
  ggplot(aes(y = vehicle, yend = vehicle)) +
  geom_vline(xintercept = 1) + 
  geom_segment(
    aes(x = `1`, xend = `3`), 
    color = 'grey65'
  ) +
  geom_point(
    data = df_fig4,
    aes(x = mean, color = age_years)
  ) +
  facet_grid(
    powertrain ~ ., 
    scales = "free_y", 
    space = "free"
  ) +
  scale_x_reverse(
    limits = c(1.05, 0.2), 
    breaks = seq(1, 0.2, -0.2)
  ) +
  scale_color_manual(values = plotColors) +
  plot_theme() +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Retention rate', 
    y = NULL, 
    title = 'Predicted retention rate of car models at ages 1, 2, and 3 years old',
    # subtitle = 'Predictions made with zero mileage and sample means for operating cost, driving range, days on market, and new market subsidy',
    color = 'Age (years)'
  )

save_fig(fig4, height = 11, width = 7)



# FIG 5 ----

# Predicted depreciation by powertrain

# Load data used to estimate model

dt_car_tesla <- load_dt_car() %>% 
  separate_bev_tesla()

# Read in estimated model

x <- seq(1, 8, by = 0.1)
pred_df <- data.frame(
  powertrain = rep(
    c('bev', 'bev_tesla', 'phev', 'hybrid', 'conventional'), 
    each = length(x)
  ),
  age_years = rep(x, 5)
)
df_fig5 <- make_predictions('b2', pred_df) %>% 
  format_powertrain_var() %>% 
  mutate(
    powertrain = fct_recode(
      powertrain, 
      'BEV Tesla' = 'Bev_tesla', 
      'BEV Non-Tesla' = 'BEV'
    )
  )
rm(dt_car_tesla)

# Save data 

write_parquet(df_fig5, here::here('data', 'df_fig5.parquet'))

# Make the figure

df_fig5 <- read_parquet(here::here('data', 'df_fig5.parquet'))
plotColors <- RColorBrewer::brewer.pal(5, "Set1")
fig5 <- df_fig5 %>% 
  ggplot() +
  geom_ribbon(
    aes(x = age_years, ymin = ci_low, ymax = ci_high, fill = powertrain),
    alpha = 0.5
  ) +
  geom_line(aes(x = age_years, y = fit, color = powertrain)) +
  geom_text_repel(
    data = df_fig5 %>% 
      filter(age_years == max(age_years)),
    aes(x = age_years, y = fit, label = powertrain, color = powertrain),
    hjust = 0, 
    direction = 'y', 
    segment.color = NA, 
    nudge_x = 0.15, 
    family = 'Roboto Condensed'
  ) +
  plot_theme() +
  scale_x_continuous(breaks = seq(1, 8)) +
  coord_cartesian(xlim = c(1, 9.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(
    x = 'Vehicle age (years)', 
    y = 'Retention rate', 
    title = 'Effect of age on predicted retenion rate by powertrain'
  ) +
  scale_color_manual(values = plotColors, 2) +
  scale_fill_manual(values = plotColors)

save_fig(fig5, width = 7, height = 4)


# FIG 6 ----

# Predicted depreciation by powertrain, accounting for model year effects

dt_car_tesla <- load_dt_car() %>% 
  separate_bev_tesla() %>% 
  select(rr, age_years, my, miles, cents_per_mile, powertrain, make, model)

pred_dt_my <- dt_car_tesla %>% 
  group_by(powertrain, my) %>% 
  summarise(cents_per_mile = mean(cents_per_mile)) %>% 
  distinct(powertrain, my, cents_per_mile) %>% 
  mutate(
    miles = 0,
    age_years = 2,
    powertrain = powertrain
  )

df_fig6 <- make_predictions('b3', pred_dt_my) %>% 
  format_powertrain_var() %>% 
  mutate(
    my = as.numeric(as.character(my)),
    powertrain = fct_recode(
      powertrain, 
      'BEV Tesla' = 'Bev_tesla', 
      'BEV Non-Tesla' = 'BEV'
    )
  )

# Save data 

write_parquet(df_fig6, here::here('data', 'df_fig6.parquet'))

# Make the figure 

df_fig6 <- read_parquet(here::here('data', 'df_fig6.parquet'))
fig6 <- df_fig6 %>% 
  mutate(my = ymd(paste(my, '01', '01', sep = '-'))) %>% 
  ggplot(aes(x = my, y = fit)) +
  geom_hline(yintercept = 1) + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0) +
  geom_point() +
  facet_wrap(vars(powertrain), nrow = 1) +
  plot_theme() +
  theme(legend.position = 'right') +
  scale_x_date(
    date_breaks = '1 year',
    date_labels = "'%y"
  ) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2)) +
  labs(
    x = 'Model year', 
    y = 'Retention rate', 
    title = 'Predicted two-year retenion rate by powertrain and model year',
    subtitle = 'Predictions made with zero mileage and mean operating cost across all models.'
  )

save_fig(fig6, width = 11, height = 3)



# FIG 7 ----

# Predicted depreciation for select make models accounting for model years

# To make a prediction, must first load the original data
# Then create the prediction data
# Then make the prediction 

dt_bev_car <- load_dt_bev_car()
pred_dt_bev_car <- dt_bev_car %>% 
  filter(model %in% c('leaf', 'model s')) %>% 
  group_by(make, model, tesla, my) %>% 
  summarise(
    range = mean(range), 
    cents_per_mile = mean(cents_per_mile), 
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>% 
  mutate(
    age_years = 1,
    powertrain = 'BEV', 
    miles = 0,
  )
pred_dt_bev_car <- pred_dt_bev_car %>%
  rbind(pred_dt_bev_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_bev_car %>% mutate(age_years = 3))
pred_bev <- make_predictions('c2', pred_dt_bev_car)
rm(dt_bev_car)

# PHEV

dt_phev_car <- load_dt_phev_car()
pred_dt_phev_car <- dt_phev_car %>% 
  filter(model == 'volt') %>% 
  group_by(make, model, my) %>% 
  summarise(
    range = median(range), 
    cents_per_mile = median(cents_per_mile), 
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>% 
  mutate(
    age_years = 1,
    powertrain = 'PHEV', 
    miles = 0
  ) 
pred_dt_phev_car <- pred_dt_phev_car %>%
  rbind(pred_dt_phev_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_phev_car %>% mutate(age_years = 3))
pred_phev <- make_predictions('d2', pred_dt_phev_car)
rm(dt_phev_car)

# HEV

dt_hev_car <- load_dt_hev_car()
pred_dt_hev_car <- dt_hev_car %>% 
  filter(model == 'prius') %>% 
  group_by(make, model, my) %>% 
  summarise(cents_per_mile = median(cents_per_mile)) %>% 
  mutate(
    age_years = 1,
    miles = 0,
    powertrain = 'Hybrid'
  )
pred_dt_hev_car <- pred_dt_hev_car %>%
  rbind(pred_dt_hev_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_hev_car %>% mutate(age_years = 3))
pred_hev <- make_predictions('e2', pred_dt_hev_car)
rm(dt_hev_car)

# CV

dt_cv_car <- load_dt_cv_car()
pred_dt_cv_car <- dt_cv_car %>% 
  filter(model == 'camry') %>% 
  group_by(make, model, my) %>%
  summarise(cents_per_mile = median(cents_per_mile)) %>% 
  mutate(
    age_years = 1,
    miles = 0,
    powertrain = 'Conventional'
  )
pred_dt_cv_car <- pred_dt_cv_car %>%
  rbind(pred_dt_cv_car %>% mutate(age_years = 2)) %>% 
  rbind(pred_dt_cv_car %>% mutate(age_years = 3))
pred_cv <- make_predictions('f2', pred_dt_cv_car)
rm(dt_cv_car)

# Make the data for the figure

df_fig7 <- rbind(pred_bev, pred_phev, pred_hev, pred_cv) %>% 
  make_vehicle_var() %>% 
  ungroup() %>% 
  mutate(
    powertrain = fct_relevel(powertrain, c(
      'Conventional', 'Hybrid', 'PHEV', 'BEV'
    )),
    age_years = as.factor(age_years), 
    vehicle = paste0(vehicle, ' (', powertrain, ')')
  ) %>%
  arrange(powertrain, age_years, fit) %>% 
  mutate(vehicle = fct_inorder(vehicle)) %>% 
  select(mean = fit, everything()) %>% 
  select(
    -se.fit, -ci_low, -ci_high, -tesla, -range, -cents_per_mile,
    -subsidy_total_listing
  )

# Save data

write_parquet(df_fig7, here::here('data', 'df_fig7.parquet'))

# Make the figure

df_fig7 <- read_parquet(here::here('data', 'df_fig7.parquet')) %>% 
  mutate(my = ymd(paste(my, '01', '01', sep = '-'))) 
plotColors <- RColorBrewer::brewer.pal(3, "Set1")[c(1, 2, 3)]
fig7 <- df_fig7 %>% 
  pivot_wider(values_from = mean, names_from = age_years) %>% 
  ggplot(aes(x = my, xend = my)) +
  geom_segment(
    aes(y = `1`, yend = `3`), 
    color = 'grey65'
  ) +
  geom_point(
    data = df_fig7,
    aes(y = mean, color = age_years)
  ) +
  facet_wrap(vars(vehicle), nrow = 1) +
  scale_color_manual(values = plotColors) +
  scale_x_date(
    date_breaks = '1 year',
    date_labels = "'%y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0,0.05)), 
    limits = c(0, 1)
  ) +
  plot_theme() +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Retention rate', 
    y = NULL, 
    title = 'Predicted one and three year depreciation for select vehicle models',
    subtitle = 'Predictions made with zero mileage and mean operating costs, driving ranges, and subsidies',
    color = 'Age (years)'
  )

save_fig(fig7, width = 11, height = 3.5)


# FIG 8 ----

# Predicted depreciation for BEV make models to see range effect

# To make a prediction, must first load the original data
# Then create the prediction data
# Then make the prediction 

# Non-Tesla 

dt_bev_car_nontesla <- load_dt_bev_car_nontesla()
pred_dt_bev_car_nontesla <- dt_bev_car_nontesla %>% 
  filter(age_years > 1.5, age_years < 2.5) %>% 
  filter(year > 2012) %>%
  filter(model %in% c('leaf', 'i3', 'e-golf', 'focus')) %>%
  group_by(make, model, my) %>% 
  summarise(
    rr = mean(rr),
    range = mean(range), 
    dom_listing = mean(dom_listing), 
    cents_per_mile = mean(cents_per_mile), 
    subsidy_total_listing = mean(subsidy_total_listing), 
    miles = mean(miles)
  ) %>% 
  mutate(
    age_years = 2,
    powertrain = 'BEV'
  )
pred_bev_nontesla <- make_predictions('c2', pred_dt_bev_car_nontesla) %>% 
  make_vehicle_var() %>% 
  group_by(vehicle) %>% 
  mutate(
    index = row_number(),
    n = n(), 
    label = ifelse(index == 1, vehicle, ""),
    my = as.character(my)
  ) %>% 
  # Drop models with less than 3 years of data (can't fit line)
  filter(n > 3)

# Tesla 

dt_bev_car_tesla <- load_dt_bev_car_tesla()
pred_dt_bev_car_tesla <- dt_bev_car_tesla %>% 
  filter(model == 'model s') %>%
  filter(age_years > 1.5, age_years < 2.5) %>% 
  group_by(make, model, my) %>% 
  summarise(
    rr = mean(rr),
    range = mean(range), 
    dom_listing = mean(dom_listing), 
    cents_per_mile = mean(cents_per_mile), 
    subsidy_total_listing = mean(subsidy_total_listing), 
    miles = mean(miles)
  ) %>% 
  mutate(
    age_years = 2,
    powertrain = 'BEV'
  )
pred_bev_tesla <- make_predictions('g2', pred_dt_bev_car_tesla) %>% 
  make_vehicle_var() %>% 
  group_by(vehicle) %>% 
  mutate(
    index = row_number(),
    n = n(), 
    label = ifelse(index == 1, vehicle, ""),
    my = as.character(my)
  ) %>% 
  # Drop models with less than 3 years of data (can't fit line)
  filter(n > 3)

df_fig8 <- rbind(pred_bev_nontesla, pred_bev_tesla)

# Save data

write_parquet(df_fig8, here::here('data', 'df_fig8.parquet'))

# Make figure

df_fig8 <- read_parquet(here::here('data', 'df_fig8.parquet')) %>% 
  select(range, fit, vehicle, label, my, rr)

plotColors <- RColorBrewer::brewer.pal(5, "Set1")[c(2, 3, 4, 1, 5)]
fig8 <- df_fig8 %>%
  ggplot(aes(x = range, y = rr, color = vehicle)) +
  geom_label_repel(
    aes(y = fit, label = label), 
    direction = 'x', 
    segment.color = NA, 
    max.overlaps = 100
  ) + 
  geom_point() +
  geom_text_repel(
    aes(label = my),
    nudge_x = -5,
    family = 'Roboto Condensed'
  ) +
  geom_smooth(aes(y = fit), se = FALSE, method = 'lm') +
  plot_theme() + 
  scale_y_continuous(breaks = seq(0.3, 0.9, 0.2)) +
  coord_cartesian(xlim = c(50, 300), ylim = c(0.3, 0.9)) +
  scale_color_manual(values = plotColors) +
  labs(
    x = 'Electric driving range (miles)', 
    y = 'Retention rate', 
    title = 'Predicted two-year-old retention rate versus range (select BEVs)'
  )

set.seed(1234) # Keeps the labels where they need to be
save_fig(fig8, width = 7, height = 5)



# FIG 9 ----

# Load sales
evsales_bev <- raw_data_start <- read_csv(here("data", "usEvSales.csv"))
evsales_phev <- read_csv(here("data", "evsales_phev_vehicle_list.csv"))
ev_phaseout <- read_csv(here("data", "evphaseout_list.csv")) %>% select(vehicle, year, month, taxcredit)

# load models to get subsidy effects
model <- qread(here::here('models-temp', paste0('c2', '.qs')))
sub_nontesla <- (exp(coef(model)) - 1)['subsidy_total_listing']
model <- qread(here::here('models-temp', paste0('g2', '.qs')))
sub_tesla <- (exp(coef(model)) - 1)['subsidy_total_listing']

# create data frame with accurate tax credit amounts for phev, phaseouts
evsales_taxcred <- evsales_bev %>%
  left_join(evsales_phev, by = c("vehicle", "category", "year")) %>%
  mutate(
    taxcredit = replace(taxcredit, category == "bev", 7500))%>%
  left_join(ev_phaseout, by = c("vehicle", "year", "month")) %>%
  mutate(
    taxcredit = if_else(is.na(taxcredit.y)== "TRUE",taxcredit.x, taxcredit.y)
  ) %>%
  select(-taxcredit.x, -taxcredit.y) %>%
  mutate(
    taxcredit = taxcredit/1000
  )

#combine sales data and subsidy effect
df_fig9 <- evsales_taxcred %>%
  mutate(
    taxcred_used_adj = ifelse(brand == "Tesla", taxcredit*sub_tesla , taxcredit*sub_nontesla)) %>%
  mutate(
    taxcred_spend = sales * taxcredit,
    taxcred_spend_used_adj = sales * (-1*taxcred_used_adj*taxcredit),
    type = ifelse(brand == "Tesla", "Tesla", "Non-Tesla")) %>%
  group_by(year, type) %>%
  summarise(sales_sum = sum(sales), 
            taxcred_spend = sum(taxcred_spend),
            taxcred_spend_used_adj = sum(taxcred_spend_used_adj)) %>%
  mutate(
    taxcred_spend = round(taxcred_spend / 10^3,2),
    taxcred_spend_used_adj = round(taxcred_spend_used_adj / 10^3,2),
    year = as.character(year)
  )

# Save figure data

write_parquet(df_fig9, here::here('data', 'df_fig9.parquet'))

# Make the figure

df_fig9 <- read_parquet(here::here('data', 'df_fig9.parquet'))

# Plot
total <- scales::dollar(round(sum(df_fig9$taxcred_spend_used_adj)))
fig9 <- df_fig9 %>%
  ggplot() +
  geom_col(aes(x = year, y = taxcred_spend_used_adj, fill = type), width = 0.5)  +
  geom_text(
    data = df_fig9 %>% 
      group_by(year) %>% 
      summarise(total = sum(taxcred_spend_used_adj)) %>% 
      mutate(total_label = scales::dollar(total, accuracy = 0.1)),
    aes(x = year, y = total, label = total_label),
    vjust = -1,color = 'black', size = 3, nudge_y = -0.02
  ) +
  scale_fill_manual(values = c(color_ev, color_tesla)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, 85),
    labels = scales::dollar
  ) +
  labs(
    x = 'Year',  
    y = "Indirect Subsidy to Resale Market (USD $ Million)",
    fill = "BEV Type",
    title = "Indirect Subsidies to Resale Market",
    subtitle = paste0(
      'Between 2010 and 2019, PEV Subsidies in the New Vehicle Market Have Indirectly Provided\n', 
      total, ' Million in Subsidies to the Resale Market Through Reduced Prices.')
  ) +
  plot_theme() +
  theme(
    legend.position = c(0.1, 0.78), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    legend.margin = margin(5, 5, 5, 5)
  ) 

# Save plots 
save_fig(fig9, width = 7, height = 5)

# FIG 10 ----

# Prices before and after COVID 

# Make chart in 2019 inflation-adjusted prices first

prices_2019 <- rbind(
    load_dt_all(pt = 'bev', vt = 'car') %>% 
      select(price, status_date, powertrain, tesla) %>% 
      arrange(status_date),
    load_dt_all(pt = 'conventional', vt = 'car') %>% 
      select(price, status_date, powertrain, tesla) %>% 
      arrange(status_date)
  ) %>% 
  mutate(
    type = ifelse(
      powertrain == 'conventional', 'Conventional', ifelse(
      tesla == 1, 'BEV (Tesla)', 'BEV (Non-Tesla)'
    )),
    date = zoo::as.yearmon(status_date)
  ) %>%
  group_by(date, type) %>%
  summarise(
    price25 = fquantile(price, 0.25),
    price_mean = round(mean(price)),
    price50 = fquantile(price, 0.5),
    price75 = fquantile(price, 0.75)
  ) 

# Now compute the real dollar amounts

cpi <- read_csv(here::here('data', 'inflation-cpi-tidy.csv'))

prices_real <- rbind(
  load_dt_all(pt = 'bev', vt = 'car') %>% 
    select(price, status_date, powertrain, tesla) %>% 
    arrange(status_date),
  load_dt_all(pt = 'conventional', vt = 'car') %>% 
    select(price, status_date, powertrain, tesla) %>% 
    arrange(status_date)
) %>% 
  mutate(
    year = year(status_date),
    month = month(status_date)
  ) %>% 
  # inflation adjust
  left_join(
    cpi %>% 
      select(-date), 
    by = c('year', 'month')
  ) %>% 
  mutate(
    price = price * cpi,
    type = ifelse(
      powertrain == 'conventional', 'Conventional', ifelse(
        tesla == 1, 'BEV (Tesla)', 'BEV (Non-Tesla)'
      )),
    date = zoo::as.yearmon(status_date)
  ) %>%
  group_by(date, type) %>%
  summarise(
    price25 = fquantile(price, 0.25),
    price_mean = round(mean(price)),
    price50 = fquantile(price, 0.5),
    price75 = fquantile(price, 0.75)
  ) 

# Make plot function, then make each figure separately

price_plot <- function(df, tesla_yloc) {
  
  # Mar 2022
  
  price_mean_mar22 <- df %>%
    filter(date == "Mar 2022") %>%
    mutate(
      label = paste("March 2022 Mean:","\n", dollar(price_mean)))
  
  # Jan 2020
  
  price_mean_jan20 <- df %>%
    filter(date == "Jan 2020") %>%
    mutate(
      label = paste("Jan 2020 Mean:","\n", dollar(price_mean), " "),
      position = price_mean + c(-8000, 12000, 8000)
    )
  
  plot <- df %>%  
    ggplot(aes(x = date)) +
    geom_ribbon(
      aes(ymin = price25, ymax = price75, fill = type), 
      alpha = 0.25
    ) +
    # geom_line(aes(y = price50, color = type), linetype = 'dashed') +
    geom_line(aes(y = price_mean, color = type)) +
    # March 2022 Mean point and labels
    geom_point(
      data = price_mean_mar22, 
      aes(y = price_mean, color = type)
    ) +
    geom_text_repel(
      data = price_mean_mar22, 
      aes(y = price_mean, label = label, color = type), 
      hjust = 0, nudge_x = 0.2, direction = 'y',
      family = 'Roboto Condensed'
    ) +
    # Jan 2020 Mean point and labels
    geom_point(
      data = price_mean_jan20, 
      aes(y = price_mean, color = type)
    ) +
    geom_text(
      data = price_mean_jan20, 
      aes(y = position, label = label, color = type), 
      hjust = 1, nudge_x = 0, 
      family = 'Roboto Condensed'
    ) +
    scale_color_manual(
      values = c(
        color_ev, color_tesla, color_cv, color_ev, color_tesla, color_cv
      )
    ) +
    scale_fill_manual(
      values = c(
        color_ev, color_tesla, color_cv, color_ev, color_tesla, color_cv
      )
    ) +
    scale_y_continuous(
      limits = c(0, 80000), 
      labels = scales::dollar, 
      expand = expansion(mult = c(0, 0)), 
    ) +
    geom_vline(xintercept = 2020.16666666667, color = "red") +
    plot_theme() +
    coord_cartesian(
      xlim = c(
        zoo::as.yearmon(ymd('2016-01-01')), 
        zoo::as.yearmon(ymd('2023-06-01'))
      )
    ) +
    labs(
      x = "Listing Date",
      y = 'Listing price ($USD)',
      title = 'Used market listing prices are substantially higher post-COVID19'
    ) +
    geom_label(
      data = data.frame(
        x = c( 2017, 2017, 2017), 
        y = c(tesla_yloc, 5000, 22000),
        label = c('BEV (Tesla)', 'BEV (Non-Tesla)', 'Conventional')),
      mapping = aes(x = x, y = y, label = label, color = label),
      size = 4,
      family = 'Roboto Condensed'
    )
  
  return(plot)
}

fig10_2019 <- price_plot(prices_2019, 46000) +
  labs(subtitle = 'Prices inflation-adjust to constant 2019 $USD')
fig10_real <- price_plot(prices_real, 44000) +
  labs(subtitle = 'Prices unadjusted for inflation as current $USD')

save_fig(fig10_2019, width = 8, height = 5.5)
save_fig(fig10_real, width = 8, height = 5.5)


# FIG 11 ----

# BEV prices before and after COVID

dt_bev <-load_dt_bev()
dt_bev_covid <- load_dt_covid(pt = 'bev', vt = 'car')

quantiles_bev_covid <- dt_bev_covid %>%
  mutate(age_months = round(age_years * 12)) %>% 
  group_by(age_months, tesla) %>% 
  summarise(
    rr25 = fquantile(rr, 0.25),
    rr50 = fquantile(rr, 0.5),
    rr75 = fquantile(rr, 0.75)
  ) %>%
  mutate(age_years = age_months / 12) %>%
  filter(between(age_years, 1, 8)) %>%
  mutate(
    covid = 1
  )

quantiles_bev <- read_csv(here::here('data', 'quantiles_bev.csv')) %>% 
  select(-arr25, -arr50, -arr75) %>%
  mutate(age_years = age_months / 12)   %>%
  filter(between(age_years, 1, 8)) %>%
  mutate(
    covid = 0
  )

quants_bev <- quantiles_bev %>%
  rbind(quantiles_bev_covid) %>%
  mutate(
  type = ifelse(tesla == 1, 'BEV (Tesla)', 'BEV (Non-Tesla)'),
  covid = ifelse(covid == 1, "after", "before"))

fig_bev_covid <- quants_bev %>% 
  ggplot() +
  geom_ribbon(
    aes(
      x = age_years,
      ymin = rr25,
      ymax = rr75,
      fill = covid,
    ),
    alpha = 0.25) +
  geom_line(
    aes(
      x = age_years,
      y = rr50,
      color = covid,
    ))+
  facet_wrap(~type) +
  scale_x_continuous(
    breaks = seq(1, 8, 1),
    limits = c(1, 8)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 1.4, 0.2)
  ) +
  coord_cartesian(ylim = c(0, 1.4)) +
  plot_theme() +
  scale_fill_manual(values = c(color_ev, color_tesla)
  ) +
  scale_color_manual(values = c(
    color_ev, color_tesla, color_ev, color_tesla)
  ) +
  labs(
    x = "Vehicle age (years)",
    y = 'Vehicle value retention rate'
  ) +
  geom_label(
    data = data.frame(
      x = c( 6, 3), 
      y = c( 0.8, 0.2),
      label = c(
       'Post COVID','Pre COVID')),
    mapping = aes(x = x, y = y, label = label, color = label),
    size = 4,
    family = 'Roboto Condensed'
  ) 

fig_bev_covid

save_fig(fig_bev_covid, width = 7, height = 3.8)


# FIG 12 ----

# Predicted two year retention rate by powertrain for every make model
# Show difference between 2014 and 2018 MY
# To make a prediction, must first load the original data
# Then create the prediction data
# Then make the prediction

target_years <- c(2014, 2018)

# BEV Non-Tesla

dt_bev_car_nontesla <- load_dt_bev_car_nontesla()
pred_dt_bev_car_nontesla <- dt_bev_car_nontesla %>%
  group_by(make, model, my) %>%
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing),
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>%
  mutate(
    age_years = 2,
    powertrain = 'BEV',
    miles = 0
  ) %>% 
  filter(my %in% as.factor(target_years))
pred_bev_nontesla <- make_predictions('c2', pred_dt_bev_car_nontesla)
rm(dt_bev_car_nontesla)

# BEV Tesla

dt_bev_car_tesla <- load_dt_bev_car_tesla()
pred_dt_bev_car_tesla <- dt_bev_car_tesla %>%
  group_by(make, model, my) %>%
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing),
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>%
  mutate(
    age_years = 2,
    powertrain = 'BEV',
    miles = 0
  ) %>% 
  filter(my %in% as.factor(target_years))
pred_bev_tesla <- make_predictions('g2', pred_dt_bev_car_tesla)
rm(dt_bev_car_tesla)

# Merge BEVs
pred_bev <- rbind(pred_bev_nontesla, pred_bev_tesla)

# PHEV

dt_phev_car <- load_dt_phev_car()
pred_dt_phev_car <- dt_phev_car %>%
  group_by(make, model, my) %>%
  summarise(
    range = mean(range),
    dom_listing = mean(dom_listing),
    cents_per_mile = mean(cents_per_mile),
    subsidy_total_listing = mean(subsidy_total_listing)
  ) %>%
  mutate(
    age_years = 2,
    powertrain = 'PHEV',
    miles = 0
  ) %>% 
  filter(my %in% as.factor(target_years))
pred_phev <- make_predictions('d2', pred_dt_phev_car)
rm(dt_phev_car)

# HEV

dt_hev_car <- load_dt_hev_car()
pred_dt_hev_car <- dt_hev_car %>%
  group_by(make, model, my) %>%
  summarise(
    dom_listing = mean(dom_listing),
    cents_per_mile = median(cents_per_mile),
  ) %>%
  mutate(
    age_years = 2,
    miles = 0,
    powertrain = 'Hybrid'
  ) %>% 
  filter(my %in% as.factor(target_years))
pred_hev <- make_predictions('e2', pred_dt_hev_car)
rm(dt_hev_car)

# CV

dt_cv_car <- load_dt_cv_car()
pred_dt_cv_car <- dt_cv_car %>%
  group_by(make, model, my) %>%
  summarise(
    dom_listing = mean(dom_listing),
    cents_per_mile = median(cents_per_mile),
  ) %>%
  mutate(
    age_years = 2,
    miles = 0,
    powertrain = 'Conventional'
  ) %>% 
  filter(my %in% as.factor(target_years))
pred_cv <- make_predictions('f2', pred_dt_cv_car)
rm(dt_cv_car)

# Make the data for the figure

df_fig12 <- rbind(pred_bev, pred_phev, pred_hev, pred_cv) %>%
  ungroup() %>% 
  select(make, model, my, powertrain, fit) %>% 
  mutate(
    make = ifelse(powertrain == 'Conventional', paste0(' ', make), make)
  ) %>%
  make_vehicle_var() %>%
  ungroup() %>%
  mutate(
    powertrain = fct_relevel(powertrain, c(
      'Conventional', 'Hybrid', 'PHEV', 'BEV'
    )), 
    my = as.numeric(as.character(my)), 
    my = factor(my, c('2014', '2018'))
  ) %>%
  select(mean = fit, everything()) %>% 
  # Fill in missing 2018s as same as 2014, this preserves the ordering
  pivot_wider(values_from = mean, names_from = my) %>%
  mutate(`2018` = ifelse(is.na(`2018`), `2014`, `2018`)) %>% 
  pivot_longer(values_to = 'mean', names_to = 'my', cols = c(`2014`, `2018`)) %>% 
  arrange(powertrain, desc(my), mean) %>%
  mutate(vehicle = fct_inorder(vehicle))
  
# Save figure data

write_parquet(df_fig12, here::here('data', 'df_fig12.parquet'))

# Make the figure

df_fig12 <- read_parquet(here::here('data', 'df_fig12.parquet'))
plotColors <- RColorBrewer::brewer.pal(3, "Set1")[c(1, 2)]
fig12 <- df_fig12 %>%
  pivot_wider(values_from = mean, names_from = my) %>%
  mutate(`2018` = ifelse(is.na(`2018`), `2014`, `2018`)) %>% 
  ggplot(aes(y = vehicle, yend = vehicle)) +
  geom_vline(xintercept = 1) +
  geom_segment(
    aes(x = `2014`, xend = `2018`),
    color = 'grey42'
  ) +
  geom_point(
    data = df_fig12,
    aes(x = mean, color = my)
  ) +
  facet_grid(
    powertrain ~ .,
    scales = "free_y",
    space = "free"
  ) +
  scale_x_continuous(
    limits = c(0.2, 1.05),
    breaks = seq(0.2, 1, 0.2)
  ) +
  scale_color_manual(values = plotColors) +
  labs(
    x = 'Retention rate',
    y = NULL,
    title = paste0(
      "Predicted two-year old retention rate for model years <span style = 'color: ", 
      plotColors[1],
      ";'>2014</span> vs. <span style = 'color: ", 
      plotColors[2],
      ";'>2018</span>")
  ) +
  plot_theme() +
  theme(
    strip.background = element_rect(fill = "grey80"), 
    panel.grid.major = element_line(size = 0.3, colour = "grey90"),
    plot.title = element_markdown()
  ) 

save_fig(fig12, height = 11, width = 7)

