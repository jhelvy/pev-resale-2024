source(here::here('code', '0-functions.R'))

color_cv <- "grey42"
color_ev <- "#00BA38" # "forestgreen"
color_tesla <- "#619CFF" # "dodgerblue"

# FIG 1 ----

quantiles <- read_csv(here::here('data', 'quantiles.csv')) %>% 
  mutate(age_years = age_months / 12) %>% 
  filter(between(age_years, 1, 8))
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
df_fig1 <- rbind(quantiles_other, quantiles_conventional) %>% 
    set_powertrain_levels()

fig1 <- df_fig1 %>%
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

save_fig(fig1, width = 11, height = 3.5)



# FIG 2 ----

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

df_fig2 <- rbind(pred_bev, pred_phev, pred_hev, pred_cv) %>%
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

write_parquet(df_fig2, here::here('data', 'df_fig2.parquet'))

# Make the figure

df_fig2 <- read_parquet(here::here('data', 'df_fig2.parquet'))
plotColors <- RColorBrewer::brewer.pal(3, "Set1")[c(1, 2)]
fig2 <- df_fig2 %>%
  pivot_wider(values_from = mean, names_from = my) %>%
  mutate(`2018` = ifelse(is.na(`2018`), `2014`, `2018`)) %>% 
  ggplot(aes(y = vehicle, yend = vehicle)) +
  geom_vline(xintercept = 1) +
  geom_segment(
    aes(x = `2014`, xend = `2018`),
    color = 'grey42'
  ) +
  geom_point(
    data = df_fig2,
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

save_fig(fig2, height = 11, width = 7)
















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



# FIG 3 ----

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

df_fig3 <- rbind(pred_bev_nontesla, pred_bev_tesla)

# Save data

write_parquet(df_fig3, here::here('data', 'df_fig3.parquet'))

# Make figure

df_fig3 <- read_parquet(here::here('data', 'df_fig3.parquet')) %>% 
  select(range, fit, vehicle, label, my, rr)

plotColors <- RColorBrewer::brewer.pal(5, "Set1")[c(2, 3, 4, 1, 5)]
fig3 <- df_fig3 %>%
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
save_fig(fig3, width = 7, height = 5)



# FIG 4 ----

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
df_fig4 <- evsales_taxcred %>%
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

write_parquet(df_fig4, here::here('data', 'df_fig4.parquet'))

# Make the figure

df_fig4 <- read_parquet(here::here('data', 'df_fig4.parquet'))

# Plot
total <- scales::dollar(round(sum(df_fig4$taxcred_spend_used_adj)))
fig4 <- df_fig4 %>%
  ggplot() +
  geom_col(aes(x = year, y = taxcred_spend_used_adj, fill = type), width = 0.5)  +
  geom_text(
    data = df_fig4 %>% 
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

save_fig(fig4, width = 7, height = 5)




# FIG 5 ----

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

# Save figure data

write_parquet(prices_2019, here::here('data', 'df_fig5.parquet'))

# Make the figures

fig5_2019 <- price_plot(prices_2019, 46000) +
  labs(subtitle = 'Prices inflation-adjust to constant 2019 $USD')
fig5_real <- price_plot(prices_real, 44000) +
  labs(subtitle = 'Prices unadjusted for inflation as current $USD')

save_fig(fig5_2019, width = 8, height = 5.5)
save_fig(fig5_real, width = 8, height = 5.5)
