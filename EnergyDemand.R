---
  title: "AP Final Project Code"
format: html
editor: visual
---
  
  ### Import Libraries
  
  ```{r}
library(fpp3)
library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tsibble)
library(feasts)
library(fabletools)
library(janitor)
library(slider)
library(skimr)

```

### Load Data

```{r}

energy_raw <- read_csv("Demand_data.csv", show_col_types = FALSE) |>
  clean_names()

# Initial resampling and conversion
energy_hourly <- energy_raw |>
  distinct(hora, .keep_all = TRUE) |>
  arrange(hora) |>
  mutate(hour = floor_date(hora, "hour")) |>
  group_by(hour) |>
  summarise(
    real       = mean(real, na.rm = TRUE),
    prevista   = mean(prevista, na.rm = TRUE),
    programada = mean(programada, na.rm = TRUE),
    .groups = "drop"
  ) |>
  as_tsibble(index = hour) |>
  fill_gaps()  

glimpse(energy_hourly)

```

### Exploratory Data Analysis

```{r}

energy_hourly |>
  pivot_longer(cols = c(real, prevista, programada),
               names_to = "series",
               values_to = "value") |>
  ggplot(aes(x = series, y = value)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Outlier Analysis",
    x = "",
    y = "Demand"
  )

```

```{r}
# Hourly plot
autoplot(energy_hourly, real) +
  labs(
    title = "Hourly Real Electricity Demand",
    x = "Time",
    y = "MW"
  )

# Daily average
energy_daily <- energy_hourly |>
  index_by(day = as_date(hour)) |>
  summarise(real = mean(real, na.rm = TRUE))

autoplot(energy_daily, real) +
  labs(
    title = "Daily Average Electricity Demand",
    x = "Date",
    y = "MW"
  )

```

```{r}

energy_features <- energy_hourly |>
  mutate(
    hour_of_day = hour(hour),
    wday        = wday(hour, label = TRUE),
    month       = month(hour, label = TRUE)
  )

# Hourly pattern
energy_features |>
  group_by(hour_of_day) |>
  summarise(mean_real = mean(real, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(hour_of_day, mean_real)) +
  geom_line() +
  labs(
    title = "Average Hourly Pattern",
    x = "Hour",
    y = "Mean Demand"
  )

# Daily pattern
energy_features |>
  group_by(wday) |>
  summarise(mean_real = mean(real, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(wday, mean_real, group = 1)) +
  geom_line() +
  labs(
    title = "Average Daily Pattern",
    x = "Day",
    y = "Mean Demand"
  )

# Monthly pattern
energy_features |>
  group_by(month) |>
  summarise(mean_real = mean(real, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(month, mean_real, group = 1)) +
  geom_line() +
  labs(
    title = "Average Monthly Pattern",
    x = "Month",
    y = "Mean Demand"
  )

```

```{r}

energy_hourly |>
  ACF(real) |>
  autoplot() +
  labs(
    title = "Hourly Demand ACF",
    x = "Lag (hours)",
    y = "ACF"
  )

```

```{r}

energy_daily |>
  mutate(roll_sd = slide_dbl(real, sd, .before = 30, .complete = FALSE)) |>
  ggplot(aes(day, roll_sd)) +
  geom_line() +
  labs(
    title = "Rolling 30-Day STD Demand",
    x = "Date",
    y = "Standard Deviation"
  )

```

```{r}

energy_hourly |>
  as_tibble() |>
  select(real, prevista, programada) |>
  cor(use = "pairwise.complete.obs")



```

```{r}

energy_daily_ts <- energy_daily |>
  as_tsibble(index = day)

energy_daily_ts |>
  model(STL(real ~ season(window = "periodic"))) |>
  components() |>
  autoplot()


```

```{r}
# Real vs. Prevista and Programada
energy_hourly |>
  filter(year(hour) == 2014) |> 
  pivot_longer(
    cols = c(real, prevista, programada),
    names_to = "series",
    values_to = "value"
  ) |>
  ggplot(aes(x = hour, y = value, colour = series)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Real Demand vs. Benchmarks (2014)",
    y = "Demand (MW)",
    colour = "Series"
  ) +
  scale_color_manual(values = c("real" = "black", "prevista" = "blue", "programada" = "red")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.5)))

```

```{r}

# Benchmark Forecasting Error 
energy_hourly |>
  mutate(error = real - prevista) |>
  ggplot(aes(x = error)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "lightblue", colour = "black") +
  geom_density(colour = "red", linewidth = 1) +
  xlim(-5, 5) + 
  geom_vline(xintercept = mean(energy_hourly$error, na.rm = TRUE), 
             linetype = "dashed", colour = "darkgreen", linewidth = 1) +
  labs(
    title = "Distribution of Prevista Forecast Errors",
    subtitle = "Error = Real - Prevista",
    x = "Error (MW)",
    y = "Density"
  )

```

```{r}

# Calculate RMSE
calculate_rmse <- function(actual, predicted) {
  na_removed <- na.omit(data.frame(actual, predicted))
  rmse <- sqrt(mean((na_removed$actual - na_removed$predicted)^2))
  return(rmse)
}

#Prevista RMSE
rmse_prevista_value <- energy_hourly |>
  as_tibble() |> 
  summarise(rmse = calculate_rmse(real, prevista)) |>
  pull(rmse)

#Programada RMSE
rmse_programada_value <- energy_hourly |>
  as_tibble() |> 
  summarise(rmse = calculate_rmse(real, programada)) |>
  pull(rmse)

print("--- Final Benchmark RMSE Results ---")
print(paste0("Prevista vs Real RMSE: ", round(rmse_prevista_value, 4)))
print(paste0("Programada vs Real RMSE: ", round(rmse_programada_value, 4)))


```

### Pre-processing & Feature Engineering

```{r}

energy_hourly_clean <- energy_hourly |>
  mutate(
    real = if_else(!is.finite(real) | real <= 0, NA_real_, real)
  )

# Impute missing values
energy_hourly_clean <- energy_hourly_clean |>
  tidyr::fill(real, .direction = "downup")


```

### Training / Validation Split

```{r}

# Training: 2014-2015
train_data <- energy_hourly_clean |>
  filter(hour >= ymd_h("2014-01-01 00"),
         hour <= ymd_h("2015-12-31 23"))

nrow(train_data)

# Box–Cox lambda on training data
lambda_train <- train_data |>
  features(real, features = guerrero, .period = 24) |>
  pull(lambda_guerrero)

lambda_train


```

### Model Training

```{r}
# Fit Models (TSLM / ETS / ARIMA )
fit_models <- train_data |>
  model(
    TSLM = TSLM(
      box_cox(real, lambda_train) ~
        trend() +
        season("1 day")
    ),
    ETS   = ETS(box_cox(real, lambda_train)),
    ARIMA = ARIMA(box_cox(real, lambda_train))
  )


report(fit_models)


```

### Model Evaluation - Rolling Origin Time-series CV

```{r}

# Inverse Box-cox helper
inv_box_cox <- function(x, lambda) {
  if (lambda == 0) {
    exp(x)
  } else {
    (lambda * x + 1)^(1 / lambda)
  }
}

# Rolling Origin Time-series CV
cv_base <- train_data |>
  filter(year(hour) == 2015)

cv_data <- cv_base |>
  stretch_tsibble(
    .init = 24 * 30,
    .step = 24 * 60
  )

cv_base_bc <- cv_base |>
  mutate(real_bc = box_cox(real, lambda_train))

cv_data_bc <- cv_base_bc |>
  stretch_tsibble(
    .init = 24 * 30,
    .step = 24 * 60
  )


#Fit model set on each fold and generate 6 hour ahead forecasts
cv_fc <- cv_data_bc |>
  model(
    TSLM = TSLM(real_bc ~ trend() + season("day")),
    ETS  = ETS(real_bc),
    ARIMA = ARIMA(real_bc)
  ) |>
  forecast(h = 1)

cv_errors <- cv_fc |>
  as_tibble() |>
  select(hour, .model, .mean) |>
  mutate(
    .mean = inv_box_cox(.mean, lambda_train)   # back to MW
  ) |>
  left_join(
    cv_base |>
      select(hour, real) |>
      as_tibble(),
    by = "hour"
  ) |>
  filter(!is.na(real)) 

cv_accuracy_models <- cv_errors |>
  group_by(.model) |>
  summarise(
    RMSE = sqrt(mean((.mean - real)^2, na.rm = TRUE)),
    MAE  = mean(abs(.mean - real), na.rm = TRUE),
    MAPE = mean(abs((.mean - real) / real), na.rm = TRUE) * 100,
    .groups = "drop"
  )

benchmark_accuracy_cv <- cv_base |>
  as_tibble() |>
  pivot_longer(
    cols = c(prevista, programada),
    names_to = ".model",
    values_to = "forecast"
  ) |>
  group_by(.model) |>
  summarise(
    RMSE = sqrt(mean((real - forecast)^2, na.rm = TRUE)),
    MAE  = mean(abs(real - forecast), na.rm = TRUE),
    MAPE = mean(abs((real - forecast) / real), na.rm = TRUE) * 100,
    .groups = "drop"
  )

cv_accuracy_all <- bind_rows(
  cv_accuracy_models |>
    mutate(source = "stat_model"),
  benchmark_accuracy_cv |>
    mutate(source = "benchmark")
) |>
  arrange(RMSE)

cv_accuracy_all


```

### Visualize Results

```{r}

model_errors <- cv_errors |>
  mutate(
    error     = .mean - real,
    abs_error = abs(error)
  ) |>
  select(hour, .model, error, abs_error) |>
  mutate(source = "stat_model")

benchmark_errors <- cv_base |>
  as_tibble() |>
  pivot_longer(
    cols = c(prevista, programada),
    names_to = ".model",
    values_to = "forecast"
  ) |>
  mutate(
    error     = forecast - real,
    abs_error = abs(error),
    source    = "benchmark"
  ) |>
  select(hour, .model, error, abs_error, source)

# Combine all errors
all_errors <- bind_rows(
  model_errors,
  benchmark_errors
)

all_errors |>
  ggplot(aes(
    x    = reorder(.model, abs_error, FUN = median),
    y    = abs_error,
    fill = source
  )) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 2)) +  
  labs(
    title    = "Absolute Forecast Errors (2015 CV)",
    x        = "Model",
    y        = "Absolute Error (MW)",
    fill     = "Source"
  )

```

```{r}

cv_accuracy_all |>
  ggplot(aes(
    x    = reorder(.model, RMSE),
    y    = RMSE,
    fill = source
  )) +
  geom_col() +
  labs(
    title    = "1-Step-Ahead Forecast RMSE by Model (2015 CV)",
    x        = "Model",
    y        = "RMSE (MW)",
    fill     = "Source"
  ) +
  coord_flip()  
```