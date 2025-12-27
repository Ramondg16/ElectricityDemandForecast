# Electricity Demand Forecasting

**Project Goal:** Develop a robust time-series forecasting pipeline to predict hourly electricity demand, optimizing for grid reliability by benchmarking statistical models against legacy forecasting systems ("Prevista" and "Programada").

## Key Results & Best Model Performance
The **ARIMA** model was selected as the optimal choice due to its superior ability to capture the complex autocorrelation structure and seasonal fluctuations of hourly demand, outperforming both simpler linear models and the existing "Prevista" benchmark.

| Model | RMSE (MW) | MAE (MW) | MAPE (%) |
| :--- | :--- | :--- | :--- |
| ARIMA | 0.2834 | 0.2289 | 0.8554 |
| ETS | 0.3981 | 0.3535 | 1.3315 |
| Programada | 0.5540 | 0.2919 | 1.0623 |
| TSLM | 2.2124 | 1.9673 | 7.3881 |
| Prevista | 2.4526 | 0.2587 | 0.9259 |

## Technical Workflow
This project executes an end-to-end data science pipeline in R, utilizing the `fpp3` and `fable` ecosystems:

### 1. Data Exploration & Pre-processing
* **Resampling:** Converted raw electricity data into an hourly `tsibble` object, handling gaps and distinct timestamps to ensure a continuous time series.
* **Decomposition:** Applied **STL Decomposition** to isolate the underlying trend, daily seasonality, and remainder components.

* **Outlier Analysis:** Used rolling 30-day standard deviation windows to identify and address demand spikes.

### 2. Feature Engineering & Transformation
* **Variance Stabilization:** Utilized **Guerrero’s method** to calculate the optimal lambda for a **Box-Cox transformation**, effectively mitigating heteroscedasticity.
* **Temporal Features:** Engineered features for hour-of-day, day-of-week, and monthly patterns to capture cyclical human-activity demand.

### 3. Modeling & Evaluation
* **Comparative Modeling:** Benchmarked **ARIMA** (Auto-Regressive Integrated Moving Average), **ETS** (Exponential Smoothing), and **TSLM** (Time-series Linear Model).
* **Validation Strategy:** Employed **Rolling-Origin Time-series Cross-Validation** (`stretch_tsibble`) to simulate real-world 1-step-ahead forecasting performance.
* **Benchmarking:** Quantified model value by comparing error distributions against existing human-scheduled and predicted demand series.

## Repository Contents
* `EnergyDemad.qmd`: The primary analysis file containing the full R implementation.
* `Demand_data.csv`: The raw hourly demand dataset.
* `README.md`: Project documentation and summary.
* `.gitignore`: Configured to exclude R-specific environment and cache files.
