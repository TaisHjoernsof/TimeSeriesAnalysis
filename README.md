# 02417 Time Series Analysis – DTU Course Repository

This repository contains assignment solutions and project material for the MSc course **02417 Time Series Analysis** at the Technical University of Denmark (DTU). The course covers time series modeling, forecasting, and system identification methods including ARIMA, ARX, and state-space models.

## Assignments Overview

### Assignment 1: Linear and Recursive Trend Models
Analyzes vehicle data in Denmark using:
- Linear trend and weighted least squares (WLS) models
- Recursive least squares (RLS) with forgetting
- Forecasting and parameter update optimization
- Model comparison (OLS, WLS, RLS) for 12-month predictions

### Assignment 2: ARMA and Seasonal Processes
Explores stationarity, invertibility, and ACF behavior of:
- AR(2) process and analytical ACF derivation
- Simulated seasonal ARIMA models with varying structures
- Visual identification of ARMA processes from simulated plots

### Assignment 3: ARMA, Seasonal Models, and ARX Models
Involves:
- Extended ARMA stability and autocorrelation analysis
- Seasonal PV output forecasting using AR models
- Box heating dynamics modeled using ARX transfer function models
- Model selection using AIC/BIC and RMSE
- Evaluation of one-step and multi-step prediction accuracy

### Assignment 4: Kalman Filtering and State-Space Modeling
Focuses on:
- Kalman filter implementation for 1D and 2D state-space systems
- Parameter estimation using maximum likelihood
- Robustness against non-Gaussian noise (e.g. Student's t-distribution)
- Application to transformer station temperature modeling

## Repository Structure

```text
├── assignment1/              # Linear Trend & Recursive Estimation
├── assignment2/              # ARMA and Seasonal Process Modeling
├── assignment3/              # ARMA & ARX with Forecasting Applications
├── assignment4/              # Kalman Filter & State Space Estimation
├── README.md                 # This file
