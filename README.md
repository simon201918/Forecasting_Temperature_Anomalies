# Forecasting Temperature Anomalies
This group project studies global temperature change with forecasting technics. Specifically, the project investigates the predictive power of different models for one year ahead forecast and determines the optimal model.

## Source of Data
**[data.giss.nasa.gov](https://data.giss.nasa.gov/gistemp/)**

You may search keyword **"Global-mean monthly, seasonal, and annual means"** [here](https://data.giss.nasa.gov/gistemp/) to get the latest records.


## **About Temperature Anomaly**

Intuitively, temperature anomaly is the difference between the actual temperature and reference value. Reference value means the expected value (of a given site) during the same period, which usually is a long-term average over 30 years. For the rigorous definition of temperature anomaly, please visit [data.giss.nasa.gov](https://data.giss.nasa.gov/gistemp/).

  

## **Data Pre-Processing and Visualization**

This research uses **monthly** global temperature anomaly. The original data downloaded from [data.giss.nasa.gov](https://data.giss.nasa.gov/gistemp/) (**"GLB.Ts+dSST.csv"**) on the webiste to obtain the latest records) is a panel data that includes monthly records since January 1880. The data is converted to a one-column series named **"abnormal_temp.csv"** for this project. 

The plot of the temperature anomaly is shown below. The training period is 1950-1999, and the validation period is 2000-2019. We also conclude that the data has a linear trend without apparent seasonality.

![say sth](https://github.com/simon201918/Forecasting_Temperature_Anomalies/blob/main/Plots/Global%20Abnormal%20Temp%201950-2019.jpeg?raw=true)
  

## **Model Selection**

The candidate models include the exponential filter model, autoregressive model (AR), moving average model (MA), and ARIMA model (adjust trend and seasonality if necessary). Recursive forecast technics are used to improve parameter reliability. The candidate models are compared with naive models based on RMSE and Diebold/Mariano (DM) test. The result suggests both the **quadratic model** and **exponential filter model with additive error and additive trend (AAN)** have good predictive power in one year ahead forecasting. We recommend using the AAN model to project short-run fluctuations and **quadratic model** to predict long-term trends.

**One-year ahead forecast result:**

![say sth](https://github.com/simon201918/Forecasting_Temperature_Anomalies/blob/main/Plots/Model%20Selection%20(1950-2019).jpeg?raw=true)

**Zoom in the validation period:**

![say sth](https://github.com/simon201918/Forecasting_Temperature_Anomalies/blob/main/Plots/Model%20Selection%20(2000-2019),%20Validation.jpeg?raw=true)
