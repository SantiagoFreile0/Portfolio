# 🏡 Real Estate Price Estimator

🎯 [Try the Live App](https://santiagofreile.shinyapps.io/finalapp/)

This is an interactive Shiny web application that estimates real estate prices based on user inputs. The model is trained using the well-known **Ames Housing** dataset and performs multiple linear regression on selected features.

---

## 📌 Overview

The app allows users to input features of a house (like square footage, garage size, year built, etc.) and returns an estimated sale price based on a regression model. It also provides useful visualizations like:

- 📉 Histogram of similar houses
- 📍 Map with nearby properties
- 📊 Scatter plot of price vs. living area

---

## 🧠 Technologies Used

- **R**
- **Shiny**
- **ggplot2**
- **leaflet**
- **AmesHousing** (dataset)
- **Multiple Linear Regression**

---

## 🛠️ How to Run Locally

To run the app locally:

1. Clone this repository or download the folder
2. Open `app.R` in RStudio
3. Run the app with:

```r
shiny::runApp()

