setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Local Public Finance/2nd Assignment (Presentation)/Data")

library(tidyverse)
library(readxl)
library(tmap)
library(sf)

print(list.files <- list.files())

data_log <- read_xlsx("IJASE-AnalysisofTheRoleofTourisminTheEconomyinIndonesia.xlsx")

# Prepare pdata.frame for fixed effect panel regression
library(plm)
pdata <- pdata.frame(data_log, index = c("province", "year"))

## 1st Model with GRDP
model_grdp  <- plm(formula = log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist, model = "within", data = pdata)
summary(model_grdp)

## 2nd Model with Tourism Employment
model_employment <- plm(formula = log_tourism_employment ~ HDI + log_pop_density  + log_domestic_tourist + log_DDI + log_FDI + log_foreign_tourist, model="within", data = pdata)
summary(model_employment)

## 3rd Model with growth
model_growth  <- plm(formula = growth ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_unemployment + log_FDI + log_DDI + log_total + log_tourism_employment + log_grdp + log_pop_density, model = "within", data = pdata)
summary(model_growth)

# Function to calculate VIF
calculate_vif <- function(model) {
  vifs <- NULL
  variables <- names(model$coefficients)
  for (var in variables) {
    if (var != "(Intercept)") {
      formula <- as.formula(paste(var, "~ ."))
      submodel <- lm(formula, data = model$model)
      r_squared <- summary(submodel)$r.squared
      vif <- 1 / (1 - r_squared)
      vifs <- c(vifs, vif)
    }
  }
  variables <- variables[variables != "(Intercept)"]
  names(vifs) <- variables
  return(vifs)
}

# Calculate VIF values
vif_values <- calculate_vif(model_employment)
print(vif_values)

## Model with growth revised
model_growth_rev  <- plm(formula = growth ~ HDI + log_foreign_tourist + log_unemployment + log_FDI + log_DDI + log_grdp + log_pop_density, model = "within", data = pdata)
summary(model_growth_rev)


# Add all variables -------------------------------------------------------
## X = GRDP, Y with tourism employment
model1  <- plm(formula = log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_tourism_employment + log_pop_density, model = "within", data = pdata)
summary(model1)

# Function to calculate VIF
calculate_vif <- function(model) {
  vifs <- NULL
  variables <- names(model$coefficients)
  for (var in variables) {
    if (var != "(Intercept)") {
      formula <- as.formula(paste(var, "~ ."))
      submodel <- lm(formula, data = model$model)
      r_squared <- summary(submodel)$r.squared
      vif <- 1 / (1 - r_squared)
      vifs <- c(vifs, vif)
    }
  }
  variables <- variables[variables != "(Intercept)"]
  names(vifs) <- variables
  return(vifs)
}

# Calculate VIF values
vif_values <- calculate_vif(model1)
print(vif_values)

# Revised model
model_rev_1  <- plm(formula = log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, model = "within", data = pdata)
summary(model_rev_1)


## Y = unemployment, X = GRDP
model2  <- plm(formula = log_unemployment ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, model = "within", data = pdata)
summary(model2)

# Function to calculate VIF
calculate_vif <- function(model) {
  vifs <- NULL
  variables <- names(model$coefficients)
  for (var in variables) {
    if (var != "(Intercept)") {
      formula <- as.formula(paste(var, "~ ."))
      submodel <- lm(formula, data = model$model)
      r_squared <- summary(submodel)$r.squared
      vif <- 1 / (1 - r_squared)
      vifs <- c(vifs, vif)
    }
  }
  variables <- variables[variables != "(Intercept)"]
  names(vifs) <- variables
  return(vifs)
}

# Calculate VIF values
vif_values <- calculate_vif(model2)
print(vif_values)

library(gridExtra)

# Function to create scatter plots with regression lines
create_scatter_plot <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste(y_var, "vs", x_var),
         x = x_var,
         y = y_var) +
    theme_minimal()
}

# List of independent variables
independent_vars <- c("log_accommodation", "log_domestic_tourist", "log_foreign_tourist", "log_FDI", "log_DDI", "log_pop_density")

# Create plots for each independent variable
plots <- lapply(independent_vars, function(var) {
  create_scatter_plot(data_log, var, "log_unemployment")
})

# Display all plots
do.call(grid.arrange, c(plots, ncol = 2))


# Spatial Lag Model -------------------------------------------------------
install.packages("spatialreg")
library(spdep)
library(spatialreg)
library(plm)
library(sf)

# Example neighbor list for Indonesian provinces
neighbors <- list(
  "Aceh" = c("Sumatera Utara"),
  "Sumatera Utara" = c("Aceh", "Sumatera Barat", "Riau"),
  "Sumatera Barat" = c("Sumatera Utara", "Riau", "Jambi", "Bengkulu"),
  "Riau" = c("Sumatera Utara", "Sumatera Barat", "Jambi", "Kepulauan Riau"),
  "Jambi" = c("Riau", "Sumatera Barat", "Sumatera Selatan", "Bengkulu"),
  "Sumatera Selatan" = c("Jambi", "Bengkulu", "Lampung"),
  "Bengkulu" = c("Sumatera Barat", "Jambi", "Sumatera Selatan", "Lampung"),
  "Lampung" = c("Sumatera Selatan", "Bengkulu", "Banten"),
  "Kepulauan Riau" = c("Riau"),
  "Bangka Belitung" = c("Sumatera Selatan", "Kepulauan Riau"),
  "Jakarta Raya" = c("Banten", "Jawa Barat"),
  "Jawa Barat" = c("Jakarta Raya", "Banten", "Jawa Tengah"),
  "Banten" = c("Jakarta Raya", "Jawa Barat", "Lampung"),
  "Jawa Tengah" = c("Jawa Barat", "Yogyakarta", "Jawa Timur"),
  "Yogyakarta" = c("Jawa Tengah"),
  "Jawa Timur" = c("Jawa Tengah", "Bali"),
  "Bali" = c("Jawa Timur", "Nusa Tenggara Barat"),
  "Nusa Tenggara Barat" = c("Bali", "Nusa Tenggara Timur"),
  "Nusa Tenggara Timur" = c("Nusa Tenggara Barat"),
  "Kalimantan Barat" = c("Kalimantan Tengah"),
  "Kalimantan Tengah" = c("Kalimantan Barat", "Kalimantan Selatan", "Kalimantan Timur"),
  "Kalimantan Selatan" = c("Kalimantan Tengah", "Kalimantan Timur"),
  "Kalimantan Timur" = c("Kalimantan Tengah", "Kalimantan Selatan", "Kalimantan Utara"),
  "Kalimantan Utara" = c("Kalimantan Timur"),
  "Sulawesi Utara" = c("Gorontalo"),
  "Gorontalo" = c("Sulawesi Utara", "Sulawesi Tengah"),
  "Sulawesi Tengah" = c("Gorontalo", "Sulawesi Barat", "Sulawesi Selatan"),
  "Sulawesi Barat" = c("Sulawesi Tengah", "Sulawesi Selatan"),
  "Sulawesi Selatan" = c("Sulawesi Barat", "Sulawesi Tengah", "Sulawesi Tenggara"),
  "Sulawesi Tenggara" = c("Sulawesi Selatan"),
  "Maluku" = c("Maluku Utara"),
  "Maluku Utara" = c("Maluku"),
  "Papua Barat" = c("Papua"),
  "Papua" = c("Papua Barat")
)

# Create a list of neighbors using the nb class from spdep
nb <- lapply(neighbors, function(x) match(x, names(neighbors)))
class(nb) <- "nb"
attr(nb, "region.id") <- names(neighbors)

# Convert the nb object to a spatial weights list
lw <- nb2listw(nb, style = "W")
print(lw)

# Ensure pdata has the same regions in the same order for one cross-sectional year
# 2018
pdata_cross_2018 <- pdata[pdata$year == 2018, ]

# 2019
pdata_cross_2019 <- pdata[pdata$year == 2019, ]

# 2020
pdata_cross_2020 <- pdata[pdata$year == 2020, ]

# Convert the data to a matrix for spatial regression
# 2018
log_grdp <- as.numeric(pdata_cross_2018$log_grdp)
HDI <- as.numeric(pdata_cross_2018$HDI)
log_accommodation <- as.numeric(pdata_cross_2018$log_accommodation)
log_domestic_tourist <- as.numeric(pdata_cross_2018$log_domestic_tourist)
log_foreign_tourist <- as.numeric(pdata_cross_2018$log_foreign_tourist)
log_FDI <- as.numeric(pdata_cross_2018$log_FDI)
log_DDI <- as.numeric(pdata_cross_2018$log_DDI)
log_pop_density <- as.numeric(pdata_cross_2018$log_pop_density)

# Combine all into a matrix
X_2018 <- cbind(HDI, log_accommodation, log_domestic_tourist, log_foreign_tourist, log_FDI, log_DDI, log_pop_density)

# Fit the spatial lag model - GRDP
slm_grdp_2018 <- lagsarlm(log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2018, listw = lw)

# Print summary - GRDP
print(summary(slm_grdp_2018))

# Fit the spatial lag model - unemployment
slm_unemployment_2018 <- lagsarlm(log_unemployment ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2018, listw = lw)

# Print summary
print(summary(slm_unemployment_2018))

# 2019
log_grdp <- as.numeric(pdata_cross_2019$log_grdp)
HDI <- as.numeric(pdata_cross_2019$HDI)
log_accommodation <- as.numeric(pdata_cross_2019$log_accommodation)
log_domestic_tourist <- as.numeric(pdata_cross_2019$log_domestic_tourist)
log_foreign_tourist <- as.numeric(pdata_cross_2019$log_foreign_tourist)
log_FDI <- as.numeric(pdata_cross_2019$log_FDI)
log_DDI <- as.numeric(pdata_cross_2019$log_DDI)
log_pop_density <- as.numeric(pdata_cross_2019$log_pop_density)

# Combine all into a matrix
X_2019 <- cbind(HDI, log_accommodation, log_domestic_tourist, log_foreign_tourist, log_FDI, log_DDI, log_pop_density)

# Fit the spatial lag model - GRDP
slm_grdp_2019 <- lagsarlm(log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2019, listw = lw)

# Print summary - GRDP
print(summary(slm_grdp_2019))

# Fit the spatial lag model - unemployment
slm_unemployment_2019 <- lagsarlm(log_unemployment ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2019, listw = lw)

# Print summary
print(summary(slm_unemployment_2019))

# 2020
log_grdp <- as.numeric(pdata_cross_2020$log_grdp)
HDI <- as.numeric(pdata_cross_2020$HDI)
log_accommodation <- as.numeric(pdata_cross_2020$log_accommodation)
log_domestic_tourist <- as.numeric(pdata_cross_2020$log_domestic_tourist)
log_foreign_tourist <- as.numeric(pdata_cross_2020$log_foreign_tourist)
log_FDI <- as.numeric(pdata_cross_2020$log_FDI)
log_DDI <- as.numeric(pdata_cross_2020$log_DDI)
log_pop_density <- as.numeric(pdata_cross_2020$log_pop_density)

# Combine all into a matrix
X_2020 <- cbind(HDI, log_accommodation, log_domestic_tourist, log_foreign_tourist, log_FDI, log_DDI, log_pop_density)

# Fit the spatial lag model - GRDP
slm_grdp_2020 <- lagsarlm(log_grdp ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2020, listw = lw)

# Print summary - GRDP
print(summary(slm_grdp_2020))

# Fit the spatial lag model - unemployment
slm_unemployment_2020 <- lagsarlm(log_unemployment ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density, data = pdata_cross_2020, listw = lw)

# Print summary
print(summary(slm_unemployment_2020))