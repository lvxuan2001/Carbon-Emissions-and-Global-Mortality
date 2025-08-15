
#------------------- Spline simulation -------------------
library(ggplot2)
library(splines)
library(ggbreak)
library(scales)

# Data preparation
model_both <- all_causes_standardized_both

co2_both <- model_both$ghg_per_capita 
val_both <- model_both$death 
gdp_both <- model_both$GDP_per_capita
gini_both <- model_both$gini
density_both <- model_both$density
temp_both <- model_both$temp
pm_both <- model_both$pm
ozone_both <- model_both$ozone
physician_both <- model_both$physician
DPT_both <- model_both$DPT
age65_both <- model_both$age65

# Natural spline fitting
model_both <- lm(val_both ~ ns(co2_both, df = 4) + gdp_both + temp_both + pm_both + ozone_both + DPT_both + age65_both + gini_both + density_both + physician_both)
co2_both_dense <- seq(min(co2_both, na.rm = TRUE), 30, length.out = 1520)
predicted_data_both_dense <- data.frame(
  co2_both = co2_both_dense,
  gdp_both = mean(gdp_both, na.rm = TRUE),
  temp_both = mean(temp_both, na.rm = TRUE),
  pm_both = mean(pm_both, na.rm = TRUE),
  ozone_both = mean(ozone_both, na.rm = TRUE),
  DPT_both = mean(DPT_both, na.rm = TRUE),
  age65_both = mean(age65_both, na.rm = TRUE),
  gini_both = mean(gini_both, na.rm = TRUE),
  density_both = mean(density_both, na.rm = TRUE),
  physician_both = mean(physician_both, na.rm = TRUE)
)

# Prediction
predictions_both_dense <- predict(model_both, newdata = predicted_data_both_dense, interval = "confidence")
predicted_data_both_dense$predicted_incidence <- predictions_both_dense[, "fit"]
predicted_data_both_dense$lower_bound <- predictions_both_dense[, "lwr"]
predicted_data_both_dense$upper_bound <- predictions_both_dense[, "upr"]

# Plot
library(ggplot2)
ggplot() +
  geom_line(aes(x = co2_both, y = predicted_incidence, color = "Both"), 
            data = predicted_data_both_dense, size = 1) +
  geom_ribbon(aes(x = co2_both, ymin = lower_bound, ymax = upper_bound, fill = "Both"), 
              data = predicted_data_both_dense, alpha = 0.2) +
  scale_color_manual(values = "#fbb01a") +
  scale_fill_manual(values = "#fbb01a") +
  scale_x_continuous(limits = c(0, 30), expand = c(0, 0), breaks = seq(0, 40, by = 5)) +
  scale_y_continuous(limits = c(500, 1700), breaks = seq(500, 1800, by = 200)) +
  labs(title = "All population", x = "GHG emissions per capita (tons) - df=5", y = "Death rate (per 100,000 population)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank()) + 
  guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL))


#------------------- Projection -------------------

library(splines)
library(dplyr)
library(tidyr)

select.country <- "SAU" # Change to the desired country code

emission_select <- emission_all[emission_all$Code == select.country, ]   
start_year <- 2020
end_year <- 2050

# GHG
start_value <- emission_select$ghg_per_country[emission_select$Year == start_year]
# SSP126
end_value <- start_value * 0.5
future_years <- start_year:end_year
future_emission <- seq(start_value, end_value, length.out = length(future_years))
future_data_126 <- data.frame(Code = select.country, Label = "SSP126", Year = future_years, co2_per_country = future_emission)
head(future_data_126)
# SSP245
end_value <- start_value
future_years <- start_year:end_year
future_emission <- rep(start_value, length(future_years))
future_data_245 <- data.frame(Code = select.country, Label = "SSP245", Year = future_years, co2_per_country = future_emission)
head(future_data_245)
# SSP370
end_value <- start_value * 1.35
future_years <- start_year:end_year
future_emission <- seq(start_value, end_value, length.out = length(future_years))
future_data_370 <- data.frame(Code = select.country, Label = "SSP370", Year = future_years, co2_per_country = future_emission)
head(future_data_370)

# Covariates
data_from_gbd <- all_causes_standardized_both[, c("Code", "Year", "temp", "pm", "ozone", "DPT", 
                                                  "age65", "gini", "physician", "land_area")]
data_sub_gbd <- subset(data_from_gbd, Code == select.country & Year >= 2010 & Year <= 2019)

# GDP
gdp <- read.csv("C:/Users/Administrator 007/Desktop/科研/春晖计划/data/1106/GDP-per-capita.csv")
gdp_long <- gdp %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year", 
    values_to = "GDP_per_capita"
  ) %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))
data_sub_gdp <- subset(gdp_long, Code == select.country & Year >= 2014 & Year <= 2023)

# Base data
base_year <- 2019
base_data <- data.frame(
  temp_base = mean(data_sub_gbd$temp[data_sub_gbd$Year == base_year], na.rm = TRUE),
  pm_base = mean(data_sub_gbd$pm[data_sub_gbd$Year == base_year], na.rm = TRUE),
  ozone_base = mean(data_sub_gbd$ozone[data_sub_gbd$Year == base_year], na.rm = TRUE),
  DPT_base = mean(data_sub_gbd$DPT[data_sub_gbd$Year == base_year], na.rm = TRUE),
  age65_base = mean(data_sub_gbd$age65[data_sub_gbd$Year == base_year], na.rm = TRUE),
  GDP_base = mean(gdp_long$GDP_per_capita[gdp_long$Code == select.country & gdp_long$Year == base_year], na.rm = TRUE),
  physician_base = mean(data_sub_gbd$physician[data_sub_gbd$Year == base_year], na.rm = TRUE),
  gini_base = mean(data_sub_gbd$gini[data_sub_gbd$Year == base_year], na.rm = TRUE),
  area_base = mean(data_sub_gbd$land_area[data_sub_gbd$Year == base_year], na.rm = TRUE)
)

generate_scenario_data <- function(scenario_label, start_year, end_year,
                                   temp_rate, pm_rate, ozone_rate,
                                   gdp_rate, base_data) {
  years <- start_year:end_year
  
  df <- data.frame(
    Year = years,
    Code = select.country,
    Label = scenario_label,
    land_area = base_data$area_base
  )
  
  # Environmental variables prediction
  if(scenario_label == "SSP245"){
    df$temp <- predict(lm(temp ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
    df$pm <- predict(lm(pm ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
    df$ozone <- predict(lm(ozone ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
    df$GDP_per_capita <- predict(lm(GDP_per_capita ~ Year, data_sub_gdp), newdata = data.frame(Year = years))
  } else {
    df$temp <- base_data$temp_base * (1 + temp_rate)^(years - base_year)
    df$pm <- base_data$pm_base * (1 + pm_rate)^(years - base_year)
    df$ozone <- base_data$ozone_base * (1 + ozone_rate)^(years - base_year)
    df$GDP_per_capita <- base_data$GDP_base * (1 + gdp_rate)^(years - base_year)
  }
  
  # Other variables prediction
  df$DPT <- predict(lm(DPT ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
  df$age65 <- predict(lm(age65 ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
  df$physician <- predict(lm(physician ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
  df$gini <- predict(lm(gini ~ Year, data_sub_gbd), newdata = data.frame(Year = years))
  return(df)
}

model_temp <- lm(temp ~ Year, data = data_sub_gbd)
model_pm <- lm(pm ~ Year, data = data_sub_gbd)
model_ozone <- lm(ozone ~ Year, data = data_sub_gbd)
model_gdp <- lm(GDP_per_capita ~ Year, data = data_sub_gdp)

scenario_data_126 <- generate_scenario_data(
  "SSP126", 2020, 2050,
  temp_rate = -0.02, pm_rate = -0.02, ozone_rate = -0.02,
  gdp_rate = 0.04, base_data = base_data
)

scenario_data_245 <- generate_scenario_data(
  "SSP245", 2020, 2050,
  temp_rate = NA, pm_rate = NA, ozone_rate = NA,
  gdp_rate = NA, base_data = base_data
)

scenario_data_370 <- generate_scenario_data(
  "SSP370", 2020, 2050,
  temp_rate = 0.02, pm_rate = 0.02, ozone_rate = 0.02,
  gdp_rate = 0.02, base_data = base_data
)

# Data combine
scenario_data_126 <- scenario_data_126 %>%
  left_join(future_data_126[, c("Code", "Year", "co2_per_country")], by = c("Code", "Year"))
scenario_data_245 <- scenario_data_245 %>%
  left_join(future_data_245[, c("Code", "Year", "co2_per_country")], by = c("Code", "Year"))
scenario_data_370 <- scenario_data_370 %>%
  left_join(future_data_370[, c("Code", "Year", "co2_per_country")], by = c("Code", "Year"))

# Data combine
future_data <- bind_rows(scenario_data_126, scenario_data_245, scenario_data_370) %>%
  left_join(
    population_future %>% mutate(Year = as.integer(Year)), 
    by = c("Code", "Year")
  ) %>%
  mutate(
    density_both = population / land_area,
    co2_both = co2_per_country / population
  ) %>%
  select(-land_area)

# Rename
names(future_data) <- c("Year", "Code", "Label", "temp_both", "pm_both", "ozone_both",
                        "gdp_both", "DPT_both", "age65_both", "physician_both", "gini_both",
                        "co2_per_country", "population", "density_both", "co2_both")

# Data cleaning
future_data$temp_both[future_data$temp_both > 99] <- 99
future_data$pm_both[future_data$pm_both > 99] <- 99
future_data$ozone_both[future_data$ozone_both > 99] <- 99
future_data$DPT_both[future_data$DPT_both > 99] <- 99
future_data$co2_both[future_data$co2_both < 0] <- 0
future_data$gini_both[future_data$gini_both < 0] <- 0
future_data$gini_both[future_data$gini_both > 1] <- 1
future_data$physician_both[future_data$physician_both < 0] <- 0


# Mortality prediction
library(splines)
Year <- all_causes_standardized_both$Year
model_both_fixed <- lm(
  val_both ~ ns(co2_both, df = 4) + gdp_both + temp_both + pm_both +
    ozone_both + DPT_both + age65_both + gini_both + density_both + physician_both + ns(Year, df = 3) + Code, 
  data = all_causes_standardized_both)
future_data <- future_data %>%
  mutate(
    pred = predict(model_both_fixed, newdata = .),
    pred_lwr = pred - 1.96 * sd(model_both_fixed$residuals),
    pred_upr = pred + 1.96 * sd(model_both_fixed$residuals)
  )

# Plot
library(ggplot2)
ggplot(future_data, aes(x = Year, y = pred, color = Label, fill = Label)) +
  geom_line(size = 1) +
  labs(title = "China",  # Change to the desired country
       x = "Year",
       y = "Death rate (per 100,000 population)") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2025, 2040),
                     breaks = seq(2025, 2040, by = 5)) +
  scale_color_manual(values = c("SSP126" = "#00BA38", "SSP245" = "#619CFF", "SSP370" = "#C86193")) +
  scale_y_continuous(limits = c(450, 750), breaks = seq(0, 1000, by = 50)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right")


#------------------- Inequity analysis -------------------
library(ineq)
library(ggplot2)

df <- all_causes_standardized_both[all_causes_standardized_both$Year == 2019, ]
df_sorted <- df[order(df$ghg_per_capita), ]
lorenz_curve <- Lc(df_sorted$life_expectancy)
concentration_index <- 2 * Gini(df_sorted$life_expectancy) - 1
cat("Concentration Index (CI): ", concentration_index, "\n")

df <- all_causes_standardized_both[all_causes_standardized_both$Year == 2019, ] # Change to the desired year

df_top10 <- df %>%
  arrange(desc(population)) %>%
  slice_head(n = 10)

ggplot(df, aes(x = ghg_per_capita, y = life_expectancy, size = population)) +
  geom_point(color = "#F5AD92", alpha = 0.6, shape = 21, fill = "#F5AD92", stroke = 0.5) +
  geom_text(
    data = df_top10,
    aes(label = Code),
    color = "black",
    size = 3,
    nudge_y = 0,
    check_overlap = TRUE
  ) +
  geom_smooth(
    method = "loess", 
    se = FALSE, 
    color = "#4484B1", 
    linewidth = 1.2,
    linetype = "dashed",
    formula = y ~ x,
    inherit.aes = FALSE,
    aes(x = ghg_per_capita, y = life_expectancy)
  ) +
  scale_size_continuous(
    name = "Population (Million)",
    range = c(2, 15),
    breaks = c(2e7, 1e8, 5e8, 1e9),
    labels = c("20", "100", "500", "1000"),
    guide = guide_legend(
      override.aes = list(color = "#F5EBAE", alpha = 0.6),
      title.position = "top"
    )
  ) +
  scale_x_continuous(
    name = "GHG emissions per capita (tons)",
    limits = c(0, max(df$ghg_per_capita)*1.1),
    expand = c(0, 0),
    breaks = seq(0, 40, by = 4)) +
  scale_y_continuous(
    name = "Life expectancy (years-old)",
    limits = c(floor(min(df$life_expectancy)*0.95), ceiling(max(df$life_expectancy)*1.05)),
    expand = c(0, 0),
    breaks = seq(40, 85, by = 5)) +
  theme_classic() +
  labs(caption = "(2019)") +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(t = 5)),
    legend.position = "none"
  )


# CI curve
library(ineq)
library(ggplot2)
library(dplyr)
library(ggthemes)

df <- all_causes_standardized_both
concentration_index <- c()

# Calculate CI for each year
for (year in unique(df$Year)) {
  year_data <- df %>% filter(Year == year)
  year_data_sorted <- year_data[order(year_data$ghg_per_capita), ]
  gini_value <- Gini(year_data_sorted$life_expectancy)
  ci_value <- 2 * gini_value - 1
  concentration_index <- c(concentration_index, ci_value)
}

concentration_data <- data.frame(
  Year = unique(df$Year),
  Concentration_Index = concentration_index
)

summary(lm(Concentration_Index ~ Year, data = concentration_data))

ggplot(concentration_data, aes(x = Year, y = Concentration_Index)) +
  geom_point(color = "#d62728", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#2ca02c", linetype = "dashed", size = 0.8) +
  labs(
    x = "Year", 
    y = "Concentration Index",
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2020, by = 5),
    limits = c(1990, 2019),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.9, -0.75),
    breaks = seq(-9, -0.75, by = 0.05),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) 