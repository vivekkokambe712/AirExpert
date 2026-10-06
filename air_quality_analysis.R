# ==========================================
# AIR QUALITY ANALYSIS & VISUALIZATION
# ==========================================

# Load tidyverse
library(tidyverse)

# ==========================================
# 1. LOAD DATASET
# ==========================================

air_data <- read_csv("city_day.csv")

# Display first 6 rows
head(air_data)

# ==========================================
# 2. UNDERSTAND DATASET
# ==========================================

# Number of rows and columns
dim(air_data)

# Column names
names(air_data)

# Structure of dataset
str(air_data)

# Statistical summary
summary(air_data)

# ==========================================
# 3. MISSING VALUE ANALYSIS
# ==========================================

# Count missing values in each column
missing_values <- colSums(is.na(air_data))

print(missing_values)

# Calculate percentage of missing values
missing_percentage <- round(
  colMeans(is.na(air_data)) * 100,
  2
)

print(missing_percentage)

# ==========================================
# 4. DATA CLEANING
# ==========================================

# Remove rows where AQI is missing
aqi_data <- air_data %>%
  filter(!is.na(AQI))

# Check cleaned dataset
dim(aqi_data)

# Check missing AQI values
sum(is.na(aqi_data$AQI))



# Count each AQI category
table(aqi_data$AQI_Bucket)



# ==========================================
# 5. BASIC AQI ANALYSIS
# ==========================================

# Mean AQI
mean(aqi_data$AQI)

# Median AQI
median(aqi_data$AQI)

# Minimum AQI
min(aqi_data$AQI)

# Maximum AQI
max(aqi_data$AQI)

# Standard deviation
sd(aqi_data$AQI)


# ==========================================
# 6. CITY-WISE AQI ANALYSIS
# ==========================================

city_aqi <- aqi_data %>%
  group_by(City) %>%
  summarise(
    Average_AQI = mean(AQI, na.rm = TRUE),
    Maximum_AQI = max(AQI, na.rm = TRUE),
    Records = n()
  ) %>%
  arrange(desc(Average_AQI))

print(city_aqi)


# ==========================================
# 7. AQI CATEGORY DISTRIBUTION
# ==========================================

p1 <- ggplot(aqi_data, aes(x = AQI_Bucket)) +
  geom_bar() +
  labs(
    title = "Distribution of AQI Categories",
    x = "AQI Category",
    y = "Number of Records"
  ) +
  theme_minimal()

ggsave(
  "outputs/aqi_category_distribution.png",
  p1,
  width = 10,
  height = 6
)


# ==========================================
# 8. AVERAGE AQI BY CITY
# ==========================================

p2 <- ggplot(
  city_aqi,
  aes(
    x = reorder(City, Average_AQI),
    y = Average_AQI
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average AQI by City",
    x = "City",
    y = "Average AQI"
  ) +
  theme_minimal()

ggsave(
  "outputs/average_aqi_by_city.png",
  p2,
  width = 10,
  height = 8
)


# ==========================================
# 9. AQI DISTRIBUTION
# ==========================================

p3 <- ggplot(aqi_data, aes(x = AQI)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of AQI",
    x = "AQI",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  "outputs/aqi_distribution.png",
  p3,
  width = 10,
  height = 6
)


# ==========================================
# 10. AVERAGE POLLUTANT LEVELS
# ==========================================

pollutant_summary <- aqi_data %>%
  summarise(
    PM2.5 = mean(PM2.5, na.rm = TRUE),
    PM10 = mean(PM10, na.rm = TRUE),
    NO = mean(NO, na.rm = TRUE),
    NO2 = mean(NO2, na.rm = TRUE),
    NOx = mean(NOx, na.rm = TRUE),
    NH3 = mean(NH3, na.rm = TRUE),
    CO = mean(CO, na.rm = TRUE),
    SO2 = mean(SO2, na.rm = TRUE),
    O3 = mean(O3, na.rm = TRUE),
    Benzene = mean(Benzene, na.rm = TRUE),
    Toluene = mean(Toluene, na.rm = TRUE),
    Xylene = mean(Xylene, na.rm = TRUE)
  )

print(pollutant_summary)


# ==========================================
# 11. CORRELATION WITH AQI
# ==========================================

correlation_data <- aqi_data %>%
  select(
    AQI,
    PM2.5,
    PM10,
    NO,
    NO2,
    NOx,
    NH3,
    CO,
    SO2,
    O3,
    Benzene,
    Toluene,
    Xylene
  )

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

print(round(correlation_matrix, 2))

# ==========================================
# 12. AQI-POLLUTANT CORRELATION
# ==========================================

aqi_correlations <- correlation_matrix["AQI", ]

correlation_df <- data.frame(
  Pollutant = names(aqi_correlations),
  Correlation = as.numeric(aqi_correlations)
) %>%
  filter(Pollutant != "AQI") %>%
  arrange(desc(Correlation))

print(correlation_df)


p4 <- ggplot(
  correlation_df,
  aes(
    x = reorder(Pollutant, Correlation),
    y = Correlation
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Correlation of Pollutants with AQI",
    x = "Pollutant",
    y = "Correlation with AQI"
  ) +
  theme_minimal()

ggsave(
  "outputs/pollutant_aqi_correlation.png",
  p4,
  width = 10,
  height = 7
)

# ==========================================
# 13. MONTHLY AQI TREND
# ==========================================

monthly_aqi <- aqi_data %>%
  mutate(
    Year_Month = floor_date(Date, "month")
  ) %>%
  group_by(Year_Month) %>%
  summarise(
    Average_AQI = mean(AQI, na.rm = TRUE)
  )

print(head(monthly_aqi))


p5 <- ggplot(
  monthly_aqi,
  aes(
    x = Year_Month,
    y = Average_AQI
  )
) +
  geom_line() +
  labs(
    title = "Monthly Average AQI Trend",
    x = "Date",
    y = "Average AQI"
  ) +
  theme_minimal()

ggsave(
  "outputs/monthly_aqi_trend.png",
  p5,
  width = 12,
  height = 6
)


# ==========================================
# 14. TOP 10 CITIES BY AVERAGE AQI
# ==========================================

top_cities <- city_aqi %>%
  slice_max(
    order_by = Average_AQI,
    n = 10
  )

print(top_cities)

p6 <- ggplot(
  top_cities,
  aes(
    x = reorder(City, Average_AQI),
    y = Average_AQI
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Cities by Average AQI",
    x = "City",
    y = "Average AQI"
  ) +
  theme_minimal()

ggsave(
  "outputs/top_10_cities_aqi.png",
  p6,
  width = 10,
  height = 6
)