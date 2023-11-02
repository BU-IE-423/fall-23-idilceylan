library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"

data <- read.csv("Desktop/all_ticks_wide.csv.gz",sep = ",")

#AKBANK
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
akbank <- data[,c("timestamp","AKBNK")]
akbank2yil <- akbank[1883:22132,]
akbank2yil$AKBNK <- na.approx(akbank2yil$AKBNK)

# A function to approximate na's in data
sum(is.na(akbank2yil$timestamp))

# Extract the month and year
akbank2yil <- akbank2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(akbank2yil, aes(x = factor(month, labels = month.abb), y = AKBNK)) +
  geom_boxplot() +
  labs(x = "Month", y = "AKBNK") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "akbank2yil" with columns "timestamp" and "AKBNK"
akbank2yil <- akbank2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- akbank2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(AKBNK, na.rm = TRUE),
    sd_value = sd(AKBNK, na.rm = TRUE)
  )

# Flag outliers based on the 3-sigma rule
akbank2yil <- akbank2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(AKBNK < (mean_value - 3 * sd_value) | AKBNK > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_akbank <- akbank2yil[akbank2yil$outlier == "Outlier", ]
outliers_akbank

#GARAN
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
garan <- data[,c("timestamp","GARAN")]
garan2yil <- garan[1883:22132,]
garan2yil$GARAN <- na.approx(garan2yil$GARAN)

# A function to approximate na's in data
sum(is.na(garan2yil$timestamp))

# Extract the month and year
garan2yil <- garan2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(garan2yil, aes(x = factor(month, labels = month.abb), y = GARAN)) +
  geom_boxplot() +
  labs(x = "Month", y = "GARAN") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "garan2yil" with columns "timestamp" and "GARAN"
garan2yil <- garan2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- garan2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(GARAN, na.rm = TRUE),
    sd_value = sd(GARAN, na.rm = TRUE)
  )

# Flag outliers based on the 3-sigma rule
garan2yil <- garan2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(GARAN < (mean_value - 3 * sd_value) | GARAN > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_garan <- garan2yil[garan2yil$outlier == "Outlier", ]
outliers_garan


#ASELS
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
asels <- data[,c("timestamp","ASELS")]
asels2yil <- asels[22134:45588,]
asels2yil$ASELS <- na.approx(asels2yil$ASELS)

# A function to approximate na's in data
sum(is.na(asels2yil$timestamp))

# Extract the month and year
asels2yil <- asels2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(asels2yil, aes(x = factor(month, labels = month.abb), y = ASELS)) +
  geom_boxplot() +
  labs(x = "Month", y = "ASELS") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "asels2yil" with columns "timestamp" and "ASELS"
asels2yil <- asels2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- asels2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(ASELS, na.rm = TRUE),
    sd_value = sd(ASELS, na.rm = TRUE)
  )

# Flag outliers based on the 3-sigma rule
asels2yil <- asels2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(ASELS < (mean_value - 3 * sd_value) | ASELS > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_asels <- asels2yil[asels2yil$outlier == "Outlier", ]
outliers_asels


#PGSUS
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
pgsus <- data[,c("timestamp","PGSUS")]
pgsus2yil <- pgsus[22134:45588,]
pgsus2yil$PGSUS <- na.approx(pgsus2yil$PGSUS)

# A function to approximate na's in data
sum(is.na(pgsus2yil$timestamp))

# Extract the month and year
pgsus2yil <- pgsus2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(pgsus2yil, aes(x = factor(month, labels = month.abb), y = PGSUS)) +
  geom_boxplot() +
  labs(x = "Month", y = "PGSUS") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "pgsus2yil" with columns "timestamp" and "PGSUS"
pgsus2yil <- pgsus2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- pgsus2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(PGSUS, na.rm = TRUE),
    sd_value = sd(PGSUS, na.rm = TRUE)
  )

# Flag outliers based on the 3-sigma rule
pgsus2yil <- pgsus2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(PGSUS < (mean_value - 3 * sd_value) | PGSUS > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_pgsus <- pgsus2yil[pgsus2yil$outlier == "Outlier", ]
outliers_pgsus

#THYAO
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
thyao <- data[,c("timestamp","THYAO")]
thyao2yil <- thyao[22134:45588,]
thyao2yil$THYAO <- na.approx(thyao2yil$THYAO)

# A function to approximate na's in data
sum(is.na(thyao2yil$timestamp))

# Extract the month and year
thyao2yil <- thyao2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(thyao2yil, aes(x = factor(month, labels = month.abb), y = THYAO)) +
  geom_boxplot() +
  labs(x = "Month", y = "THYAO") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "thyao2yil" with columns "timestamp" and "THYAO"
thyao2yil <- thyao2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- thyao2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(THYAO, na.rm = TRUE),
    sd_value = sd(THYAO, na.rm = TRUE)
  )

# Flag outliers based on the 3-sigma rule
thyao2yil <- thyao2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(THYAO < (mean_value - 3 * sd_value) | THYAO > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_thyao <- thyao2yil[thyao2yil$outlier == "Outlier", ]
outliers_thyao

#BANVT
# Assuming your DataFrame is named "data" and the timestamp column is named "timestamp"
banvt <- data[,c("timestamp","BANVT")]
banvt2yil <- banvt[22134:45588,]
banvt2yil$BANVT <- na.approx(banvt2yil$BANVT)

# A function to approximate na's in data
sum(is.na(banvt2yil$timestamp))

# Extract the month and year
banvt2yil <- banvt2yil %>%
  mutate(year = year(timestamp), month = month(timestamp))

# Create separate boxplots for each combination of month and year
ggplot(banvt2yil, aes(x = factor(month, labels = month.abb), y = BANVT)) +
  geom_boxplot() +
  labs(x = "Month", y = "BANVT") +
  facet_wrap(~year, scales = "free_x")

# Assuming your DataFrame is named "banvt2yil" with columns "timestamp" and "BANVT"
banvt2yil <- banvt2yil %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>%
  mutate(year = year(timestamp), month = month(timestamp))


# Group by year and month, calculate mean and standard deviation
monthly_summary <- banvt2yil %>%
  group_by(year, month, .groups = "drop") %>%
  summarize(
    mean_value = mean(BANVT, na.rm = TRUE),
    sd_value = sd(BANVT, na.rm = TRUE)
  )


# Flag outliers based on the 3-sigma rule
banvt2yil <- banvt2yil %>%
  left_join(monthly_summary, by = c("year", "month")) %>%
  mutate(outlier = ifelse(BANVT < (mean_value - 3 * sd_value) | BANVT > (mean_value + 3 * sd_value), "Outlier", "Not Outlier"))
outliers_banvt <- banvt2yil[banvt2yil$outlier == "Outlier", ]
outliers_banvt

