# ==============================================================================
# DATA MINING FINAL PROJECT - WEEK 4/5 DELIVERABLE
# Script Name: 03_eda.R (Exploratory Data Analysis)
# Role: Data Visualization Lead
# Target: Create and save 5 core visualizations for the report
# ==============================================================================

# --- STEP 1: LOAD NECESSARY LIBRARIES ---
library(tidyverse)
library(scales)

# Set seed for reproducibility (as required by grading standards)
set.seed(2026)

# Create an output folder if it doesn't exist yet
if (!dir.exists("output")) {
  dir.create("output")
}

# --- STEP 2: LOAD DATA WITH SMART FALLBACK PATHS ---
# This looks for the file in common spots before bringing up a manual file picker
data_path <- "hotel_bookings/hotel_bookings.csv"

if (file.exists(data_path)) {
  hotel_data <- read_csv(data_path)
} else if (file.exists("hotel_bookings.csv")) {
  hotel_data <- read_csv("hotel_bookings.csv")
} else {
  message("Could not find the CSV automatically. Please select it in the popup:")
  hotel_data <- read_csv(file.choose())
}

# --- STEP 3: PREPARE AND FORMAT VARIABLES ---
# Order the months chronologically so our line graphs display correctly
months_ordered <- c(
  "January", "February", "March", "April", "May", "June", 
  "July", "August", "September", "October", "November", "December"
)

# Convert categorical columns to clean factors for plotting
clean_data <- hotel_data %>%
  mutate(
    booking_status = factor(
      is_canceled, 
      levels = c(0, 1), 
      labels = c("Checked In", "Canceled")
    ),
    hotel = as.factor(hotel),
    arrival_month = factor(arrival_date_month, levels = months_ordered),
    deposit_type = as.factor(deposit_type),
    market_segment = as.factor(market_segment)
  )

# --- STEP 4: GENERATE PLOTS ---

# PLOT 1: Cancellation Rates by Hotel Type
plot_cancellations <- ggplot(clean_data, aes(x = hotel, fill = booking_status)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#3182bd", "#e6550d")) + # Blue and orange
  labs(
    title = "Cancellation Rates by Hotel Type",
    subtitle = "City hotels show a higher rate of cancellations than resort hotels.",
    x = "Type of Hotel",
    y = "Percentage of Bookings",
    fill = "Booking Status"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

print(plot_cancellations)

# PLOT 2: Lead Time Distribution (Boxplot)
plot_lead_time <- ggplot(clean_data, aes(x = booking_status, y = lead_time, fill = booking_status)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  scale_fill_manual(values = c("#3182bd", "#e6550d")) +
  labs(
    title = "Lead Time Distribution by Booking Status",
    subtitle = "Guests who cancel tend to book much further in advance.",
    x = "Booking Outcome",
    y = "Lead Time (Number of Days)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none"
  ) +
  coord_flip()

print(plot_lead_time)

# PLOT 3: Monthly Booking Volatility (Line Plot)
monthly_summary <- clean_data %>%
  group_by(arrival_month, hotel) %>%
  summarise(total_bookings = n(), .groups = 'drop')

plot_monthly_trends <- ggplot(monthly_summary, aes(
  x = arrival_month, 
  y = total_bookings, 
  group = hotel, 
  color = hotel
)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#2ca25f", "#756bb1")) + # Green and purple
  labs(
    title = "Monthly Booking Trends Across Hotels",
    subtitle = "Bookings spike heavily during the summer holiday months.",
    x = "Month of Arrival",
    y = "Total Bookings Made",
    color = "Hotel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hj = 1),
    legend.position = "top"
  )

print(plot_monthly_trends)

# PLOT 4: Deposit Types vs Cancellations (Bar Plot)
plot_deposit_impact <- ggplot(clean_data, aes(x = deposit_type, fill = booking_status)) +
  geom_bar(position = "fill", width = 0.55) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#3182bd", "#e6550d")) +
  labs(
    title = "How Deposit Policies Impact Cancellations",
    subtitle = "Non-refundable deposits paradoxically show the highest cancellation rates.",
    x = "Required Deposit Type",
    y = "Percentage of Bookings",
    fill = "Booking Status"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

print(plot_deposit_impact)

# PLOT 5: Market Segment Distribution
plot_market_segments <- ggplot(clean_data, aes(x = market_segment, fill = hotel)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = c("#2ca25f", "#756bb1")) +
  labs(
    title = "Distribution of Bookings by Market Segment",
    subtitle = "Online Travel Agents make up the largest source of booking traffic.",
    x = "Market Segment Category",
    y = "Total Bookings",
    fill = "Hotel Property"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 35, hj = 1),
    legend.position = "top"
  )

print(plot_market_segments)

# --- STEP 5: AUTOMATICALLY SAVE PLOTS TO OUTPUT DIRECTORY ---
# Using high-quality dpi settings for professional presentation slides
ggsave("output/01_cancellation_rates.png", plot_cancellations, width = 7, height = 4.5, dpi = 300)
ggsave("output/02_lead_time.png", plot_lead_time, width = 7, height = 4.5, dpi = 300)
ggsave("output/03_monthly_trends.png", plot_monthly_trends, width = 7, height = 4.5, dpi = 300)
ggsave("output/04_deposit_impact.png", plot_deposit_impact, width = 7, height = 4.5, dpi = 300)
ggsave("output/05_market_segments.png", plot_market_segments, width = 8, height = 5, dpi = 300)

message("All plots have been successfully saved to your 'output/' folder!")