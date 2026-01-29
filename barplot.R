# Load packages
library(ggplot2)
library(dplyr)

# Read data
data <- read.csv("data.csv")

# Create age groups (keeps the order <18 at bottom -> >70 at top)
data$AgeGroup <- cut(
  data$Age,
  breaks = c(0, 18, 31, 51, 71, Inf),
  labels = c("<18", "18–30", "31–50", "51–70", ">70"),
  right = FALSE
)

# Count by age and sex
demographic_data <- data %>%
  count(AgeGroup, Sex)

# Convert male counts to negative
demographic_data <- demographic_data %>%
  mutate(n = ifelse(Sex == "Male", -n, n))

# Maximum absolute value for symmetric x-axis limits
max_count <- max(abs(demographic_data$n))


# Midpoint positions per AgeGroup (this will be the x coordinate for dashed markers)
midpoints <- demographic_data %>%
  mutate(n = ifelse((duplicated(AgeGroup)|duplicated(AgeGroup, fromLast=TRUE)) == 0, n / 2, n)) %>% #for unique item in age group divide by 2 
  group_by(AgeGroup) %>%
  summarise(Midpoint = mean(n))


# Plot: map x = n (numeric), y = AgeGroup (factor) -> horizontal bars
ggplot(demographic_data) +
  # Bars (horizontal because y is factor, x numeric)
  geom_col(aes(x = n, y = AgeGroup, fill = Sex),width = 0.38, color = "black", linewidth = 0.25) +
  
  # Solid central vertical line at zero (midline)
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  
  # Short vertical dashed midpoint markers inside each bar
  # We use as.numeric(AgeGroup) to place the segment centered on the category.
  geom_segment(
    data = midpoints,
    aes(x = Midpoint, xend = Midpoint,
        y = as.numeric(AgeGroup) - 0.14,
        yend = as.numeric(AgeGroup) + 0.14),
    color = "black", linetype = "twodash", linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  # X-axis symmetric ticks and labels (show absolute values)
  scale_x_continuous(
    limits = c(-max_count - 1, max_count + 1),
    breaks = seq(-max_count, max_count, by = 2),
    labels = abs(seq(-max_count, max_count, by = 2)),
    expand = expansion(add = c(0,0))
  ) +
  
  # Colors for sex
  scale_fill_manual(values = c("Male" = "#5782C7", "Female" = "#c46666")) +
  
  # Labels
  labs(
    x = "No. of cases",
    y = "Age (Years)",
    fill = "Sex",
    title = "Demographic Distribution, Year 2025"
  ) +
  
  # Classic theme with black axes (no gridlines)
  # theme_classic(base_size = 14) +
  theme_minimal(base_size = 14)+
  theme(
    # plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.title.position = "panel",
    axis.title.x = element_text(face = "bold", margin = margin(t = 9)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 9)),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    # legend.position = "right"
  )
ggsave("gender_barplot.svg",dpi = 1200, width = 8, height = 4.7)
