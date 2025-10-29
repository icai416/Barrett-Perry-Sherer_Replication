
# Notes:
# File paths all relative. Set working directory to wherever the .dta file is and the figures will be saved there


library(haven)     # For reading .dta files
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting


senate_data <- read_dta("voldenwaiwiseman_senate_perspectives_replication_final.dta")


senate_collapsed <- senate_data %>%
  filter(!is.na(dem), !is.na(state_leg)) %>%
  group_by(congress, dem) %>%
  summarize(state_leg = mean(state_leg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(state_leg = 100 * state_leg)  # Convert to percent


senate_collapsed <- senate_collapsed %>%
  mutate(party = factor(dem, levels = c(0, 1), labels = c("Republicans", "Democrats"))) %>%
filter(!is.na(party))


ggplot(senate_collapsed, aes(x = congress, y = state_leg, color = party)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(min(senate_collapsed$congress), max(senate_collapsed$congress), by = 2)) +  # More ticks here
  labs(
    title = "Percent of Senators Who Previously Served in State Legislatures, 1973-2021",
    y = "Percent of Senators",
    x = "Congress",
    color = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    legend.position = c(0.95, 0.95),  # (x, y) inside plot; values between 0 and 1
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)  # semi-transparent background for better readability
  )
ggsave("senate_state_leg.png", width = 7.5, height = 5, units = "in")
ggsave("senate_state_leg.pdf", width = 7.5, height = 5, units = "in")



house_data <- read_dta("voldenwaiwiseman_house_perspectives_replication_final.dta")


house_collapsed <- house_data %>%
  filter(!is.na(dem), !is.na(state_leg)) %>%
  group_by(congress, dem) %>%
  summarize(state_leg = mean(state_leg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(state_leg = 100 * state_leg)  # Convert to percent


house_collapsed <- house_collapsed %>%
  mutate(party = factor(dem, levels = c(0, 1), labels = c("Republicans", "Democrats"))) %>%
  filter(!is.na(party))

ggplot(house_collapsed, aes(x = congress, y = state_leg, color = party)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(min(house_collapsed$congress), max(house_collapsed$congress), by = 2)) +  # More ticks here
  labs(
    title = "Percent of House Members Who Previously Served in State Legislatures, 1973-2021",
    y = "Percent of House",
    x = "Congress",
    color = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    legend.position = c(0.95, 0.95),  # (x, y) inside plot; values between 0 and 1
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)  # semi-transparent background for better readability
  )
ggsave("house_state_leg.png", width = 7.5, height = 5, units = "in")
ggsave("house_state_leg.pdf", width = 7.5, height = 5, units = "in")








