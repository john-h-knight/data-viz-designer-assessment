# setup -------------------------------------------------------------------

# load libraries
library(tidyverse)
library(cori.charts)
library(scales)
library(here)
library(rsvg)

# declare location of the current script
i_am("R/graphics.R")

# load Lato and Bitter fonts
cori.charts::load_fonts()

# load data for rural vs nonrural employment graphic (fig 2)
employment <- read_csv(
  here("data", "rural_nonrural_employment.csv")
)

# load data for rural vs nonrural sector graphic (fig 7)
sector <- read_csv(
  here("data", "sectoral_employment.csv")
)

# rural vs nonrural employment --------------------------------------------

# per cori.charts documentation, update the geom defaults prior to plotting
update_cori_geom_defaults()

# identify latest year
latest_date <- employment %>%
  pull(year) %>%
  max()

# determine the value and label at the most recent year for each line
line_labels <- employment %>%
  filter(year == latest_date) %>%
  arrange(desc(indexed_value)) %>%
  pull(is_rural)

line_values <- employment %>%
  filter(year == latest_date) %>%
  arrange(desc(indexed_value)) %>%
  pull(indexed_value)

# transform data for geom_ribbon
ribbon <- employment %>%
  pivot_wider(names_from = is_rural,
              values_from = indexed_value) %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

# build the graphic
fig2 <- employment %>%
  # convert year to Date
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  ggplot(
    aes(x = year,
        y = indexed_value,
        color = is_rural)) +
  geom_line(linewidth = 2) +
  geom_ribbon(
    data = ribbon,
    inherit.aes = FALSE,
    aes(
      x = year,
      ymin = Rural,
      ymax = Nonrural),
    fill = "#af7d95",
    alpha = 0.5) +
  scale_color_cori(palette = "ctg2ruralnonrural",
                   reverse = TRUE) +
  scale_x_date(
    breaks = seq(
      as.Date("1970-01-01"), 
      as.Date("2020-01-01"), 
      by = "10 years"),
    date_labels =  "%Y",
    expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(1, 2.5),
    expand = expansion(mult = c(0.05, 0.1)),
    # Add the direct labels as a second axis
    sec.axis = sec_axis(
      transform = ~.*1,
      breaks = line_values,
      labels = line_labels)) +
  theme_cori() +
  theme(
    # remove the legend
    legend.position = "none",
    # add ticks to the x-axis
    axis.ticks.x = element_line(color = "#d0d2ce", linewidth = .25),
    axis.ticks.length = unit(8, 'pt'),
    # add margin to separate tick and text
    axis.text.x = element_text(margin = margin(t = 2)),
    # adjust the space between the line and the line label
    axis.ticks.length.y.right = unit(2, "pt"),
    # left justify the direct labels on the secondary axis
    axis.text.y.right = element_text(hjust = 0)) +
  labs(
    title = "The widening gap in rural and nonrural employment",
    subtitle = "Employment relative to 1969 levels",
    x = NULL,
    y = NULL,
    caption = "Source: Bureau of Economic Analysis")

# view
fig2

# export graphic
save_plot(
  fig2,
  here("export/fig2_remake.png")
)

# rural vs nonrural sector ------------------------------------------------


