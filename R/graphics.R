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
  here("data", "rural_nonrural_employment.csv"))

# load data for rural vs nonrural sector graphic (fig 7)
sector <- read_csv(
  here("data", "sectoral_employment.csv"))

# rural vs nonrural employment --------------------------------------------

# transform data for geom_ribbon
ribbon <- employment %>%
  select(year, is_rural, indexed_value) %>%
  pivot_wider(names_from = is_rural,
              values_from = indexed_value) %>%
  mutate(
    year = as.Date(paste0(year, "-01-01")))

# per cori.charts documentation, update the geom defaults prior to plotting
update_cori_geom_defaults()

# build the graphic
fig2_makeover <- employment %>%
  # downselect columns
  select(year, is_rural, indexed_value) %>%
  # convert year to Date to use scale_x_date 
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  ggplot(
    aes(x = year,
        y = indexed_value,
        color = is_rural)) +
  # emphasize horizontal axis
  geom_hline(
    yintercept = 1,
    linewidth = 1,
    linetype = "solid",
    color = "black") +
  # add ribbon to fill in area between lines
  geom_ribbon(
    data = ribbon,
    # ribbon should not inherit the global aesthetics
    inherit.aes = FALSE,
    # define aesthetics for this layer
    aes(
      x = year,
      ymin = Rural,
      ymax = Nonrural),
    fill = "#211448",
    alpha = 0.1) +
  # lines on top of the ribbon based on global aesthetics
  geom_line(
    linewidth = 2,
    alpha = 1.0) +
  # use rural vs nonrural palette
  scale_color_cori(palette = "ctg2ruralnonrural",
                   reverse = TRUE) +
  # use dates for x-axis 
  scale_x_date(
    breaks = seq(
      as.Date("1970-01-01"), 
      as.Date("2020-01-01"), 
      by = "10 years"),
    date_labels =  "%Y",
    expand = expansion(mult = c(0, 0))) +
  # use percent for y-axis and set limits
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(1, 2.5),
    position = "right",
    expand = expansion(mult = c(0, 0.05))) +
  theme_cori() +
  theme(
    # remove the legend
    legend.position = "none",
    # add ticks to the x-axis
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(8, 'pt'),
    # add margin to separate tick and text
    axis.text.x = element_text(margin = margin(t = 0)),
    # use dark gray for caption text
    plot.caption = element_text(color = "darkgray")) +
  labs(
    title = "The widening gap in rural and nonrural employment",
    subtitle = "Employment relative to 1969",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Bureau of Economic Analysis\n",
      "Note: “Rural” refers to the nonmetro definition which includes all ",
      "nonmetro counties.")) +
  # add direct label for nonrural line
  annotate(
    "text",
    x = as.Date("2006-01-01"),
    y = 2.15,
    label = "Nonrural",
    fontface = "bold",
    color = "#211448",
    size = 6) +
  # add direct label for rural line
  annotate(
    "text",
    x = as.Date("2010-06-01"),
    y = 1.42,
    label = "Rural",
    fontface = "bold",
    color = "#00825B",
    size = 6)

# view
fig2_makeover

# export graphic
save_plot(
  fig2_makeover,
  here("export/fig2_makeover.png"))

# rural vs nonrural sector ------------------------------------------------

# remove duplicates
sector_distinct <- sector %>%
  distinct(sector, is_rural, year, share_sector)

# build the graphic
fig7_makeover <- sector_distinct %>%
  # filter for the most recent year
  filter(year == 2022) %>%
  # change sector from character to factor and set levels
  mutate(
    sector = factor(
      sector,
      levels = c("Local services",
                 "Tradable goods",
                 "Tradable services"))) %>%
  ggplot(
    aes(x = share_sector,
        y = sector)) +
  # connecting line between points
  geom_line(
    linewidth = 1,
    color = "black",
    alpha = 0.5) +
  # points
  geom_point(
    aes(color = is_rural),
    size = 5,
    alpha = 0.75) +
  # use rural vs nonrural palette
  scale_color_cori(palette = "ctg2ruralnonrural",
                   reverse = TRUE) +
  # use percent for x-axis
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    breaks = c(0.15, 0.25, 0.35, 0.45),
    expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_discrete(
    expand = expansion(mult = c(0.15, 0.25))) +
  theme_cori() +
  theme(
    # remove the legend
    legend.position = "none",
    # Add ticks to the x-axis
    axis.ticks.x = element_line(color = "#d0d2ce", linewidth = 0.25),
    axis.ticks.length = unit(8, 'pt'),
    # Add margin to separate tick and text
    axis.text.x = element_text(margin = margin(t = 2)),
    # use dark gray for caption text
    plot.caption = element_text(color = "darkgray")) +
  labs(
    title = paste0(
      "Tradable services represent a larger share of employment in\n",
      "nonrural areas than rural areas"),
    subtitle = "Share of employment in 2022 by sector",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Bureau of Economic Analysis\n",
      "Note: “Rural” refers to the nonmetro definition which includes all ",
      "nonmetro counties.")) +
  # add direct label for nonrural
  annotate(
    "text",
    x = 0.3,
    y = "Tradable services",
    label = "Nonrural",
    fontface = "bold",
    color = "#211448",
    size = 6,
    vjust = -1.1) +
  # add direct label for rural
  annotate(
    "text",
    x = 0.173,
    y = "Tradable services",
    label = "Rural",
    fontface = "bold",
    color = "#00825B",
    size = 6,
    vjust = -1.1)

# view
fig7_makeover

# export graphic
save_plot(
  fig7_makeover,
  here("export/fig7_makeover.png"))
