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

# transform data for geom_ribbon
ribbon <- employment %>%
  select(year, is_rural, indexed_value) %>%
  pivot_wider(names_from = is_rural,
              values_from = indexed_value) %>%
  mutate(
    year = as.Date(paste0(year, "-01-01")),
    Nonrural = round(Nonrural*100, 0.1),
    Rural = round(Rural*100, 0.1))

# build the graphic
fig2 <- employment %>%
  # downselect columns
  select(year, is_rural, indexed_value) %>%
  mutate(
    # convert year to Date to use scale_x_date 
    year = as.Date(paste0(year, "-01-01")),
    # create new index column using 1969 = 100
    index = round(indexed_value*100, 0.1)) %>%
  ggplot(
    aes(x = year,
        y = index,
        color = is_rural)) +
  # emphasize horizontal axis
  geom_hline(
    yintercept = 100,
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
    limits = c(100, 250),
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
    subtitle = "Employment levels, 1969 = 100",
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
    y = 215,
    label = "Nonrural",
    fontface = "bold",
    color = "#211448",
    size = 6) +
  # add direct label for rural line
  annotate(
    "text",
    x = as.Date("2010-06-01"),
    y = 142,
    label = "Rural",
    fontface = "bold",
    color = "#00825B",
    size = 6)

# view
fig2

# export graphic
save_plot(
  fig2,
  here("export/fig2_remake.png")
)

# # identify latest year
# latest_date <- employment %>%
#   pull(year) %>%
#   max()
# 
# # determine the value and label at the most recent year for each line
# line_labels <- employment %>%
#   filter(year == latest_date) %>%
#   arrange(desc(indexed_value)) %>%
#   pull(is_rural)
# 
# line_values <- employment %>%
#   filter(year == latest_date) %>%
#   arrange(desc(indexed_value)) %>%
#   pull(indexed_value)
# 
# # transform data for geom_ribbon
# ribbon <- employment %>%
#   select(year, is_rural, indexed_value) %>%
#   pivot_wider(names_from = is_rural,
#               values_from = indexed_value) %>%
#   mutate(year = as.Date(paste0(year, "-01-01")))

# # build the graphic
# fig2 <- employment %>%
#   # convert year to Date to use scale_x_date
#   mutate(year = as.Date(paste0(year, "-01-01"))) %>%
#   ggplot(
#     aes(x = year,
#         y = indexed_value,
#         color = is_rural)) +
#   # add ribbon to fill in area between lines
#   geom_ribbon(
#     data = ribbon,
#     # ribbon should not inherit the global aesthetics
#     inherit.aes = FALSE,
#     # define aesthetics for this layer
#     aes(
#       x = year,
#       ymin = Rural,
#       ymax = Nonrural),
#     fill = "#af7d95",
#     alpha = 0.5) +
#   # lines on top of the ribbon
#   geom_line(linewidth = 2) +
#   # use rural vs nonrural palette
#   scale_color_cori(palette = "ctg2ruralnonrural",
#                    reverse = TRUE) +
#   # use dates for x-axis 
#   scale_x_date(
#     breaks = seq(
#       as.Date("1970-01-01"), 
#       as.Date("2020-01-01"), 
#       by = "10 years"),
#     date_labels =  "%Y",
#     expand = expansion(mult = c(0, 0))) +
#   # use percent for y-axis and set limits
#   scale_y_continuous(
#     labels = label_percent(accuracy = 1),
#     limits = c(1, 2.5),
#     expand = expansion(mult = c(0.05, 0.1)),
#     # Add the direct labels as a second axis
#     sec.axis = sec_axis(
#       transform = ~.*1,
#       breaks = line_values,
#       labels = line_labels)) +
#   theme_cori() +
#   theme(
#     # remove the legend
#     legend.position = "none",
#     # add ticks to the x-axis
#     axis.ticks.x = element_line(color = "#d0d2ce", linewidth = .25),
#     axis.ticks.length = unit(8, 'pt'),
#     # add margin to separate tick and text
#     axis.text.x = element_text(margin = margin(t = 2)),
#     # adjust the space between the line and the line label
#     axis.ticks.length.y.right = unit(2, "pt"),
#     # left justify the direct labels on the secondary axis
#     axis.text.y.right = element_text(hjust = 0)) +
#   labs(
#     title = "The widening gap in rural and nonrural employment",
#     subtitle = "Employment relative to 1969 levels",
#     x = NULL,
#     y = NULL,
#     caption = paste0(
#       "Source: Bureau of Economic Analysis\n",
#       "Note: “Rural” refers to the nonmetro definition which includes all ",
#       "nonmetro counties."))
# 
# # view
# fig2

# # export graphic
# save_plot(
#   fig2,
#   here("export/fig2_remake.png")
# )

# rural vs nonrural sector ------------------------------------------------

