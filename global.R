library(shiny)
library(plotly)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(DT)
library(bslib)
library(thematic)
library(shinycssloaders)
library(arrow)
library(shinyWidgets)
library(metill)
library(visitalaneysluverds)
library(shinyjs)

shinyOptions(plot.autocolor = TRUE)
thematic_on()

##### Data #####
# Ársreikningagögn

throun_d <- read_parquet(
  here("data", "throun_data.parquet")
)

dreifing_d <- read_parquet(
  here("data", "dreifing_data.parquet")
)

vidmid_d <- read_parquet(
  here("data", "vidmid_data.parquet")
)

# Fasteignagjöld
fasteignagjold <- read_parquet("data/fasteignagjold.parquet")

##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
  br(" "),
  h5("Höfundur:"),
  p("Brynjólfur Gauti Guðrúnar Jónsson"),
  HTML("<a href='https://github.com/metill-is/shiny_sveitarfelog' target='_top'> Kóði og gögn </a><br>") # nolint
)
# This is the caption for plots
caption <- ""

##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
theme_set(
  theme_metill() +
    theme(
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.001, units = "npc"),
      legend.key.width = unit(0.001, units = "npc")
    )
)



light <- bs_theme(
  bootswatch = "flatly",
  primary = "#484D6D",
  secondary = "#969696",
  success = "#969696",
  light = "#faf9f9",
  dark = "#484D6D",
  bg = "#faf9f9",
  fg = "#737373",
  "body-bg" = "#faf9f9"
)
