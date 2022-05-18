library(shiny)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(plotly)
library(DT)
library(bslib)
library(thematic)

##### Data #####
d <- read_csv("data/arsreikningagogn.csv") 


##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
    br(" "),
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/bgautijonsson/sveitarfelog_maelabord'> Kóði og gögn </a>")
)
# This is the caption for plots
caption <- "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/"

##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
light <- bs_theme(bootswatch = "flatly")
dark <- bs_theme(bootswatch = "superhero")
theme_set(theme_half_open(font_size = 16))
thematic_shiny()

##### List of all outcome variables #####
#  To be used by functions in utils.R

y_vars <- list(
    "Árafjöldi til niðurgreiðslu nettó skulda" = "timi_borga_skuldir",
    "Eiginfjárhlutfall" = "eiginfjarhlutfall",
    "Framlegð per íbúi (kjörtímabil í heild)" = "framlegd_per_ibui_kjortimabil",
    "Framlegð sem hlutfall af tekjum" = "framlegd_hlutf",
    "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = "framlegd_hlutf_kjortimabil",
    "Handbært fé per íbúi" = "handbaert_fe_per_ibui",
    "Jöfnunarsjóðsframlög per íbúi" = "jofnunarsjodur_a_ibua",
    "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = "hlutf_jofnunarsjods_skottum",
    "Launa- og launatengd gjöld per íbúi" = "launagjold_per_ibui",
    "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = "launagjold_hlutf_gjold",
    "Nettó jöfnunarsjóðsframlög per íbúi" = "netto_jofnunarsjod_per_ibui",
    "Nettóskuldir sem hlutfall af tekjum" = "nettoskuldir_hlutf_tekjur",
    "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = "rekstrarnidurstada_per_ibui_kjortimabil",
    "Rekstrarniðurstaða sem hlutfall af tekjum" = "rekstrarnidurstada_hlutf",
    "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = "rekstrarnidurstada_hlutf_kjortimabil",
    "Rekstrarniðurstaða undanfarinna 3 ára  sem hlutfall af tekjum" = "rekstur_3_ar_hlutf_tekjur",
    "Skuldir" = "heildarskuldir",
    "Skuldir per íbúi"  = "skuldir_per_ibui",
    "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
    "Skuldaaukning" = "skuldaaukning",
    "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = "skuldaaukning_2021",
    "Skuldahlutfall" = "skuldahlutfall",
    "Útsvar og fasteignaskattur per íbúi" = "skattur_a_ibua",
    "Veltufé frá rekstri sem hlutfall af tekjum" = "veltufe_hlutf_tekjur",
    "Veltufjárhlutfall" = "veltufjarhlutfall"
)