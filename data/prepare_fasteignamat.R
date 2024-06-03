library(tidyverse)
library(readxl)
library(arrow)

fasteignamat <- read_excel(
    "data/fasteignagjold.xlsx", 
    skip = 8,
    col_names = c(
        "svfn",
        "sveitarfelag",
        "utsvar",
        "fskattur_a",
        "fskattur_b",
        "fskattur_c",
        "fraveitugjald",
        "vatnsgjald",
        "sorphreinsun_tunnugjald",
        "sorphreinsun_eydingargjald",
        "lodarleiga_ibudir",
        "lodarleiga_fyrirtaeki",
        "fjoldi_gjalda"
    ),
    col_types = rep("text", 13)
) |>
    mutate(
        sveitarfelag = str_replace_all(sveitarfelag, "[0-9]+\\)", "") |>
            str_replace_all("1 \\)", "") |>
            str_squish()
    ) |>
    drop_na(svfn) |>
    select(sveitarfelag, fskattur_a, fraveitugjald, vatnsgjald) |>
    mutate_at(
        vars(fskattur_a, fraveitugjald, vatnsgjald), 
        ~ ifelse(str_detect(., "kr") | is.na(.), "0", as.character(.)) |>
            str_replace(",", "\\.") |>
            parse_number()
    ) |>
    mutate(fasteignamat = (fskattur_a + fraveitugjald + vatnsgjald) / 100) |>
    select(sveitarfelag, fasteignamat)

d <- read_excel(
    "data/Sveitarfélög eftir tegundum eigna.xlsx"
) |> 
    janitor::clean_names() |> 
    filter(
        tegund_eigna == "Íbúðareignir"
    ) |> 
    mutate(
        haekkun_verd = (fasteignamat_2025 - fasteignamat_2024) / fjoldi
    ) |> 
    select(
        sveitarfelag, haekkun_verd
    ) |> 
    inner_join(
        fasteignamat
    ) |> 
    mutate(
        haekkun_mat = haekkun_verd * fasteignamat
    ) |> 
    select(sveitarfelag, fasteignamat, haekkun_mat) |> 
    pivot_longer(c(-sveitarfelag))


d |> 
    write_parquet("data/fasteignagjold.parquet")
