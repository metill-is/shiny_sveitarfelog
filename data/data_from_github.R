library(arrow)
library(here)



# Þróun -------------------------------------------------------------------

throun_url <- "https://raw.githubusercontent.com/bgautijonsson/Metill.is/master/maelabord/sveitarfelog/gogn/throun_data.parquet"
throun_d <- tempfile(fileext = ".parquet")
download.file(throun_url, destfile = throun_d)
throun_d <- read_parquet(throun_d)

throun_d |> 
    write_parquet(
        here("data", "throun_data.parquet")
    )



# Dreifing ----------------------------------------------------------------

dreifing_url <- "https://raw.githubusercontent.com/bgautijonsson/Metill.is/master/maelabord/sveitarfelog/gogn/dreifing_data.parquet"
dreifing_d <- tempfile(fileext = ".parquet")
download.file(dreifing_url, destfile = dreifing_d)
dreifing_d <- read_parquet(dreifing_d)

dreifing_d |> 
    write_parquet(
        here("data", "dreifing_data.parquet")
    )



# Viðmið ------------------------------------------------------------------


vidmid_url <- "https://raw.githubusercontent.com/bgautijonsson/Metill.is/master/maelabord/sveitarfelog/gogn/vidmid_data.parquet"
vidmid_d <- tempfile(fileext = ".parquet")
download.file(vidmid_url, destfile = vidmid_d)
vidmid_d <- read_parquet(vidmid_d)

vidmid_d |> 
    write_parquet(
        here("data", "vidmid_data.parquet")
    )
