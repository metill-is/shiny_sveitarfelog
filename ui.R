ui <- shinyUI(
  ui = navbarPage(
    title = "Ársreikningar sveitarfélaga",
    tabPanel(
      title = "Þróun",
      throun_ui("throun")
    ),
    tabPanel(
      title = "Dreifing",
      dreifing_ui("dreifing")
    ),
    tabPanel(
      title = "Viðmið",
      vidmid_ui("vidmid")
    ),
    theme = light
  )
)
