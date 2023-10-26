##### UI #####

dreifing_ui <- function(id) {
    tabPanel("Dreifing",
             
             sidebarLayout(
                 sidebarPanel(
                     width = 3,
                     selectInput(
                         inputId = NS(id, "vidmid"),
                         label = "Sveitarfélag til viðmiðunar",
                         choices = unique(dreifing_d$sveitarfelag),
                         selected = c("Reykjavíkurborg"),
                         multiple = FALSE,
                         selectize = FALSE
                     ),
                     selectInput(
                         inputId = NS(id, "hluti"),
                         label = "Hluti",
                         choices = c("A-hluti", "A og B-hluti"),
                         selected = c("A-hluti")
                     ),
                     numericInput(
                         inputId = NS(id, "ar"),
                         label = "Ár",
                         value = 2022,
                         min = max(dreifing_d$ar), 
                         max = max(dreifing_d$ar),
                         step = 1
                     ),
                     selectInput(
                         inputId = NS(id, "y_var"),
                         label = "Myndrit",
                         choices = unique(dreifing_d$name),
                         selected = c("Nettóskuldir sem hlutfall af tekjum")
                     ),
                     div(
                         actionButton(
                             inputId = NS(id, "goButton"),
                             label = "Sækja gögn",
                             width = "120px"
                         ),
                         class = "center", align = "middle"
                     ),
                     HTML(sidebar_info)
                     
                 ),
                 
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Myndrit", plotlyOutput(NS(id, "dreifing_plot"), height = 1200, width = "100%") |> withSpinner()),
                         tabPanel("Tafla", DTOutput(NS(id, "dreifing_tafla")))
                     )
                 )
             )
             
    )
}


##### SERVER #####


dreifing_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        dreifing_df <- reactive({
            
            
            make_dreifing_df(input = input)
            
        }) 
        
        dreifing_plot <- reactive({
            
            dreifing_df() |> 
                make_dreifing_ggplot(input = input)
            
            
        })
        
        output$dreifing_plot <- renderPlotly({
            dreifing_plot() |> 
                make_dreifing_ggplotly(input)
            
        }) |> 
            bindCache(
                input$y_var, 
                input$hluti, 
                input$vidmid,
                input$ar
                ) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE) 
        
        outputOptions(output, "dreifing_plot", suspendWhenHidden = FALSE)
        
        dreifing_tafla <- reactive({
            
            dreifing_df() |> 
                make_dreifing_table(input = input)
            
        })
        
        
        
        output$dreifing_tafla <- renderDT({
            
            dreifing_tafla()
            
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        outputOptions(output, "dreifing_tafla", suspendWhenHidden = FALSE)
    })
}