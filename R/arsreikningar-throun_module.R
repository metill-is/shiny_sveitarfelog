##### UI #####

throun_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "sveitarfelag"),
                label = "Sveitarfélag",
                choices = unique(throun_d$sveitarfelag),
                selected = c("Reykjavíkurborg", "Kópavogsbær", "Hafnarfjarðarkaupstaður",
                             "Garðabær", "Mosfellsbær", "Seltjarnarnesbær"),
                multiple = TRUE,
                selectize = TRUE
            ),
            selectInput(
                inputId = NS(id, "hluti"),
                label = "Hluti",
                choices = c("A-hluti", "A og B-hluti"),
                selected = c("A-hluti")
            ),
            selectInput(
                inputId = NS(id, "y_var"),
                label = "Myndrit",
                choices = c(
                    unique(throun_d$name)
                ),
                selected = c("Nettóskuldir sem hlutfall af tekjum")
            ),
            selectInput(
                inputId = NS(id, "ar_fra"), 
                label = "Frá hvaða ári viltu sjá þróunina?", 
                choices = c(2002, 2006, 2010, 2014, 2018), 
                selected = 2002, 
                multiple = FALSE, 
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "verdlag"),
                label = "Verðlag",
                choices = c("Verðlag hvers árs", "Fast verðlag"),
                selected = "Fast verðlag"
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
                tabPanel(
                    "Myndrit", 
                    plotlyOutput(NS(id, "throun_plot"), height = 700, width = "100%") |> withSpinner()
                ),
                tabPanel(
                    "Tafla",
                    DTOutput(NS(id, "throun_tafla")))
            )
            
        )
    )
}



##### SERVER #####


throun_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        
        ##### Make the dataframe used in the plot and table ######
        
        throun_df <- reactive({
            
            make_throun_df(input = input)
            
        }) 
        
        ##### Plot #####
        throun_plot <- reactive({
            
            throun_df() |> 
                make_throun_ggplot(input = input)
        }) 
        
        output$throun_plot <- renderPlotly({
            
            throun_plot() |> 
                make_throun_plotly(input = input)
        }) |> 
            bindCache(input$sveitarfelag, 
                      input$ar_fra, 
                      input$hluti,
                      input$y_var,
                      input$verdlag,
                      input$visitala) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        outputOptions(output, "throun_plot", suspendWhenHidden = FALSE)
        
        
        ##### Table #####
        throun_tafla <- eventReactive(input$goButton, {
            
            throun_df() |> 
                make_throun_table(input = input)
            
        })
        
        output$throun_tafla <- renderDT({
            
            throun_tafla()
            
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        outputOptions(output, "throun_tafla", suspendWhenHidden = FALSE)
    })
}