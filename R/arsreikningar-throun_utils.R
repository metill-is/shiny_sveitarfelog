make_throun_df <- function(input) {
    
    
    throun_d |> 
        filter(
            hluti %in% input$hluti, 
            sveitarfelag %in% input$sveitarfelag,
            ar >= input$ar_fra,
            name == input$y_var
        ) |> 
        mutate(
            verdlag = input$verdlag,
            visitala = input$visitala,
            y = if_else(
                verdlag & !is_percent,
                vnv_convert(y, ar, date_unity = 2023),
                y
            )
        ) |> 
        mutate(
            y = if_else(
                visitala,
                y / y[ar == min(ar)] - 1,
                y
            ),
            .by = sveitarfelag
        ) |> 
        select(ar, sveitarfelag, y, is_percent) |> 
        mutate(
            x = ar,
            visitala = input$visitala,
            is_percent = if_else(visitala, TRUE, is_percent),
            tooltip = text_tooltip_throun(sveitarfelag, y, ar, y_var, is_percent),
            sveitarfelag = str_c(sveitarfelag, "")
        )
    
}


make_throun_ggplot <- function(throun_df, input) {
    
    
    y_scale <- if (input$visitala) make_y_scale("Vísitala") else make_y_scale(input$y_var)
    hlines <- if (input$visitala) make_hlines("Vísitala") else make_hlines(input$y_var)
    coords <- make_coords(input$y_var, throun_df$y)
    title <- paste0(input$y_var,  " (", input$hluti, ")")
    if (input$visitala) {
        title <- paste0(title," [% breyting frá ", input$ar_fra, "]")
    }
    
    throun_df |> 
        ggplot(aes(ar, y, group = sveitarfelag, col = sveitarfelag, text = tooltip)) +
        hlines +
        geom_line() +
        geom_point() +
        scale_x_continuous() +
        y_scale +
        scale_colour_brewer(type = "qual", palette = "Set1") +
        coords +
        theme(legend.position = "right",
              legend.box.margin = margin(),
              plot.margin = margin()) +
        labs(x = NULL,
             y = NULL,
             col = NULL,
             title = title,
             caption = caption)
}


make_throun_table <- function(throun_df, input) {
    
    is_percent <- unique(throun_df$is_percent)
    verdlag <- if (input$verdlag) " (fast verðlag)" else " (verðlag hvers árs)"
    visitala <- if (input$visitala) str_c(" [% breyting frá ", input$ar_fra, "]") else NULL
    table_dat <- throun_df |> 
        select(-x) |> 
        select(Ár = ar, sveitarfelag, y) |> 
        mutate(
            visitala = input$visitala,
            y_var = input$y_var,
            y_var = if_else(visitala, "Vísitala", y_var),
            y = round(y, digits = get_digits_yvar(unique(y_var)))
        ) |> 
        select(-visitala, -y_var) |> 
        pivot_wider(names_from = sveitarfelag, values_from = y)
    
    caption <- str_c(input$y_var, verdlag, visitala)
    
    out <- datatable(
        table_dat,
        extensions = "Buttons",
        rownames = FALSE,
        caption = htmltools::tags$caption(
            style = "caption-side: top",
            h4(caption)
        ),
        options = list(
            dom = "fBrtip",
            buttons = c("csv", "excel", "pdf"),
            pageLength = 68,
            lengthChange = FALSE,
            searching = TRUE,
            autoWidth = TRUE,
            captionSide = "top",
            language = list(
                decimal = ",",
                thousands = ".",
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
            )
        )
    )
    
    if (isTRUE(is_percent)) {
        n_svf <- ncol(table_dat) - 1
        out <- out |> 
            formatPercentage(
                columns = 2:(n_svf + 1)
            )
    }
    
    out
}


make_throun_plotly <- function(throun_plot, input) {
    
    
    ggplotly(
        throun_plot,
        tooltip = "text"
    ) |> 
        layout(
            hoverlabel = list(align = "left"),
            margin = list(
                t = 105,
                r = 0,
                b = 100,
                l = 0
            ),
            legend = list(
                orientation = "h"
            ),
            dragmode = FALSE
        ) |> 
        config(displayModeBar = FALSE)
}