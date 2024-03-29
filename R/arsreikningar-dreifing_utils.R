make_dreifing_df <- function(input) {
    
    dreifing_d |> 
        filter(
            ar == input$ar,
            hluti == input$hluti,
            name == input$y_var
        ) |> 
        select(sveitarfelag, ar, y, is_percent) |> 
        drop_na(y)  
}


make_dreifing_ggplot <- function(dreifing_df, input) {
    
    plot_dat <- dreifing_df |> 
        mutate(my_colour = 1 * (sveitarfelag %in% input$vidmid) + 2 * (sveitarfelag == "Heild"),
               text = text_tooltip_throun(sveitarfelag, y, ar, input$y_var, is_percent),
               sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, "</b>"),
                                        sveitarfelag == "Heild" ~ str_c("<b style='color:#b2182b'>", sveitarfelag, "</b>"),
                                        TRUE ~ str_c(sveitarfelag)),
               sveitarfelag = fct_reorder(sveitarfelag, y))
    
    x_scale <- make_x_scale(input$y_var)
    subtitles <- make_subtitles(input$y_var)
    vline_and_segments <- make_vline_and_segments(input$y_var)
    coords <- make_coords_dreifing(input$y_var, plot_dat$y)
    
    plot_dat |> 
        ggplot(aes(y, sveitarfelag, text = text)) +
        vline_and_segments + 
        geom_point(aes(col = factor(my_colour), size = factor(my_colour))) +
        x_scale +
        scale_colour_manual(values = c("#525252", "#2171b5", "#b2182b")) +
        scale_size_manual(values = c(2, 4, 4)) +
        coords +
        theme(legend.position = "none",
              axis.text.y = element_markdown(),
              plot.title = element_text(size = 14)) +
        labs(x = NULL,
             y = NULL,
             col = NULL,
             title = str_c(input$y_var, " (",input$ar, ", ", input$hluti, ")"),
             subtitle = subtitles,
             caption = caption) +
        coord_cartesian(clip = "off")
}


make_dreifing_ggplotly <- function(dreifing_ggplot, input) {
    ggplotly(
        dreifing_ggplot,
        tooltip = "text"
    ) |> 
        layout(
            hoverlabel  = list(align = "left"),
            margin = list(
                t = 105,
                r = 0,
                b = 0,
                l = 0
            )
        ) |> 
        config(displayModeBar = FALSE)
}

make_dreifing_table <- function(dreifing_df, input) {
    
    
    table_dat <- dreifing_df |> 
        select(sveitarfelag, ar, y, is_percent) |> 
        arrange(desc(y)) |> 
        mutate(y = round(y, digits = get_digits_yvar(input$y_var)),
               y = format_number(input$y_var, y, is_percent),
               nr = str_c(str_pad(row_number(), width = 2, side = "left", pad = "0"), "/", n())) |> 
        select(nr, sveitarfelag, ar, y) |> 
        rename(Röðun = nr, Sveitarfélag = sveitarfelag, "Síðasti ársreikningur" = ar, !!input$y_var := y)
    
    caption <- str_c(input$y_var)
    
    datatable(
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
    ) |> 
        formatStyle(
            target = 'row', columns = 'Sveitarfélag',  
            backgroundColor = styleEqual(input$vidmid, c("#2171b5")),
            color = styleEqual(input$vidmid, "#ffffff")
        ) |> 
        formatStyle(
            target = 'row', columns = 'Sveitarfélag',  
            backgroundColor = styleEqual("Heild", c("#b2182b")),
            color = styleEqual("Heild", "#ffffff")
        )
}