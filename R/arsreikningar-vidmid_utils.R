make_vidmid_ggplot <- function(input) {
    
    
    
    plot_dat <- vidmid_d |> 
        filter(
            sveitarfelag %in% input$sveitarfelag,
            hluti == input$hluti
        ) |> 
        mutate(
            text = text_tooltip_vidmid(sveitarfelag, name, ar, obs, vidmid),
            name = fct_recode(name,
                              "Framlegðarhlutfall" = "framlegd",
                              "Rekstrarniðurstaða\n(síðustu 3 ára)" = "rekstrarnidurstada",
                              "Veltufé frá rekstri\n(hlutfall af tekjum)" = "veltufe",
                              "Nettóskuldir\n(hlutfall af tekjum)" = "nettoskuldir",
                              "Veltufjárhlutfall" = "veltufjarhlutfall",
                              "Fjárfesting\n(hlutfall af skuldum)" = "fjarfhlutfskuldir") |> 
                fct_relevel("Nettóskuldir\n(hlutfall af tekjum)")
        ) 
    
    
    p <- plot_dat |> 
        filter(name != "Nettóskuldir\n(hlutfall af tekjum)") |> 
        ggplot() +
        geom_line(
            aes(ar, vidmid, group = paste(name, sveitarfelag)), 
            lty = 2
        ) +
        geom_point(
            aes(ar, obs, col = colour, text = text)
        ) +
        geom_point(
            data = plot_dat |> 
                filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
            aes(ar, obs, text = text), 
            inherit.aes = FALSE
        ) + 
        geom_line(
            data = plot_dat |> 
                filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
            aes(ar, obs), 
            inherit.aes = FALSE
        ) + 
        scale_x_continuous(breaks = c(2022, 2018, 2014)) +
        scale_y_continuous(
            labels = label_percent(),
            breaks = pretty_breaks(4)
        ) +
        scale_colour_brewer(
            type = "qual", 
            palette = "Set1"
        ) +
        facet_grid(name ~ sveitarfelag, scales = "free_y") +
        theme(
            legend.position = "none", 
            plot.title = element_text(size = 12),
            panel.spacing.y = unit(0.02, units = "npc"),
            strip.placement = "outside",
            strip.text = element_text(
                margin = margin(t = 5, r = 10, b = 5, l = 10), 
                hjust = 0.5,
                vjust = 0.5, 
                size = 8
            )
        ) +
        labs(
            x = NULL,
            y = NULL,
            title = str_c("Hvernig gengur sveitarfélögum að standast viðmið Eftirlitsnefndar með fjármálum sveitarfélaga (", input$hluti,")?"),
            subtitle = "Brotin lína er viðmið og er reiknuð út frá nettóskuldum sem hlutfall af tekjum",
            caption = caption
        )
    
    
    p
}


make_vidmid_ggplotly <- function(vidmid_ggplot, input) {
    
    ggplotly(
        vidmid_ggplot,
        tooltip = "text",
    ) |> 
        layout(
            margin = list(
                t = 105,
                r = 40,
                b = 0,
                l = 0
            ),
            annotations = list(
                list(x = 1, xanchor = "right", xref = "paper",
                     y = -0.05, yanchor = "bottom", yref = "paper",
                     showarrow = FALSE,
                     text = caption)
            )
        ) 
    
}