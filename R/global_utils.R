
text_tooltip_throun <- function(sveitarfelag, y, ar, y_name, is_percent) {
    paste0(
        sveitarfelag," (", ar,"): ", format_number(y_name, y, is_percent)
    )
}

text_tooltip_vidmid <- function(sveitarfelag, y_var, ar, obs, vidmid) {
    ifelse(
        y_var == "nettoskuldir",
        paste0(
            sveitarfelag," (", ar,"): ", percent(obs, accuracy = 0.1)
        ),
        paste0(
            sveitarfelag," (", ar,"): ", percent(obs, accuracy = 0.1), " (Viðmið: ", percent(vidmid, accuracy = 0.1), ")"
        )
    )
}

format_number <- function(y_name, y, is_percent) {
    ifelse(
        is_percent,
        percent(y, big.mark = ".", decimal.mark = ",", accuracy = 0.1),
        number(y, big.mark = ".", decimal.mark = ",", accuracy = 1)
    )
}

make_x_scale <- function(y_name) {
    x_scales <- list(
        "Eiginfjárhlutfall" = scale_x_continuous(labels = label_percent()),
        "Framlegð á íbúa (kjörtímabil í heild)" = scale_x_continuous(labels = label_number(suffix = " kr")),
        "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
        "Handbært fé á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr"), 
                                                  breaks = c(0, 5e5, 1e6, 2e6, 3e6)),
        "Jöfnunarsjóðsframlög á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr"), limits = c(0, NA), expand = expansion(mult = 0.01)),
        "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = scale_x_continuous(labels = label_percent(), limits = c(0, NA), expand = expansion(mult = 0.01)),
        "Launa- og launatengd gjöld á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr"), limits = c(0, NA), expand = expansion(mult = 0.01)),
        "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = scale_x_continuous(labels = label_percent(), limits = c(0, NA), expand = expansion(mult = 0.01)),
        "Nettó jöfnunarsjóðsframlög á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr")),
        "Nettóskuldir sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent(), limits = c(NA, NA), expand = expansion(mult = 0.01)),
        "Rekstrarniðurstaða á íbúa (kjörtímabil í heild)" = scale_x_continuous(labels = label_number(suffix = " kr")),
        "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
        "Skuldir á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr")),
        "Skuldir sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
        "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = scale_x_continuous(labels = label_percent()),
        "Skuldahlutfall" = scale_x_continuous(labels = label_percent()),
        "Útsvar og fasteignaskattur á íbúa" = scale_x_continuous(labels = label_number(suffix = " kr")),
        "Veltufé frá rekstri sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
        "Veltufjárhlutfall" = scale_x_continuous(labels = label_percent())
    )
    
    x_scales[[y_name]]
}


make_y_scale <- function(y_name) {
    y_scales <- list(
        "Árafjöldi til niðurgreiðslu nettó skulda" = scale_y_continuous(
            labels = label_number(suffix = " ár"), 
            limits = c(0, NA), 
            expand = expansion()
        ),
        "Eiginfjárhlutfall" = scale_y_continuous(
            labels = label_percent(), 
            breaks = seq(0, 1, by = 0.25), 
            expand = expansion()
        ),
        "Framlegð sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(), 
            expand = expansion()
        ),
        "Handbært fé á íbúa" = scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            expand = expansion(mult = 0.005),
            limits = c(NA, NA)
        ),
        "Jöfnunarsjóðsframlög á íbúa" = scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            limits = c(0, NA), 
            expand = expansion()
        ),
        "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = scale_y_continuous(
            labels = label_percent(), 
            limits = c(0, NA), 
            expand = expansion()
        ),
        "Launa- og launatengd gjöld á íbúa" = scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            limits = c(0, NA)
        ),
        "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = scale_y_continuous(
            labels = label_percent(accuracy = 0.1), 
            limits = c(NA, NA)
        ),
        "Nettóskuldir sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(accuracy = 0.1), 
            limits = c(NA, NA))
        ,
        "Nettó jöfnunarsjóðsframlög á íbúa" = scale_y_continuous(
            labels = label_number(suffix = " kr")
        ),
        "Rekstrarniðurstaða sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(accuracy = 0.1), 
            expand = expansion()
        ),
        "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(accuracy = 0.1), 
            expand = expansion()
        ),
        "Skuldir" = scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            expand = expansion(mult = 0.01)
        ),
        "Skuldir á íbúa" = scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            expand = expansion()
        ),
        "Skuldir sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(), 
            expand = expansion()
        ),
        "Skuldaaukning" = scale_y_continuous(
            labels = label_percent(), 
            expand = expansion()
        ),
        "Skuldahlutfall" = scale_y_continuous(
            labels = label_percent(), 
            breaks = seq(0, 1, by = 0.25), 
            expand = expansion()
        ),
        "Útsvar og fasteignaskattur á íbúa" =  scale_y_continuous(
            labels = label_number(suffix = " kr"), 
            limits = c(NA, NA), 
            expand = expansion(mult = 0.01)
        ),
        "Veltufé frá rekstri sem hlutfall af tekjum" = scale_y_continuous(
            labels = label_percent(accuracy = 0.1), 
            breaks = pretty_breaks(6),
            limits = c(NA, NA), 
            expand = expansion()
        ),
        "Veltufjárhlutfall" = scale_y_continuous(
            labels = label_percent(), 
            expand = expansion()
        )
    )
    
    y_scales[y_name]
}

make_subtitles <- function(y_name) {
    
    
    subtitles <- list(
        "Árafjöldi til niðurgreiðslu nettó skulda" = "Nettóskuldir deilt með veltufé frá rekstri. Þessi tala getur sveiflast mikið eftir því hversu mikið veltufé er á hverju ári.",
        "Eiginfjárhlutfall" = "Eiginfjárhlutfall (100% - skuldahlutfall) sýnir hlutfall eigna sem er fjármagnað með hagnaði og hlutafé (restin eru skuldasöfnun).\nHér þýðir 100% að skuldir séu engar og 0% að eigin eignir eru engar.",
        "Framlegð sem hlutfall af tekjum" = "Framlegð er reglulegar tekjur mínus gjöld að frádregnum rekstrargjöldum",
        "Handbært fé á íbúa" = "Handbært fé er það fé sem sveitarfélög eiga eftir þegar búið er að greiða skuldir og skuldbindingar.",
        "Jöfnunarsjóðsframlög á íbúa" = "Peningamagn sem sveitarfélag fær frá jöfnunarsjóð deilt með íbúafjölda.",
        "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = "Peningamagn sem sveitarfélag fær frá jöfnunarsjóð deilt með heildartekjum frá útsvari og fasteignasköttum.",
        "Nettó jöfnunarsjóðsframlög á íbúa" = "Framlög frá jöfnunarsjóði að frádregnum útgjöldum til jöfnunarsjóðs deilt með íbúafjölda sveitarfélags",
        "Nettóskuldir sem hlutfall af tekjum" = "Nettóskuldir eru heildarskuldir að frádregnum peningalegum eignum án eigin fyrirtækja og eru notaðar í viðmiðum eftirlitsnefndar með fjármálum sveitarfélaga.",
        "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = "Skuldir eru leiðréttar fyrir vísitölu neysluverðs áður en aukningin er reiknuð",
        "Skuldir" = "Samanburður hér er óæskilegur því stærð sveitarfélaga hefur áhrif á heildarskuldir",
        "Skuldahlutfall" = "Skuldahlutfall sýnir hve stór hluti heildareigna er fjármagnaður með lánum.\nHér þýðir 0% að skuldir séu engar og 100% að eigin eignir eru engar.\nOft er sama orðið notað fyrir skuldir sveitarfélaga sem hlutfall af tekjum, en það á ekki við hér.",
        "Veltufjárhlutfall" = "Veltufjárhlutfall er hlutfall skammtímaskulda deilt upp í eignir sem er hægt að nota í að borga í skammtímaskuldir"
    )
    
    subtitles[[y_name]]
}

make_hlines <- function(y_name) {
    hlines <- list(
        "Árafjöldi til niðurgreiðslu nettó skulda" = 20,
        "Eiginfjárhlutfall" = c(0, 1),
        "Framlegð sem hlutfall af tekjum" = 0,
        "Nettó jöfnunarsjóðsframlög á íbúa" = 0,
        "Nettóskuldir sem hlutfall af tekjum" = 1,
        "Rekstrarniðurstaða sem hlutfall af tekjum" = 0,
        "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = 0,
        "Skuldir" = 0,
        "Skuldir á íbúa" = NULL,
        "Skuldir sem hlutfall af tekjum" = 1,
        "Skuldaaukning" = 0,
        "Skuldahlutfall" = c(0, 1),
        "Veltufé frá rekstri sem hlutfall af tekjum" = 0,
        "Veltufjárhlutfall" = 1
    )
    
    if(is.null(hlines[[y_name]])) {
        return(NULL)
    } else {
        geom_hline(yintercept = hlines[[y_name]], lty = 2, alpha = 0.5)
    }
}

make_vline_and_segments <- function(y_name) {
    vlines <- list(
        "Eiginfjárhlutfall" = 0,
        "Skuldir á íbúa" = 0,
        "Framlegð á íbúa (kjörtímabil í heild)" = 0,
        "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = 0,
        "Handbært fé á íbúa" = 0,
        "Jöfnunarsjóðsframlög á íbúa" = 0,
        "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 0,
        "Launa- og launatengd gjöld á íbúa" = 0,
        "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = 0,
        "Nettó jöfnunarsjóðsframlög á íbúa" = 0,
        "Nettóskuldir sem hlutfall af tekjum" = 0,
        "Rekstrarniðurstaða á íbúa (kjörtímabil í heild)" = 0,
        "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = 0,
        "Skuldir sem hlutfall af tekjum" = 1,
        "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = 0,
        "Skuldahlutfall" = 0,
        "Útsvar og fasteignaskattur á íbúa" = 0,
        "Veltufé frá rekstri sem hlutfall af tekjum" = 0,
        "Veltufjárhlutfall" = 1
    )
    
    if (is.null(vlines[[y_name]])) {
        return(NULL)
    } else {
        return(
            list(
                geom_vline(xintercept = vlines[[y_name]], lty = 2, alpha = 0.5),
                geom_segment(aes(xend = vlines[[y_name]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3)
            )
        )
    }
}


make_coords <- function(y_name, y) {
    coords <- list(
        "Árafjöldi til niðurgreiðslu nettó skulda" = coord_cartesian(ylim = c(0, 40)),
        "Eiginfjárhlutfall" = coord_cartesian(ylim = c(pmin(0, min(y)), 
                                                       pmax(1, max(y)))),
        "Skuldahlutfall" = coord_cartesian(ylim = c(pmin(0, min(y)), 
                                                    pmax(1, max(y)))),
        "Veltufjárhlutfall" = coord_cartesian(ylim = c(0, 3))
    )
    
    coords[[y_name]]
}

make_coords_dreifing <- function(y_name, y) {
    coords <- list(
        "Árafjöldi til niðurgreiðslu nettó skulda" = coord_cartesian(xlim = c(0, 40)),
        "Eiginfjárhlutfall" = coord_cartesian(xlim = c(pmin(0, min(y)), 
                                                       pmax(1, max(y)))),
        "Skuldahlutfall" = coord_cartesian(xlim = c(pmin(0, min(y)), 
                                                    pmax(1, max(y)))),
        "Veltufjárhlutfall" = coord_cartesian(xlim = c(0, 3))
    )
    
    coords[[y_name]]
}




get_digits_yvar <- function(y_name) {
    my_digits <- list(
        "Eiginfjárhlutfall" = 3,
        "Framlegð sem hlutfall af tekjum" = 3,
        "Handbært fé á íbúa" = 0,
        "Jöfnunarsjóðsframlög á íbúa" = 0,
        "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 3,
        "Launa- og launatengd gjöld á íbúa" = 0,
        "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = 3,
        "Nettó jöfnunarsjóðsframlög á íbúa" = 0,
        "Nettóskuldir sem hlutfall af tekjum" = 3,
        "Rekstrarniðurstaða sem hlutfall af tekjum" = 3,
        "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = 3,
        "Skuldir á íbúa"  = 0,
        "Skuldir sem hlutfall af tekjum" = 3,
        "Skuldaaukning" = 3,
        "Skuldahlutfall" = 3,
        "Útsvar og fasteignaskattur á íbúa" = 0,
        "Veltufé frá rekstri sem hlutfall af tekjum" = 3,
        "Veltufjárhlutfall" = 3
    )
    
    if (is.null(my_digits[[y_name]])) my_digits[[y_name]] <- 0
    
    my_digits[[y_name]]
}

