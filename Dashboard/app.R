# app.R — Shiny dashboard con mapa, correlaciones y tabla

pkgs <- c("shiny","leaflet","dplyr","tidyr","stringr","DT","ggplot2","classInt","sf")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))


# Si main_code NO calculó las correlaciones, las calculamos aquí:
if (!exists("cor_global_p") || !exists("cor_global_s") || !exists("cor_between") || !exists("cor_within")) {
  cor_global_p  <- cor(panel$gini, panel$nonnet_norm, use = "pairwise.complete.obs", method = "pearson")
  cor_global_s  <- cor(panel$gini, panel$nonnet_norm, use = "pairwise.complete.obs", method = "spearman")
  panel_c <- panel |>
    dplyr::group_by(country_std) |>
    dplyr::mutate(gini_c = gini - mean(gini, na.rm=TRUE),
                  nonnet_c = nonnet_norm - mean(nonnet_norm, na.rm=TRUE)) |>
    dplyr::ungroup()
  cor_within <- cor(panel_c$gini_c, panel_c$nonnet_c, use = "pairwise.complete.obs")
  country_means <- panel |>
    dplyr::group_by(country_std) |>
    dplyr::summarise(gini_mean = mean(gini, na.rm=TRUE),
                     nonnet_mean = mean(nonnet_norm, na.rm=TRUE), .groups="drop")
  cor_between <- cor(country_means$gini_mean, country_means$nonnet_mean, use = "pairwise.complete.obs")
}

# Serie temporal de correlaciones por año
per_year_corr <- panel |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    pearson  = cor(gini, nonnet_norm, use = "pairwise.complete.obs", method = "pearson"),
    spearman = cor(gini, nonnet_norm, use = "pairwise.complete.obs", method = "spearman"),
    .groups = "drop"
  )

# UI
ui <- navbarPage(
  title = "Desigualdad y Acceso a Internet",
  tabPanel(
    "Mapa",
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Variable a mostrar",
                    choices = c("Coeficiente Gini" = "gini",
                                "No uso normalizado" = "nonnet",
                                "Ratio gini/nonnet" = "ratio"),
                    selected = "ratio"),
        sliderInput("year", "Año",
                    min = min(panel$year, na.rm = TRUE),
                    max = max(panel$year, na.rm = TRUE),
                    value = min(panel$year, na.rm = TRUE),
                    step = 1, sep = "",
                    animate = animationOptions(interval = 1000, loop = TRUE))
      ),
      mainPanel(
        leafletOutput("map", height = 650),
        tags$br(),
        p("Usa el selector para alternar variable y el slider para recorrer todos los años.")
      )
    )
  ),
  tabPanel(
    "Correlaciones",
    fluidPage(
      fluidRow(
        column(
          width = 5,
          DTOutput("cor_summary_table")
        ),
        column(
          width = 7,
          plotOutput("cor_time_plot", height = 360)
        )
      ),
      tags$hr(),
      p("Definiciones:"),
      tags$ul(
        tags$li("Global: todas las observaciones país-año."),
        tags$li("Between: correlación de promedios por país."),
        tags$li("Within: correlación tras centrar por país (demeaning), captura variación temporal intra-país.")
      )
    )
  ),
  tabPanel(
    "Tabla de datos",
    fluidPage(
      DTOutput("tbl_panel")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Mapa reactivo
  output$map <- renderLeaflet({
    # Reutilizamos la función map_var_year(panel, world, sel_year, var)
    map_var_year(panel, world, sel_year = input$year, var = input$var)
  })
  
  # Resumen de correlaciones
  output$cor_summary_table <- renderDT({
    cor_vals <- tibble::tibble(
      Tipo = c("Global (Pearson)", "Global (Spearman)", "Between países", "Within país"),
      Correlación = c(cor_global_p, cor_global_s, cor_between, cor_within)
    )
    datatable(cor_vals, rownames = FALSE,
              options = list(dom = "t", paging = FALSE),
              caption = "Resumen de correlaciones: Gini vs No uso normalizado")
  })
  
  # Serie de correlaciones por año
  output$cor_time_plot <- renderPlot({
    df <- tidyr::pivot_longer(per_year_corr, cols = c(pearson, spearman),
                              names_to = "tipo", values_to = "corr")
    ggplot(df, aes(x = year, y = corr, linetype = tipo)) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      geom_line(linewidth = 0.9) +
      scale_linetype_manual(values = c("solid","dashed"), labels = c("Pearson","Spearman")) +
      labs(title = "Correlaciones por año: Gini vs No uso normalizado",
           x = "Año", y = "Coeficiente de correlación", linetype = NULL) +
      theme_minimal()
  })
  
  # Tabla de datos completa
  output$tbl_panel <- renderDT({
    datatable(
      panel |>
        dplyr::select(country_std, iso3c, year, gini, nonnet_norm, ratio) |>
        dplyr::arrange(country_std, year),
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
