# function for consistent 2 decimal digits ----
fRound = function(str, digits = 2, width = 2){
  formatC(str, digits = 2, width = 2, format = "f")
}

# function for sidebar ----
create_accordion_panel = function(country, prefix, selected = FALSE) {
  accordion_panel(
    country,
    sliderInput(
      paste0(prefix, "_start_age"),
      "Age of the patient population",
      min = 0,
      max = 100,
      value = 0,
      step = 1
    ),
    sliderInput(
      paste0(prefix, "_sex_mix"),
      "% female in the patient population",
      min = 0,
      max = 100,
      value = 50,
      step = 1
    ),
    selectInput(
      paste0(prefix, "_utils"),
      "Select scenario",
      choices = list(
        "Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = paste0(prefix, "_dsu_2014"),
        "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = paste0(prefix, "_dsu"),
        "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = paste0(prefix, "_vanHout"),
        "Alternative C: MVH value set + health state profiles" = paste0(prefix, "_mvh"),
        "Alternative D: MVH value set + HSE 2012+14" = paste0(prefix, "_tto")
      ),
      selected = paste0(prefix, "_dsu_2014")
    ),
    sliderInput(
      paste0(prefix, "_remaining_qalys"),
      "Remaining QALYS",
      min = 0,
      max = 49,
      value = 10,
      step = 1
    ),
    sliderInput(
      paste0(prefix, "_disc_rate"),
      "Discount rate",
      min = 0,
      max = 10,
      value = 1.5,
      step = 0.5
    ),
    selected = selected  # Set selected argument
  )
}

sidebar_acc = accordion(open = F,
                        create_accordion_panel("Norway", "n")
)

# function to create CS, HRQoL & CQ highchart ----
create_highchart <- function(plot_df, title, ytitle, y_max, color = "#7cb5ec") {
  highchart(
    hc_opts = list(),
    theme = getOption("highcharter.theme"),
    type = "chart",
    width = NULL,
    height = NULL,
    elementId = NULL,
    google_fonts = getOption("highcharter.google_fonts")
  ) %>%
    hc_add_series(
      plot_df, type = "area",
      name = "Shortfall", color = color,
      hcaes(x = "age", y = "var"),
      tooltip = list(enabled = FALSE),
      fast = TRUE
    ) %>%
    hc_title(
      text = title,
      y = 60, x = -50,
      style = list(fontSize = "16px")
    ) %>%
    hc_plotOptions(
      line = list(
        marker = list(
          enabled = FALSE
        )
      ),
      series = list(
        tooltip = list(
          enabled = TRUE,
          followPointer = TRUE,
          fillColor = "transparent"
        )
      ),
      area = list(
        states = list(
          hover = list(
            enabled = TRUE
          )
        ),
        marker = list(
          enabled = FALSE,
          fillColor = "blue",
          width = 1,
          height = 1,
          enabledThreshold = 10,
          radius = 1
        )
      )
    ) %>%
    hc_xAxis(
      title = list(text = "Age"),
      gridLineColor = 'lightgray',
      gridLineWidth = 1,
      gridLineDashStyle = "Dot",
      tickLength = 10,
      tickWidth = 2,
      tickmarkPlacement = 'between'
    ) %>%
    hc_yAxis(
      title = list(text = ytitle),
      max = y_max
    ) %>%
    hc_tooltip(
      enabled = TRUE,
      valueDecimals = 2,
      pointFormat = '{point.y} ',
      valueSuffix = ' '
    ) %>%
    hc_chart(
      style = list(
        fontFamily = "Inter"
      )
    ) %>%
    hc_legend(
      enabled = FALSE
    )
}

# funciton for AS highchart ----
create_highchart_as <- function(data, remaining_qalys, color) {
  if (data$shortfall_abs < 0) {
    p_error <- highchart() %>%
      hc_title(
        text = "Error: QALYs must be lower with the disease.",
        align = "center",
        x = -10,
        verticalAlign = 'middle',
        floating = TRUE,
        style = list(
          fontSize = "16px",
          color = color
        )
      )
    return(p_error)
  } else {
    short_fall <- data.frame(
      name = c("QALYs with disease", "Absolute shortfall", "QALYs without disease"),
      value = c(remaining_qalys, data$shortfall_abs, max(data$res$Qx[1])),
      color = c(color, "#6d757d", "#3e6386"),
      a = c(FALSE, FALSE, TRUE)
    )
    
    shortfall_str <- paste0("Absolute QALY shortfall:<b>", round(data$shortfall_abs, 2), "</b>")
    
    p1 <- highchart() %>%
      hc_add_series(
        data = short_fall, "waterfall",
        pointPadding = "0",
        hcaes(
          name = name,
          y = value,
          isSum = a,
          color = color
        ),
        name = "QALYs"
      ) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 2
      ) %>%
      hc_xAxis(
        categories = short_fall$name
      ) %>%
      hc_boost(enabled = FALSE)
    
    return(p1)
  }
}

# function to generate nav_panel for each country ----
generate_nav_panel <- function(country, id_prefix) {
  nav_panel(
    country,
    h1(paste("Remaining QALYs for", country)),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:"), card_body(uiOutput(paste0(id_prefix, "_qales_healthy_txt")))),
      card(fill = FALSE, card_header("with the disease:"), card_body(uiOutput(paste0(id_prefix, "_qales_ill_txt")))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(uiOutput(paste0(id_prefix, "_abs_short_txt")))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(uiOutput(paste0(id_prefix, "_prop_short_txt")))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(uiOutput(paste0(id_prefix, "_mltplr_txt"))))
    ),
    layout_column_wrap(
      width = 1/2,
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Shortfall",
        nav_panel("Absolute", card_body(highchartOutput(paste0(id_prefix, "_hc_as")))),
        nav_panel("Proportional", card_body(highchartOutput(paste0(id_prefix, "_hc_ps"))))
      ),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Other",
        nav_panel("Cumulative QALYs", card_body(highchartOutput(paste0(id_prefix, "_hc_cq")))),
        nav_panel("HRQoL by year", card_body(highchartOutput(paste0(id_prefix, "_hc_hrqol")))),
        nav_panel("Cumulative Survival", card_body(highchartOutput(paste0(id_prefix, "_hc_cs"))))
      )
    )
  )
}