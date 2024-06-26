#
# This is a Shiny web application to provide a demo preview of Nordic Shortfall Calculator.
# You can run the application by clicking the 'Run App' button above.
#

library(shiny)
library(tibble)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)
library(highcharter)

# functions ----

# compute QALE
compQale = function(
    ons_df,                   
    prop_female = 0.5, 
    start_age = 50, 
    disc_rate = 0.035,
    utils = "cw"
){
  
  compQaleInternal = function(
    ons_df,                   
    prop_female = 0.5, 
    start_age = 50, 
    disc_rate = 0.035,
    utils = "cw"
  ){
    
    ons_df = ons_df[ons_df$age >= start_age,]
    ons_df = ons_df[order(ons_df$age),]
    df_female = ons_df[ons_df$sex == "female", c("age",utils,"lx","dx","mx","ex")]
    df_male = ons_df[ons_df$sex == "male",c("age",utils,"lx","dx","mx","ex")]
    
    df_comp = data.frame(
      age = df_female$age,
      utils = (1-prop_female) * df_male[,utils]  + prop_female * df_female[,utils],
      lx = (1-prop_female) * df_male$lx  + prop_female * df_female$lx,
      dx = (1-prop_female) * df_male$dx  + prop_female * df_female$dx,
      mx = (1-prop_female) * df_male$mx  + prop_female * df_female$mx,
      ex = (1-prop_female) * df_male$ex  + prop_female * df_female$ex
    )
    
    # person years in year i
    df_comp$Lx = NA
    for(i in 2:nrow(df_comp)){
      df_comp$Lx[i-1] = df_comp$lx[i] + (0.5 * df_comp$dx[i-1])
    }
    df_comp$Lx[nrow(df_comp)] = (df_comp$lx[nrow(df_comp)]-df_comp$dx[nrow(df_comp)]) + (0.5 * df_comp$dx[nrow(df_comp)])
    
    # person QALYs in year i
    df_comp$Yx = df_comp$utils * df_comp$Lx
    
    # apply discounting
    v_disc <- 1/(1+disc_rate)^(0:(length(df_comp$Yx)-1))
    df_comp$Yx = df_comp$Yx * v_disc
    
    # remaining person QALYs?
    df_comp$Nx = NA
    df_comp$Nx[nrow(df_comp)] = df_comp$Yx[nrow(df_comp)]
    for(i in nrow(df_comp):2){
      df_comp$Nx[i-1] = df_comp$Yx[i-1] + df_comp$Nx[i]
    }
    
    # Quality adjusted life expectancy 
    df_comp$Qx = df_comp$Nx / df_comp$lx
    
    
    q_factor = sum(df_comp$Yx) / df_comp$Qx[1]
    
    df_comp$qalys_by_year = df_comp$Yx/q_factor 
    df_comp$cumulative_qalys = cumsum(df_comp$qalys_by_year)
    
    # cumulative survival function
    df_comp$S = 1-df_comp$mx
    df_comp$S_cumulativ =  cumprod(df_comp$S)
    df_comp$hrqol = df_comp$utils
    
    df_comp = df_comp[,c("age","hrqol","ex","Qx","S_cumulativ","cumulative_qalys")]
    
    return(df_comp)
    
  }
  
  
  qale_male = compQaleInternal(
    ons_df = ons_df,                   
    prop_female = 0, 
    start_age = start_age, 
    disc_rate = disc_rate,
    utils = utils
  )
  qale_female = compQaleInternal(
    ons_df = ons_df,                   
    prop_female = 1, 
    start_age = start_age, 
    disc_rate = disc_rate,
    utils = utils
  )
  qale_mix = qale_male * (1-prop_female) + qale_female * prop_female
  
  return(qale_mix)
  
}

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

# load data ----
n_ref_df = as.tibble()
n_mvh_df = as.tibble()

# ui ----
ui <- page_navbar(
  theme = bs_theme(preset = "shiny", "primary" = "#0675DD"),
  lang = "en",
  title = "Nordic Shortfall Calculator Demo",
  sidebar = sidebar(HTML('<p style="font-weight:bold;">Input for</p>'), width = 300, sidebar_acc),
  nav_spacer(),
  generate_nav_panel("Norway", "n")
)

# server ----
server <- function(input, output, session) {
  n_dat = reactiveValues()
  
  observe({
    
    util_df = switch(input$n_utils,
                     "d_mvh" = n_mvh_df,
                     "d_vanHout" = n_ref_df,
                     "d_tto" = n_ref_df,
                     "d_dsu" = n_ref_df,
                     "d_dsu_2014" = n_ref_df,
                     n_ref_df
    )
    
    utils = switch(input$n_utils,
                   "d_mvh" = "tto",
                   "d_vanHout" = "cw",
                   "d_tto" = "tto",
                   "d_dsu" = "co",
                   "d_dsu_2014" = "dsu_2014",
                   "cw"
    )
    
    n_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$n_sex_mix/100,
      start_age = input$n_start_age,
      disc_rate = input$n_disc_rate/100,
      utils = utils
    )
    
    n_dat$shortfall_abs = n_dat$res$Qx[1] - input$n_remaining_qalys
    n_dat$shortfall_prop = n_dat$shortfall_abs / n_dat$res$Qx[1]
    n_dat$q_weight = ifelse(n_dat$shortfall_prop >= 0.95 | n_dat$shortfall_abs >= 18, 1.7,
                            ifelse(n_dat$shortfall_prop >= 0.85 | n_dat$shortfall_abs >= 12, 1.2, 1)
    )
  })
  
  # absolute shortfall highchart
  highchart_n_as <- reactive({
    create_highchart_as(n_dat, input$n_remaining_qalys, "#7cb5ec")
  })
  
  # proportional shortfall highchart ----
  highchart_n_ps = reactive({
    short_fall = data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - n_dat$shortfall_prop*100, n_dat$shortfall_prop*100),
      col = c("green","gray")
    )
    
    shortfall_str = paste0(round(n_dat$shortfall_prop*100,1))
    shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
    
    p1 = highchart() %>%
      hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
      hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 1,
        valueSuffix = '%'
      ) %>%
      hc_colors(c("#7cb5ec","gray"))
    
    return(p1)
  })
  
  # cumulative QALY highchart
  highchart_n_cq <- reactive({
    disc_str <- input$n_disc_rate > 0
    y_max <- max(n_dat$res$Qx[1])
    title <- paste0("QALYs without the disease: <b>", round(max(n_dat$res$Qx[1]), 2), "</b>", ifelse(disc_str, "(discounted)", ""))
    ytitle <- "Cumulative QALYs"
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 6]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # HRQoL highchart
  highchart_n_hrqol <- reactive({
    disc_str <- input$n_disc_rate > 0
    y_max <- max(n_dat$res$Qx[1])
    title <- paste0("HRQoL over the lifecourse", ifelse(disc_str, "(undiscounted)", ""))
    ytitle <- "EQ-5D score"
    y_max <- 1
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 2]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # cumulative survival highchart
  highchart_n_cs <- reactive({
    title <- paste0("Cumulative survival")
    ytitle <- "S(t)"
    y_max <- 1
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 5]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # numeric outputs ----
  output$n_qales_healthy_txt = renderText({fRound(n_dat$res$Qx[1],2)})
  output$n_qales_ill_txt = renderText({fRound(input$n_remaining_qalys)})
  output$n_abs_short_txt = renderText({fRound(n_dat$shortfall_abs,2)})
  output$n_prop_short_txt = renderText({paste0(fRound(n_dat$shortfall_prop*100,2),"%")})
  output$n_mltplr_txt = renderText({paste0("x ",n_dat$q_weight)})

  # highcharts ----
  output$n_hc_as = renderHighchart({highchart_n_as()})
  output$n_hc_ps = renderHighchart({highchart_n_ps()})
  output$n_hc_cq = renderHighchart({highchart_n_cq()})
  output$n_hc_hrqol = renderHighchart({highchart_n_hrqol()})
  output$n_hc_cs = renderHighchart({highchart_n_cs()})
  
}

# RUN APP ----
shinyApp(ui, server)