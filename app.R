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

# load data ----
app_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(app_dir)

# Load Norwaygian dummy life tables ----
load_data = function(file_path) {
  read.csv(paste0("./data/", file_path))
}

countries = "n"

for (country in countries) {
  assign(paste0(country, "_ref_df"), load_data(paste0(country, "_ref_df_appended.csv")))
  assign(paste0(country, "_mvh_df"), load_data(paste0(country, "_mvh_df.csv")))
}

# load functions ----
source("./scr/compQale.R")
source("./scr/loadFnc.R")

ui <- page_navbar(
  theme = bs_theme(preset = "shiny", "primary" = "#0675DD"),
  lang = "en",
  title = "Nordic Shortfall Calculator Demo",
  sidebar = sidebar(HTML('<p style="font-weight:bold;">Input for</p>'), width = 300, sidebar_acc),
  nav_spacer(),
  generate_nav_panel("Norway", "n")
)

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