rm(list = ls())
gc(reset = T)

if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(plotly)) install.packages('plotly'); library(plotly)
if(!require(leaflet)) install.packages('leaflet'); library(leaflet)
if(!require(dashboardthemes)) install.packages("dashboardthemes"); library(dashboardthemes)
if(!require(highcharter)) install.packages('highcharter'); library(highcharter)
if(!require(DT)) install.packages('DT'); library(DT)
if(!require(RcppRoll)) install.packages("RcppRoll"); library(RcppRoll)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(leafpop)) install.packages("leafpop"); library(leafpop)
# if(!require()) install.packages(""); library()
install.packages("raster")
install.packages("sf")
install.packages("rgdal")
#install.packages("dplyr")
install.packages("rgeos")
install.packages("maptools")
install.packages("gpclib", type="source")

mapview
library(mapview)
library(raster)
library(sf)
library(rgdal)
library(rgeos)
library(maptools)
library(gpclib)


setwd("~/Desktop/lab/새론솔루션/shiny/data")

musu_kml = st_read("musu.kml")

musu_kml_n <- st_zm(musu_kml[1], drop=T, what='ZM')
as.data.frame(musu_kml_n) -> musu_kml.df
as(musu_kml_n, "Spatial") -> polygon.musu

# st_write(jkt_n, dsn= "jakarta", driver= "ESRI Shapefile",'jkt.shp')
# ams_ll <- spTransform(polygon.musu, CRS("+init=epsg:4326"))
heoijuk_kml = st_read("heoijuk.kml")

heoijuk_kml_n <- st_zm(heoijuk_kml[1], drop=T, what='ZM')
as.data.frame(heoijuk_kml_n) -> heoijuk_kml.df
as(heoijuk_kml_n , "Spatial") -> path.heoijuk

geumgok_kml = st_read("geumgok.kml")

geumgok_kml_n <- st_zm(geumgok_kml[1], drop=T, what='ZM')
as.data.frame(geumgok_kml_n) -> geumgok_kml.df
as(geumgok_kml_n, "Spatial") -> path.geumgok

geumgok_marker1 = st_read("geumgok1.kml")
geumgok_marker1_n <- st_zm(geumgok_marker1[1], drop=T, what='ZM')
as.data.frame(geumgok_marker1_n) -> geumgok_marker1_n.df
as(geumgok_marker1_n, "Spatial") -> marker.geumgok1

geumgok_marker2 = st_read("geumgok2.kml")
geumgok_marker2_n <- st_zm(geumgok_marker2[1], drop=T, what='ZM')
as.data.frame(geumgok_marker2_n) -> geumgok_marker2_n.df
as(geumgok_marker2_n, "Spatial") -> marker.geumgok2

geumgok_marker3 = st_read("geumgok3.kml")
geumgok_marker3_n <- st_zm(geumgok_marker3[1], drop=T, what='ZM')
as.data.frame(geumgok_marker3_n) -> geumgok_marker3_n.df
as(geumgok_marker3_n, "Spatial") -> marker.geumgok3

geumgok_marker_df = rbind(marker.geumgok1, marker.geumgok2, marker.geumgok3)
geumgok_marker_df$Name = c("금곡-상부", "금곡-중부", "금곡-하부")

heoijuk_marker1 = st_read("heoijuk1.kml")
heoijuk_marker1_n <- st_zm(heoijuk_marker1[1], drop=T, what='ZM')
as.data.frame(heoijuk_marker1_n) -> heoijuk_marker1_n.df
as(heoijuk_marker1_n, "Spatial") -> marker.heoijuk1

heoijuk_marker2 = st_read("heoijuk2.kml")
heoijuk_marker2_n <- st_zm(heoijuk_marker2[1], drop=T, what='ZM')
as.data.frame(heoijuk_marker2_n) -> heoijuk_marker2_n.df
as(heoijuk_marker2_n, "Spatial") -> marker.heoijuk2

heoijuk_marker3 = st_read("heoijuk3.kml")
heoijuk_marker3_n <- st_zm(heoijuk_marker3[1], drop=T, what='ZM')
as.data.frame(heoijuk_marker3_n) -> heoijuk_marker3_n.df
as(heoijuk_marker3_n, "Spatial") -> marker.heoijuk3

heoijuk_marker_df = rbind(marker.heoijuk1, marker.heoijuk2, marker.heoijuk3)

heoijuk_marker_df$Name = c("회죽-상부", "회죽-중부", "회죽-하부")

# setwd("C:/Users/chica/Desktop/rshiny/data")
# data = fread("吏꾩쿇_媛뺤닔?웾_1999_2020.csv") %>% as_tibble()

musu_data = readxl::read_xlsx('무수저수지 데이터셋.xlsx')

colnames(musu_data) = c('date', 'amount', 'rate', 'geumgok1', 'geumgok2', 'geumgok3', 
                        'heoijuk1', 'heoijuk2', 'heoijuk3', 'temp', 'mois', 'sun', 'repi',
                        'sky1', 'temp_high1', 'temp_low1', 'temp_avg1', 'repi_prop1',
                        'sky2', 'temp_high2', 'temp_low2', 'temp_avg2', 'repi_prop2')

musu_data = musu_data %>%
  mutate(y = format(date, '%Y'), md = format(date, '%m-%d'), month = format(date, '%m'), flow = geumgok1 + heoijuk1) %>%
  mutate(year_group = if_else(y == '2020', '2020', '과거 평년')) %>% 
  mutate(year_group = if_else(y == '2019', '2019', year_group)) %>% 
  plyr::rename(c("geumgok1" = "금곡-상부", 
                 "geumgok2" = "금곡-중부", 
                 "geumgok3" = "금곡-하부",
                 "heoijuk1" = "회죽-상부",
                 "heoijuk2" = "회죽-중부",
                 "heoijuk3" = "회죽-하부"
  ))

img_path = c("https://user-images.githubusercontent.com/37679460/134656517-18af3ccd-b9c7-4d39-a142-b2ed3e856c16.png",
  "https://user-images.githubusercontent.com/37679460/134656529-2369a5ed-527a-44ba-b144-c365af7c2c28.png",
  "https://user-images.githubusercontent.com/37679460/134656536-e3811812-1563-430e-84a9-af08aadb67a1.png",
  "https://user-images.githubusercontent.com/37679460/134656539-90f94786-958d-4db1-8daa-86d2ec1b09fa.png", 
  "https://user-images.githubusercontent.com/37679460/134656543-85072195-9e2f-4565-8cc3-7f8e1282d67a.png",
  "https://user-images.githubusercontent.com/37679460/134656546-6155f99e-5265-4974-8b05-3693e2024fe3.png")

musu_data = within(musu_data, year_group <- factor(year_group, levels=c("2020", "2019", "과거 평년")))

cumflow_data = musu_data %>% 
  group_by(y) %>%
  mutate_at(vars(`금곡-상부`:`회죽-하부`, repi, flow), cumsum) %>% 
  dplyr::select(date, y, `금곡-상부`:`회죽-하부`, repi, flow, md, month, year_group)

flow_data = musu_data %>% 
  group_by(year_group, month) %>%
  summarise(sum_flow = mean(flow)) %>% 
  filter(month %in% c("04","05", "06", "07", "08", "09", "10"))

musu_area_data = musu_data %>% 
  group_by(year_group, month) %>% 
  summarise_at(vars(`금곡-상부`:`회죽-하부`), mean) %>% 
  filter(month %in% c("04","05", "06", "07", "08", "09", "10")) 

musu_area_data = musu_area_data %>% 
  gather(area, mean_flow, -year_group, -month)%>% 
  mutate(area_category = substr(area, 1, 2))

musu_area_coord_data = data.frame(
  name = c("금곡-상부",  "금곡-중부", "금곡-하부", "회죽-상부", "회죽-중부", "회죽-하부"),
  lat = c(36.97453, 36.96241, 36.95053, 36.97362, 36.96496, 36.94749),
  lng = c(127.43519, 127.43939, 127.44306, 127.42931, 127.42718, 127.42393)
  )
# musu_area_coord_data$lng = format(musu_area_coord_data$lng, digits = 8 )

#### header ####
header = dashboardHeader(title = 'Test dashboard')

#### sidebar ####
sidebar = dashboardSidebar(
  shinyDashboardThemes(
    theme = "purple_gradient"
  ),
  sidebarMenu(
    id="tabs",
    menuItem("Home", icon = icon("line-chart"), tabName = "Home", badgeColor = "green", selected = T),
    menuItem("Information", icon = icon("bar-chart"), tabName = "Information", badgeColor = "green"),
    menuItem("Map", icon = icon("map-marked-alt"), tabName = "Map", badgeColor = "green")
  )
)

#### body ####
body = dashboardBody(
  shinyDashboardThemes(
    theme = "purple_gradient"
  ),
  ############ Home #############
  tabItems(
    tabItem(tabName = "Home",
            h3("저수지 상황"),
            fluidRow(
              column(width = 12,
                     valueBoxOutput("rate"),
                     valueBoxOutput("amount"),
                     valueBoxOutput("cum_flow"))
            ),
            fluidRow(
              column(width=12,
                     valueBoxOutput("yesterday_rate"),
                     valueBoxOutput("yesterday_amount"),
                     valueBoxOutput("yesterday_cum_flow")
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
                       width = 12,
                       h3('저수율'),
                       status = 'primary',
                       highchartOutput(
                         'water_rate_plot'
                       )
                     )
              ),
              column(width = 6,
                     box(
                       width = 12,
                       h3('누적 유량'),
                       status = 'primary',
                       highchartOutput(
                         'cumulative_flow_plot'
                       )
                     )
              )
            ),
            fluidRow(
              column(
                width = 3,
                offset = 1,
                dateRangeInput("water_rate_date", label= "Date range:", start = "2020-01-01", end = "2020-12-31", min = "2020-01-01", max ="2020-12-31")
              ),
              column(
                width = 2,
                h4("월별 그래프"),
                checkboxInput('month_checkbox', "Month", FALSE)
              ),
              column(
                width = 3,
                offset = 1,
                dateRangeInput("cum_flow_date", label= "Date range:", start = "2020-01-01", end = "2020-12-31", min = "2020-01-01", max ="2020-12-31")
              ), 
              column(
                width = 2,
                h4("월별 그래프"),
                checkboxInput('flow_month_checkbox', "Month", FALSE)
              )
            )
    ),
    ########### Inform ############
    tabItem(tabName = "Information",
            fluidRow(
              tabBox(
                title = "저수지 유량",
                side = "right",
                selected = "유량",
                tabPanel("유량", highchartOutput('monthly_flow_plot')),
                tabPanel("누적 유량", highchartOutput('monthly_cumflow_plot'))
              )
            )
    ),
    
    ############ Map #############
    tabItem(tabName = "Map",
      fluidPage(
        fluidRow(
          column(
            width = 6,
            tabBox(
              width = 12,
              title = "간선 지역별 유량", 
              side = "right",
              height = "250px",
              selected = "유량",
              tabPanel("유량", highchartOutput('click_area_flow_plot')),
              tabPanel("누적 유량", highchartOutput('click_area_cumflow_plot'))
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              status = 'primary',
              leafletOutput('map', width = '100%', height = 550)  
            )
          )
        ),
        fluidRow(
          tabBox(
            title = "금곡간선 지역별 유량", 
            side = "right",
            height = "250px",
            selected = "2020",
            tabPanel("2020", highchartOutput('geumgok1_musu_area_plot')),
            tabPanel("2019", highchartOutput('geumgok2_musu_area_plot')),
            tabPanel("과거 평년", highchartOutput('geumgok3_musu_area_plot'))
          ),
          tabBox(
            title = "회죽간선 지역별 유량",
            side = "right",
            height = "250px",
            selected = "2020",
            tabPanel("2020", highchartOutput('heoijuk1_musu_area_plot')),
            tabPanel("2019", highchartOutput('heoijuk2_musu_area_plot')),
            tabPanel("과거 평년", highchartOutput('heoijuk3_musu_area_plot'))
          )
        )
      )
    )
  )
)

#### server ####
server = function(input, output) 
{
  ### Reactive DATA
  tmp_musu_data = reactive({
    musu_data %>% 
      group_by(year_group, md) %>% 
      summarise(amount = mean(amount), 
                rate = mean(rate))
  })
  
  tmp_cumflow_data = reactive({
    cumflow_data %>% 
      group_by(year_group, md) %>% 
      summarise_at(vars(`금곡-상부`:`회죽-하부`, flow, repi), mean)
  })
  
  spi_reactive = reactive({
    data %>%
      filter(date >= "2020-01-01") %>%
      gather(spi_type, spi_value, starts_with('SPI'))
  })
  
  tmp_flow_data = reactive({
    flow_data
  })
  
  tmp_montly_cumflow_data = reactive({
    cumflow_data %>% 
      group_by(year_group, month) %>% 
      summarise(flow = mean(flow)) %>% 
      filter(month %in% c("04","05", "06", "07", "08", "09", "10"))
  })
  
  tmp_musu_area_data = reactive({
    musu_area_data
  })
  
  tmp_click_area_flow_data = reactive({
    musu_data %>% 
      dplyr::select(year_group, month, `금곡-상부`:`회죽-하부`) %>% 
      filter(month %in% c("04","05", "06", "07", "08", "09", "10")) %>% 
      group_by(year_group, month) %>% 
      summarise_all(mean) %>% 
      gather(key = area, mean_flow, -year_group, -month)
  })
  
  tmp_click_area_cumflow_data = reactive({
    musu_data %>% 
      dplyr::select(year_group, y, month, md,`금곡-상부`:`회죽-하부`) %>% 
      group_by(y) %>% 
      mutate_at(vars(`금곡-상부`:`회죽-하부`), cumsum) %>% 
      ungroup() %>% 
      group_by(year_group, md) %>% 
      summarise_at(vars(`금곡-상부`:`회죽-하부`), mean) %>%
      gather(key = area, value = mean_flow, -year_group, -md)
  })
  
  musu_area_coord_df = reactive({
    musu_area_coord_data
  })
  
  ######## Tab Home ##########
  
  ### Value Box
  output$rate <- renderValueBox({
    valueBox(
      paste0(tmp_musu_data() %>% filter(year_group == '2020') %>% tail(1) %>% .$rate %>% round(., 2) %>% as.character(), "%"), "현재 저수율", icon = icon("thumbs-up"), color='blue'
    )
  })
  
  output$amount <- renderValueBox({
    valueBox(
      paste0(tmp_musu_data() %>% filter(year_group == '2020') %>% tail(1) %>% .$amount %/% 1000 %>% as.character(), 'K'), "현재 저수량", icon = icon("thumbs-up"), color='purple'
    )
  })
  
  output$cum_flow <- renderValueBox({
    valueBox(
      paste0(tmp_cumflow_data() %>% filter(year_group == '2020') %>% tail(1) %>% .$flow %/% 1000 %>% as.character(), 'K'), "현재까지 누적유량", icon = icon("thumbs-up"), color='olive'
    )
  })
  
  output$yesterday_rate <- renderValueBox({
    valueBox(
      paste0(tmp_musu_data() %>% filter(year_group == '2020') %>% slice(n()-1) %>% .$rate %>% round(., 2) %>% as.character(), "%"), "전날 저수율", icon = icon("thumbs-up"), color='blue'
    )
  })
  
  output$yesterday_amount <- renderValueBox({
    valueBox(
      paste0(tmp_musu_data() %>% filter(year_group == '2020') %>% slice(n()-1) %>% .$amount %/% 1000 %>% as.character(), 'K'), "전날 저수량", icon = icon("thumbs-up"), color='purple'
    )
  })
  
  output$yesterday_cum_flow <- renderValueBox({
    valueBox(
      paste0(tmp_cumflow_data() %>% filter(year_group == '2020') %>% slice(n()-1) %>% .$flow %/% 1000 %>% as.character(), 'K'), "전날까지 누적유량", icon = icon("thumbs-up"), color='olive'
    )
  })
  
  ### HighChart
  
  ### Reservior Water Rate plot
  
  observeEvent(input$water_rate_date, {
    dates <- input$water_rate_date
    start <- format(dates[1], '%m-%d')
    end <- format(dates[2], '%m-%d')
    output$water_rate_plot <- renderHighchart({
      highchart() %>% 
        hc_yAxis(
          title=list(text="Water Rate"),
          plotLines = list(list(value = 50, color = "red", width = 2,
                                dashStyle = "dash"),
                           list(value = 60, color = 'orange', width = 2,
                                dashStyle = 'dash'))
        ) %>% 
        hc_xAxis(
          labels = list(style = list(fontSize = 8)),
          categories = tmp_musu_data() %>% 
            filter(between(md, start, end)) %>% 
            .$md, type='datetime'
        ) %>% 
        hc_add_series(
          tmp_musu_data() %>% 
            filter(between(md, start, end)),
          type='line',
          hcaes(y=rate, group=year_group)
        ) %>% 
        hc_tooltip(valueDecimals = 2) %>% 
        hc_add_theme(hc_theme_superheroes())
    })
  })
  
  observeEvent(input$month_checkbox, {
    cat("Month selectd! \n")
    if(input$month_checkbox){
      output$water_rate_plot <- renderHighchart({
        highchart() %>% 
          hc_yAxis(
            title=list(text="Water Rate"),
            plotLines = list(list(value = 50, color = "red", width = 2,
                                  dashStyle = "dash"),
                             list(value = 60, color = 'orange', width = 2,
                                  dashStyle = 'dash'))
          ) %>% 
          hc_xAxis(
            labels = list(style = list(fontSize = 8)),
            categories = tmp_musu_data() %>% 
              filter(between(md, '08-01', '08-31')) %>% 
              .$md, type='datetime'
          ) %>% 
          hc_add_series(
            tmp_musu_data() %>% 
              filter(between(md, '08-01', '08-31')),
            type='line',
            hcaes(y=rate, group=year_group)
          ) %>% 
          hc_tooltip(valueDecimals = 2) %>% 
          hc_add_theme(hc_theme_superheroes())
      })
    } else {
      output$water_rate_plot <- renderHighchart({
        highchart() %>% 
          hc_yAxis(
            title=list(text="Water Rate"),
            plotLines = list(list(value = 50, color = "red", width = 2,
                                  dashStyle = "dash"),
                             list(value = 60, color = 'orange', width = 2,
                                  dashStyle = 'dash'))
          ) %>% 
          hc_xAxis(
            labels = list(style = list(fontSize = 8)),
            categories = tmp_musu_data()$md, type='datetime'
          ) %>% 
          hc_add_series(
            tmp_musu_data(),
            type='line',
            hcaes(y=rate, group=year_group)
          ) %>% 
          hc_tooltip(valueDecimals = 2) %>% 
          hc_add_theme(hc_theme_superheroes())
      })
    }
    
  }, ignoreInit = TRUE)
  
  ###Cumulative flow plot
  
  observeEvent(input$cum_flow_date, {
    cum_flow_dates <- input$cum_flow_date
    cum_flow_date_start <- format(cum_flow_dates[1], '%m-%d')
    cum_flow_date_end <- format(cum_flow_dates[2], '%m-%d')
    
    output$cumulative_flow_plot <- renderHighchart({
      highchart() %>% 
        hc_yAxis(
          title=list(text="Cumulative Flow")
        ) %>% 
        hc_xAxis(
          labels = list(style = list(fontSize = 8)),
          categories = tmp_cumflow_data() %>% 
            filter(between(md, cum_flow_date_start, cum_flow_date_end)) %>% 
            .$md, type='datetime'
        ) %>% 
        hc_add_series(
          tmp_cumflow_data() %>% 
            filter(between(md, cum_flow_date_start, cum_flow_date_end)),
          type='line',
          hcaes(y=flow, group=year_group)
        ) %>% 
        hc_tooltip(valueDecimals = 2) %>% 
        hc_add_theme(hc_theme_superheroes())
    })
  })
  
  observeEvent(input$flow_month_checkbox, {
    cat("Month selectd! \n")
    if(input$flow_month_checkbox){
      output$cumulative_flow_plot <- renderHighchart({
        highchart() %>% 
          hc_yAxis(
            title=list(text = "Cumulative Flow")
          ) %>% 
          hc_xAxis(
            labels = list(style = list(fontSize = 8)),
            categories = tmp_cumflow_data() %>% 
              filter(between(md, '08-01', '08-31')) %>% 
              .$md, type='datetime'
          ) %>% 
          hc_add_series(
            tmp_cumflow_data() %>% 
              filter(between(md, '08-01', '08-31')),
            type='line',
            hcaes(y=flow, group=year_group)
          ) %>% 
          hc_tooltip(valueDecimals = 2) %>% 
          hc_add_theme(hc_theme_superheroes())
      })
    } else {
      output$cumulative_flow_plot <- renderHighchart({
        highchart() %>% 
          hc_yAxis(
            title=list(text = "Cumulative Flow")
          ) %>% 
          hc_xAxis(
            labels = list(style = list(fontSize = 8)),
            categories = tmp_cumflow_data()$md, type='datetime'
          ) %>% 
          hc_add_series(
            tmp_cumflow_data(),
            type='line',
            hcaes(y=flow, group=year_group)
          ) %>% 
          hc_tooltip(valueDecimals = 2) %>% 
          hc_add_theme(hc_theme_superheroes())
      })
    }
    
  }, ignoreInit = TRUE)
  
  ######### Tab Information ##########
  
  ### Highchart ###
  
  output$monthly_flow_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_yAxis(title = list(text = "Monthly Flow")) %>%
      hc_add_series(tmp_flow_data(), hcaes(x = month, y = sum_flow, group = year_group), type = 'column') %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$monthly_cumflow_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories =  c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_yAxis(title = list(text = "Monthly Cumulative Flow")) %>%
      hc_add_series(tmp_montly_cumflow_data(), hcaes(x = year_group, y = flow, group = year_group), type = 'column') %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$geumgok1_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '2020') & (area_category == '금곡')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$geumgok2_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '2019') & (area_category == '금곡')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$geumgok3_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '과거 평년') & (area_category == '금곡')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$heoijuk1_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '2020') & (area_category == '회죽')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$heoijuk2_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '2019') & (area_category == '회죽')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  output$heoijuk3_musu_area_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_musu_area_data() %>% filter((year_group == '과거 평년') & (area_category == '회죽')), hcaes(x = month, y = mean_flow, group = area), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    
  })
  
  
  #### Map Tap #####

  image_geumgok_path = paste0(getwd(), list.files("../image_file")[1:3])
  image_heoijuk_path = paste0(getwd(), list.files("../image_file")[4:6])
  
  # output$map = renderLeaflet({
  # leaflet(options = leafletOptions(zoomControl = FALSE,  minZoom = 13, maxZoom = 13)) %>%
  #   setView(lat = 36.96362, lng = 127.43329, zoom = 13) %>%
  #   addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
  #   addPolygons(data = polygon.musu,
  #               stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = '#0072B5'
  #   ) %>%
  #   addPolylines(data = path.geumgok, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#926AA6') %>%
  #   addPolylines(data = path.heoijuk, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#264E36') %>%
  #   addAwesomeMarkers(data = geumgok_marker_df, layerId = ~Name, popup = paste(sep = '<br/>', ~htmlEscape(Name), popupImage(image_geumgok_path, "local"))) %>%
  #   addAwesomeMarkers(data = heoijuk_marker_df, layerId = ~Name, popup = paste(sep = '<br/>', ~htmlEscape(Name), popupImage(image_heoijuk_path, "local")))
  # 
  # })
  
  # output$map = renderLeaflet({
  #   leaflet(options = leafletOptions(zoomControl = FALSE,  minZoom = 13, maxZoom = 13)) %>%
  #     setView(lat = 36.96362, lng = 127.43329, zoom = 13) %>%
  #     addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
  #     addPolygons(data = polygon.musu,
  #                 stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = '#0072B5'
  #     ) %>%
  #     addPolylines(data = path.geumgok, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#926AA6') %>%
  #     addPolylines(data = path.heoijuk, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#264E36') %>%
  #     addAwesomeMarkers(data = geumgok_marker_df, layerId = ~Name, popup = paste(sep = '<br/>', musu_area_coord_data$name[1:3], popupImage(img_path[1:3], width=100, height=100))) %>%
  #     addAwesomeMarkers(data = heoijuk_marker_df, layerId = ~Name, popup = paste(sep = '<br/>', musu_area_coord_data$name[4:6], popupImage(img_path[4:6], width=100, height=100)))
  #   
  # })
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,  minZoom = 13, maxZoom = 13)) %>%
      setView(lat = 36.96362, lng = 127.43329, zoom = 13) %>%
      addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
      addPolygons(data = polygon.musu,
                  stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = '#0072B5'
      ) %>%
      addPolylines(data = path.geumgok, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#926AA6') %>%
      addPolylines(data = path.heoijuk, opacity = 0.7, dashArray = "5,5", weight = 4, color = '#264E36') %>%
      addAwesomeMarkers(data = geumgok_marker_df, layerId = ~Name, label = musu_area_coord_data$name[1:3], popup = popupImage(img_path[1:3], width=100, height=100)) %>%
      addAwesomeMarkers(data = heoijuk_marker_df, layerId = ~Name, label = musu_area_coord_data$name[4:6], popup = popupImage(img_path[4:6], width=100, height=100))
    
  })
  
  area_name = '회죽-상부'
  
  output$click_area_flow_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
      hc_add_series(data = tmp_click_area_flow_data() %>% filter(area %in% area_name), hcaes(x = month, y = mean_flow, group = year_group), type = "column") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$click_area_cumflow_plot <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = tmp_click_area_cumflow_data()$md %>% unique(), type = 'datetime') %>%
      hc_add_series(data = tmp_click_area_cumflow_data() %>% filter(area %in% area_name), hcaes(x = md, y = mean_flow, group = year_group), type = "line") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_add_theme(hc_theme_superheroes())
    })
  
  cl <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,{
    
    cl$clickedMarker <- input$map_marker_click
    output$click_area_flow_plot <- renderHighchart({
      highchart() %>%
        hc_xAxis(categories = c("04","05", "06", "07", "08", "09", "10")) %>%
        hc_add_series(data = tmp_click_area_flow_data() %>% filter(area == cl$clickedMarker$id), hcaes(x = month, y = mean_flow, group = year_group), type = "column") %>%
        hc_tooltip(valueDecimals = 2) %>%
        hc_add_theme(hc_theme_superheroes())
    })

    output$click_area_cumflow_plot <- renderHighchart({
      highchart() %>%
        hc_xAxis(categories = tmp_click_area_cumflow_data()$md %>% unique(), type = 'datetime') %>%
        hc_add_series(data = tmp_click_area_cumflow_data() %>% filter(area == cl$clickedMarker$id), hcaes(x = md, y = mean_flow, group = year_group), type = "line") %>%
        hc_tooltip(valueDecimals = 2) %>%
        hc_add_theme(hc_theme_superheroes())
    })

  })

}

ui = dashboardPage(
  header,
  sidebar,
  body
)

shinyApp(ui, server)