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
if(!require(reticulate)) install.packages("reticulate"); library(reticulate)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(sf)) install.packages("sf"); library(sf)

### data path: shiny_data path, ex) "./Desktop/lab/shiny_data/"
data_path = "/Users/chulhongsung/Desktop/lab/새론솔루션/shiny/data" 

np <- import("numpy")

setwd(data_path)

#### 지도 shape 파일 ####
musu_kml = st_read("musu.kml")

musu_kml_n <- st_zm(musu_kml[1], drop=T, what='ZM')
as.data.frame(musu_kml_n) -> musu_kml.df
as(musu_kml_n, "Spatial") -> polygon.musu

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

#### musu data  ####
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

forecast_data =  np$load("forecast_value.npy")/1370 * 100

forecast_df = data.frame(
  date = c(rev(seq(musu_data$date[nrow(musu_data)], by = "-1 day", length.out = 10)[-1]), seq(musu_data$date[nrow(musu_data)], by = "day", length.out = 6)),
  rate = NA,
  year_group = 'forecast'
) %>% mutate(md = format(date, "%m-%d"))

forecast_df[10,'rate'] = musu_data$rate %>% tail(1)
forecast_df[-c(1:10), 'rate'] = forecast_data

forecast_df = within(forecast_df, year_group <- factor(year_group, levels=c("2020", "2019", "과거 평년", 'forecast')))

#### 강수량 & 가뭄지수 ####

prec_df = read_excel('강수량_1999-2020.xls')

prec_df = prec_df %>% dplyr::select(관측일, 금년일강수량) %>%
  plyr::rename(c('관측일' = 'date', '금년일강수량' = 'prec')) %>% 
  mutate(date = as.Date(date),
         prec = as.numeric(prec),
         md = format(date, "%m-%d")) %>% 
  filter(date <= as.Date('2020-08-31'))

# 평년 시나리오
future_mean_prec_df = prec_df %>% 
  group_by(md) %>% 
  summarise(prec = mean(prec, na.rm=T)) %>%
  mutate(date = as.Date(paste0("2020-", md))) %>% 
  dplyr::select(date, prec, md)

 # 무강수 시나리오
future_zero_prec_df = future_mean_prec_df %>% mutate(prec = if_else(md > "08-31", 0, prec))

mean_prec_df = prec_df %>% bind_rows(future_mean_prec_df) %>% distinct(date, .keep_all = T) %>% arrange(date)
zero_prec_df = prec_df %>% bind_rows(future_zero_prec_df) %>% distinct(date, .keep_all = T) %>% arrange(date)

cal_spi = function(df, a_hat, b_hat){
  return(qnorm(pgamma(df+0.00001, shape=a_hat, scale=b_hat)))
}


#### header ####
header = dashboardHeader(title = tags$img(src='https://user-images.githubusercontent.com/37679460/134848905-56402ff4-2cba-4b5d-b24e-cbadf469c36c.png', height = '60', width ='140'))

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
            h3("저수지 상황판"),
            fluidRow(
              column(
                width = 3,
                valueBoxOutput("rate", width = 12)
                ),
              column(
                width = 3,
                valueBoxOutput("amount", width = 12)
               ),
              column(
                width = 3, 
                valueBoxOutput("cum_flow", width = 12)
              ),
              column(
                width = 3,
                valueBoxOutput("prec", width = 12)
              )
            ),
            fluidRow(
              column(
                width = 3,
                valueBoxOutput("yesterday_rate", width = 12)
              ),
              column(
                width = 3,
                valueBoxOutput("yesterday_amount", width = 12)
              ),
              column(
                width = 3, 
                valueBoxOutput("yesterday_cum_flow", width = 12)
              ),
              column(
                width = 3,
                valueBoxOutput("yesterday_prec", width = 12)
              )
            ),
            fluidRow(
              column(
                width = 4, 
                dateRangeInput("water_rate_date", label= h4("데이터 기간:"), start = "2020-01-01", end = "2020-12-31", min = "2020-01-01", max ="2020-12-31", width = "85%" )
                ), 
              column(
                width = 2,
                h4('월별 그래프'),
                checkboxInput('month_checkbox', "Month", FALSE)
                )
            ),
            fluidRow(
              column(width = 4,
                     h3('저수율'),
                     box(
                       width = 12,
                       status = 'primary',
                       highchartOutput(
                         'water_rate_plot'
                       )
                     )
                  ),
              column(width = 4,
                     h3('누적 유량'),
                     box(
                       width = 12,
                       status = 'primary',
                       highchartOutput(
                         'cumulative_flow_plot'
                       )
                     )
                  ), 
              column(
                width = 4,
                h3('단기 예측 저수율'),
                box(
                  width = 12,
                  status = 'primary',
                  highchartOutput(
                    'forecasting_ratio_plot'
                  )
                )
              )
            )
    ),
    ########### Inform ############
    tabItem(tabName = "Information",
            fluidRow(
              column(
                width = 6,
                tabBox(
                  width = 12,
                  title = "저수지 유량",
                  side = "right",
                  selected = "유량",
                  tabPanel("유량", highchartOutput('monthly_flow_plot')),
                  tabPanel("누적 유량", highchartOutput('monthly_cumflow_plot'))
                )
              ),
              column(
                width = 6,
                box(
                  width = 12,
                  title = "가뭄지수(SPI)",
                  status = 'primary',
                  highchartOutput('spi_plot')
                )
              )
            ),
            fluidRow(
              h2("강수 시나리오 가뭄지수"),
              column(
                width = 6,
                box(
                  width = 12,
                  title = "평년강수 시나리오 가뭄지수(SPI)",
                  status = 'primary',
                  highchartOutput('mean_spi_plot')
                  )
                ),
              column(
                width = 6,
                box(
                  width = 12,
                  title = "무강수 시나리오 가뭄지수(SPI)",
                  status = 'primary',
                  highchartOutput('zero_spi_plot')
                )
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
  #### Reactive Data ####
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
  
  spi_data = reactive({
    
    # 누적합 계산
    tmp_prec_df = mean_prec_df %>% filter(date <= "2020-08-31")
    
    cum_prec_df = data.frame(date=tmp_prec_df$date, precip=tmp_prec_df$prec, 
                             cum_30=RcppRoll::roll_sum(tmp_prec_df$prec, 30, fill=NA, align="right"),
                             cum_60=RcppRoll::roll_sum(tmp_prec_df$prec, 60, fill=NA, align="right"),
                             cum_90=RcppRoll::roll_sum(tmp_prec_df$prec, 90, fill=NA, align="right"))
    
    cum_prec_df = cum_prec_df %>% filter(date >= as.Date('2000-01-01'))
    
    a_hat1 = 0.5 / (log(mean(cum_prec_df$cum_30))- mean(log(cum_prec_df$cum_30 + 0.00001))); b_hat1 = mean(cum_prec_df$cum_30) / a_hat1
    a_hat2 = 0.5 / (log(mean(cum_prec_df$cum_60))- mean(log(cum_prec_df$cum_60 + 0.00001))); b_hat2 = mean(cum_prec_df$cum_60) / a_hat2
    a_hat3 = 0.5 / (log(mean(cum_prec_df$cum_90))- mean(log(cum_prec_df$cum_90 + 0.00001))); b_hat3 = mean(cum_prec_df$cum_90) / a_hat3
    
    cum_prec_df %>% mutate(SPI30 = cal_spi(cum_prec_df$cum_30, a_hat1, b_hat1),
                           SPI60 = cal_spi(cum_prec_df$cum_60, a_hat2, b_hat2),
                           SPI90 = cal_spi(cum_prec_df$cum_90, a_hat3, b_hat3),
                           md = format(date, "%m-%d")) %>% 
      filter(date >= "2020-01-01") %>%
      gather(spi_type, spi_value, starts_with('SPI'))
  })
  
  mean_spi_data = reactive({
    cum_prec_df = data.frame(date = mean_prec_df$date, precip=mean_prec_df$prec, 
                             cum_30=RcppRoll::roll_sum(mean_prec_df$prec, 30, fill=NA, align="right"),
                             cum_60=RcppRoll::roll_sum(mean_prec_df$prec, 60, fill=NA, align="right"),
                             cum_90=RcppRoll::roll_sum(mean_prec_df$prec, 90, fill=NA, align="right"))
    
    cum_prec_df = cum_prec_df %>% filter(date >= as.Date('2000-01-01'))
    
    a_hat1 = 0.5 / (log(mean(cum_prec_df$cum_30))- mean(log(cum_prec_df$cum_30 + 0.00001))); b_hat1 = mean(cum_prec_df$cum_30) / a_hat1
    a_hat2 = 0.5 / (log(mean(cum_prec_df$cum_60))- mean(log(cum_prec_df$cum_60 + 0.00001))); b_hat2 = mean(cum_prec_df$cum_60) / a_hat2
    a_hat3 = 0.5 / (log(mean(cum_prec_df$cum_90))- mean(log(cum_prec_df$cum_90 + 0.00001))); b_hat3 = mean(cum_prec_df$cum_90) / a_hat3
    
    cum_prec_df %>% mutate(SPI30 = cal_spi(cum_prec_df$cum_30, a_hat1, b_hat1),
                           SPI60 = cal_spi(cum_prec_df$cum_60, a_hat2, b_hat2),
                           SPI90 = cal_spi(cum_prec_df$cum_90, a_hat3, b_hat3),
                           md = format(date, "%m-%d")) %>% 
      filter(date >= "2020-01-01") %>%
      gather(spi_type, spi_value, starts_with('SPI'))
 
  })
  
  zero_spi_data = reactive({
    cum_prec_df = data.frame(date = zero_prec_df$date, precip=zero_prec_df$prec, 
                             cum_30=RcppRoll::roll_sum(zero_prec_df$prec, 30, fill=NA, align="right"),
                             cum_60=RcppRoll::roll_sum(zero_prec_df$prec, 60, fill=NA, align="right"),
                             cum_90=RcppRoll::roll_sum(zero_prec_df$prec, 90, fill=NA, align="right"))
    
    cum_prec_df = cum_prec_df %>% filter(date >= as.Date('2000-01-01'))
    
    a_hat1 = 0.5 / (log(mean(cum_prec_df$cum_30))- mean(log(cum_prec_df$cum_30 + 0.00001))); b_hat1 = mean(cum_prec_df$cum_30) / a_hat1
    a_hat2 = 0.5 / (log(mean(cum_prec_df$cum_60))- mean(log(cum_prec_df$cum_60 + 0.00001))); b_hat2 = mean(cum_prec_df$cum_60) / a_hat2
    a_hat3 = 0.5 / (log(mean(cum_prec_df$cum_90))- mean(log(cum_prec_df$cum_90 + 0.00001))); b_hat3 = mean(cum_prec_df$cum_90) / a_hat3
    
    cum_prec_df %>% mutate(SPI30 = cal_spi(cum_prec_df$cum_30, a_hat1, b_hat1),
                           SPI60 = cal_spi(cum_prec_df$cum_60, a_hat2, b_hat2),
                           SPI90 = cal_spi(cum_prec_df$cum_90, a_hat3, b_hat3),
                           md = format(date, "%m-%d")) %>% 
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
  
  tmp_forecast_df = reactive({
    tmp_date = forecast_df$md
    
    musu_data %>% filter(md %in% tmp_date) %>% 
      dplyr::select(date, rate, year_group, md) %>% 
      bind_rows(forecast_df) %>% 
      group_by(year_group, md) %>% 
      summarise(mean_rate = mean(rate))
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
  
  output$prec <- renderValueBox({
    valueBox(
      "5mm", "어제 강수량", icon = icon("thumbs-up"), color='olive'
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
  
  output$yesterday_prec <- renderValueBox({
    valueBox(
      "5mm", "엊그제 강수량", icon = icon("thumbs-up"), color='olive'
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
            .$md %>% unique(), type='datetime'
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
              .$md %>% unique(), type='datetime'
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
            categories = tmp_musu_data()$md %>% unique(), type='datetime'
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
  
  observeEvent(input$water_rate_date, {
    cum_flow_dates <- input$water_rate_date
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
            .$md %>% unique(), type='datetime'
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
  
  observeEvent(input$month_checkbox, {
    cat("Month selectd! \n")
    if(input$month_checkbox){
      output$cumulative_flow_plot <- renderHighchart({
        highchart() %>% 
          hc_yAxis(
            title=list(text = "Cumulative Flow")
          ) %>% 
          hc_xAxis(
            labels = list(style = list(fontSize = 8)),
            categories = tmp_cumflow_data() %>% 
              filter(between(md, '08-01', '08-31')) %>% 
              .$md %>% unique(), type='datetime'
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
            categories = tmp_cumflow_data()$md %>% unique(), type='datetime'
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
  
  #### forecast plot ####
  output$forecasting_ratio_plot <- renderHighchart({
    highchart() %>% 
      hc_yAxis(
        title=list(text = "Reservior Rate")
      ) %>% 
      hc_xAxis(
        labels = list(style = list(fontSize = 8)),
        categories = tmp_forecast_df()$md %>% unique(), # type='datetime',
        plotLines = list(list(value = 9, color = "red", width = 2, dashStyle = "dash"))
      ) %>% 
      hc_add_series(
        tmp_forecast_df(),
        type='line',
        hcaes(x=md, y=mean_rate, group=year_group)
      ) %>% 
      hc_tooltip(valueDecimals = 2) %>% 
      hc_add_theme(hc_theme_superheroes())
  })
  
  ######### Tab Information ##########
  
  output$spi_plot <- renderHighchart({
    highchart() %>% 
      hc_yAxis(
        title=list(text="SPI index"),
        plotLines = list(list(value = -1, color = "red", width = 2,
                              dashStyle = "shortdash"))
      ) %>% 
      hc_xAxis(
        categories = spi_data()$md %>% unique(), type='datetime'
      ) %>% 
      hc_add_series(
        spi_data(),
        type='line',
        hcaes(x=md, y=spi_value, group=spi_type)
      ) %>% 
      hc_tooltip(valueDecimals = 2) %>% 
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$mean_spi_plot <- renderHighchart({
    highchart() %>% 
      hc_yAxis(
        title=list(text="SPI index"),
        plotLines = list(list(value = -1, color = "red", width = 2,
                              dashStyle = "shortdash"))
      ) %>% 
      hc_xAxis(
        categories = mean_spi_data()$md %>% unique(), type='datetime',
        plotLines = list(list(value = 243, color = "yellow", width = 2, dashStyle = "dash")) # value 수정 필요
      ) %>% 
      hc_add_series(
        mean_spi_data(),
        type='line',
        hcaes(x=md, y=spi_value, group=spi_type)
      ) %>% 
      hc_tooltip(valueDecimals = 2) %>% 
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$zero_spi_plot <- renderHighchart({
    highchart() %>% 
      hc_yAxis(
        title=list(text="SPI index"),
        plotLines = list(list(value = -1, color = "red", width = 2,
                              dashStyle = "shortdash"))
      ) %>% 
      hc_xAxis(
        categories = zero_spi_data()$md %>% unique(), type='datetime',
        plotLines = list(list(value = 243, color = "yellow", width = 2, dashStyle = "dash")) # value 수정 필요
      ) %>% 
      hc_add_series(
        zero_spi_data(),
        type='line',
        hcaes(x=md, y=spi_value, group=spi_type)
      ) %>% 
      hc_tooltip(valueDecimals = 2) %>% 
      hc_add_theme(hc_theme_superheroes())
  })
  #### Map Tap #####
  
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
      addProviderTiles("Esri.WorldTopoMap", options= providerTileOptions(opacity = 0.99)) %>% 
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

