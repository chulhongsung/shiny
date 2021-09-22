rm(list = ls())
gc(reset = T)

if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if(!require(plotly)) install.packages('plotly'); library(plotly)
if(!require(leaflet)) install.packages('leaflet'); library(leaflet)
if(!require(dashboardthemes)) install.packages("dashboardthemes"); library(dashboardthemes)
if(!require(DT)) install.packages('DT'); library(DT)
if(!require(highcharter)) install.packages('highcharter'); library(highcharter)

setwd("C:/Users/chica/Desktop/rshiny/data")

musu_data = readxl::read_xlsx('무수저수지 데이터셋.xlsx')

colnames(musu_data) = c('date', 'amount', 'rate', 'geumgok1', 'geumgok2', 'geumgok3', 
                        'heoijuk1', 'heoijuk2', 'heoijuk3', 'temp', 'mois', 'sun', 'repi',
                        'sky1', 'temp_high1', 'temp_low1', 'temp_avg1', 'repi_prop1',
                        'sky2', 'temp_high2', 'temp_low2', 'temp_avg2', 'repi_prop2')

musu_data = musu_data %>%
  mutate(y = format(date, '%Y'), md = format(date, '%m-%d'), month = format(date, '%m'), flow = geumgok1 + heoijuk1) %>%
  mutate(year_group = if_else(y == '2020', '2020', '과거 평년')) %>% 
  mutate(year_group = if_else(y == '2019', '2019', year_group))


musu_data = within(musu_data, year_group <- factor(year_group, levels=c("2020", "2019", "과거 평년")))

cumflow_data = musu_data %>% 
  group_by(y) %>%
  mutate_at(vars(geumgok1:heoijuk3, repi, flow), cumsum) %>% 
  select(date, y, geumgok1:heoijuk3, repi, flow, md, month, year_group)

flow_data = musu_data %>% 
  group_by(year_group, month) %>%
  summarise(sum_flow = mean(flow)) %>% 
  filter(month %in% c("04","05", "06", "07", "08", "09", "10"))

musu_area_data = musu_data %>% 
  group_by(year_group, month) %>% 
  summarise_at(vars(geumgok1:heoijuk3), mean) %>% 
  filter(month %in% c("04","05", "06", "07", "08", "09", "10")) 

colnames(musu_area_data) = c('year_group', 'month', '금곡-상부', '금곡-중부', '금곡-하부', '회죽-상부', '회죽-중부', '회죽-하부')

musu_area_data = musu_area_data %>% 
  gather(area, mean_flow, -year_group, -month)%>% 
  mutate(area_category = substr(area, 1, 2))


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
          width=4,
          offset = 1,
          dateRangeInput(
            "water_rate_date",
            label= "Date range:",
            start = "2020-01-01",
            end = "2020-08-31",
            min = "2020-01-01",
            max ="2020-08-31"),
          checkboxInput(
            'month_checkbox',
            "Month",
            FALSE)
        ),
        column(
          width=4,
          offset = 2,
          dateRangeInput(
            "cum_flow_date",
            label= "Date range:",
            start = "2020-01-01",
            end = "2020-08-31",
            min = "2020-01-01",
            max ="2020-08-31"),
          checkboxInput(
            'flow_month_checkbox',
            "Month",
            FALSE)
        )
      )
    ),
    tabItem(tabName = "Information",
       fluidRow(
         tabBox(
           title = "저수지 유량",
           side = "right",
           selected = "유량",
           tabPanel("유량", highchartOutput('monthly_flow_plot')),
           tabPanel("누적 유량", highchartOutput('monthly_cumflow_plot'))
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
      summarise_at(vars(geumgok1:heoijuk3, flow, repi), mean)
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

}

ui = dashboardPage(
  header,
  sidebar,
  body
)

shinyApp(ui, server)
