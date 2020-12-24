
library(tidyverse)
library(lubridate)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(waiter)
library(reactable)
library(Cairo)


source("dashboard_helpers.R")

theme_set(theme_minimal(base_family = "verdana, sans-serif"))



ui <- dashboardPage(skin = "blue",
    ## header
    dashboardHeader(title = "Big Mac"),
    
    ## Side bar
    dashboardSidebar(
      sidebarUserPanel("A Big Mac Dashboard", 
                       subtitle = " by Emmanuel Ugochukwu",
                       image = "logo.png"),
      
        sidebarMenu(id = "page_1",
                    menuItem(text = "Big Mac and Currency",
                             icon = icon("hamburger"),
                             tabName = "page_1")),
        
        
        sidebarMenu(id = "page_2",
                    menuItem(text = "GDP & Burgernomics",
                             icon = icon("hamburger"),
                             tabName = "page_2"))),
    
    
    ## Body
    dashboardBody(
      use_waiter(),
      waiter_show_on_load(html = spin_loaders(id = 21, color = "#3C8DBC")),
        tabItems(
        tabItem("page_1",
        fluidPage(theme = shinythemes::shinytheme("paper"),
            fluidRow(box(width = 3, height = 120,status = "primary",background = "light-blue",
                         ## To choose country
                         selectInput("v_country",
                                     "Choose Country",
                                     choices = all_countries,
                                     multiple = TRUE,
                                     selected = "Argentina")),
                     box(width = 3, height = 120,status = "info",
                         ## to split
                         radioButtons("v_split",
                                      "Do you want to split plots?",
                                      choices = c("No","Yes"),
                                      selected = "No")),
                     box(width = 3, height = 120,
                         ## To Input Price
                         numericInput("v_random_price",
                                      "Choose a random price (USD)",
                                      value = 2.00,
                                      min = 1.00,
                                      max = 8.00,
                                      step = 0.30)),
                     box(width = 3, height = 120,
                         ## To choose year
                         numericInput("v_year",
                                      "Choose Year",
                                      value = 2002,
                                      min = min(big_mac$year),
                                      max = max(big_mac$year),
                                      step = 2))),
            
            fluidRow(box(width = 12,height = "550px",
                     withSpinner(plotOutput("v_con_price", height = "500px"),
                                 type = 7 ,image = "logo.png", image.width = "75px", image.height = "75px"),
                     downloadButton("v_con_price_download"))),
            
            fluidRow(box(width = 6,status = "info",
                         withSpinner(plotOutput("v_curr_movement"), type = 7,
                                     image = "logo.png", image.width = "75px", image.height = "75px"),
                         downloadButton("v_curr_movement_download")),
                     box(width = 6, status = "info",
                       withSpinner(plotOutput("v_price_search"), type = 7,
                                   image = "logo.png", image.width = "75px", image.height = "75px"),
                       downloadButton("v_price_search_download"))))),
        tabItem("page_2",
                fluidPage(theme = shinythemes::shinytheme("paper"),
                    fluidRow(box(width = 3, height = 140,status = "info",
                                 ## To choose plot filter
                                 radioButtons("v2_plot_filter",
                                             "Summary on Big Mac prices",
                                             choiceNames  = c("Top/Least","Top","Least"),
                                             choiceValues = c(0,1,2))),
                             
                             box(width = 3, height = 140,status = "primary",background = "light-blue",
                                 ## To choose country
                                 selectInput("v2_country",
                                             "Choose Country",
                                             choices = all_countries,
                                             multiple = TRUE)),
                             
                             box(width = 3, height = 140,
                                 ## To choose year
                                 numericInput("v2_year",
                                              "Choose Year",
                                              value = 2002,
                                              min = min(big_mac$year),
                                              max = max(big_mac$year),
                                              step = 1)),
                             box(width = 3, height = 140,status = "info",
                                 ## to split
                                 radioButtons("v2_split",
                                              "Do you want to split plots?",
                                              choices = c("No","Yes"),
                                              selected = "No"))),
                    
                    fluidRow(box(width = 6,height = "470px",
                                withSpinner(plotOutput("v2_static_plot", height = "400px"), type = 7,
                                            image = "logo.png", image.width = "75px", image.height = "75px")),
                      box(width = 6,height = "470px", 
                        withSpinner(reactableOutput("v_BMI_USD"), type = 7,
                                    image = "logo.png", image.width = "75px", image.height = "75px"))),
                    
                    fluidRow(box(width = 7,height = "550px",status = "info",
                                withSpinner(plotOutput("v_gdp", height = "500px"), type = 7,
                                            image = "logo.png", image.width = "75px", image.height = "75px"),
                                downloadButton("v_gdp_download")),
                             box(width = 5, height = "550px",status = "info",
                                withSpinner(plotOutput("v_burger_cont", height = "500px"), type = 7,
                                            image = "logo.png", image.width = "75px", image.height = "75px"),
                                downloadButton("v_burger_cont_download"))))
        )))
    )
         
    



server <- function(input, output) {
    min_year = min(big_mac$year)
    max_year = max(big_mac$year)
    
    waiter_hide()
    

    ## Plot for Big Mac price
    output$v_con_price <- renderPlot({
        big_mac_price <-
            big_mac %>%
            filter(name %in% input$v_country) %>%
            select(year, name, dollar_price) %>%
            ggplot(aes(year, dollar_price, col = name)) +
            geom_line(alpha = 0.6, size = 1.1) +
            scale_y_continuous(labels = scales::dollar_format()) +
            scale_x_continuous(breaks = seq(min_year,max_year,5)) +
            labs(
                title = "Big Mac prices over time",
                x = NULL,
                y = NULL,
                col = NULL
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold", size = 15),
                legend.position = "bottom",
                axis.text = element_text(face = "italic", size = 13),
                strip.text = element_text(face = "italic", size = 14)
            ) +
            guides(col = guide_legend(
                label.position = "top",
                keywidth = 7,
                label.theme = element_text(size = 13)
            ))
        
        ifelse(input$v_split == "Yes",
               print(big_mac_price +
                         facet_wrap(~ name, scales = "free")+
                         theme(legend.position = "none")),
               print(big_mac_price))
    })
    
    ## Plot for USD movement
    output$v_curr_movement <- renderPlot({
   curr_plot <- big_mac %>%
        filter(name %in% input$v_country) %>%
        select(year,name,dollar_ex) %>%
        ggplot(aes(year,dollar_ex, col = name))+
        geom_line(alpha = 0.6, size = 1.4)+
        scale_y_continuous(labels = scales::dollar_format())+
        scale_x_continuous(breaks = seq(min_year,max_year,5))+
        labs(
            title = "Local Currency against USD over time",
            x = NULL,
            y = NULL,
            col = NULL
        )+
        theme_minimal()+
        theme(
            plot.title = element_text(face = "bold", size = 15),
            legend.position = "bottom",
            axis.text = element_text(face = "italic", size = 13),
            strip.text = element_text(face = "italic", size = 14)
        )+
       guides(col = guide_legend(
           label.position = "top",
           keywidth = 7,
           label.theme = element_text(size = 13)
       ))
   
   ifelse(input$v_split == "Yes",
          print(curr_plot +
                    facet_wrap(~name, scales = "free")+
                    theme(legend.position = "none")
                  ),
          print(curr_plot))
       
    })
    
    ## Price/country plot
    output$v_price_search <- renderPlot({
        range <- c(input$v_random_price - 0.7, input$v_random_price + 0.7)
        big_mac %>%
            filter(year == input$v_year, dollar_price %inrange% range) %>%
            select(name,dollar_price,year) %>%
            count(name, wt = mean(dollar_price), name = "dollar_price") %>%
            mutate(diff = dollar_price - input$v_random_price) %>%
            slice_max(abs(diff),n = 16) %>% 
            mutate(name = fct_reorder(name,diff)) %>%
            ggplot(aes(diff,name, fill = name))+
            geom_col( col = "midnightblue")+
            geom_vline(xintercept = 0, col = "midnightblue")+
            scale_x_continuous(labels = scales::dollar_format())+
            labs(
                title = paste0("Countries where you could get a\nBig Mac for $",
                               input$v_random_price," (",input$v_year,")", collapse = "\n"),
                x = "price difference",
                y = NULL
            ) +
            theme_minimal()+
          theme(
            text = element_text(family = "verdana, sans-serif"),
            legend.position = "none",
            axis.text = element_text(face = "italic", size = 13),
            axis.title = element_text(face = "italic", size = 11),
            plot.title = element_text(size = 13, face = "bold")
          )
    })
    
    ## GDP Plot
    output$v_gdp <- renderPlot({
      req(input$v2_country %in% country_gdp)
      
        gdp_plot <- big_mac %>%
            select(name, year, gdp_dollar) %>%
            drop_na() %>%
            filter(name %in% input$v2_country) %>%
            ggplot(aes(year,gdp_dollar,col=name))+
            geom_line(alpha = 0.6, size = 1.4)+
            scale_y_continuous(labels = scales::dollar_format())+
            scale_x_continuous(breaks = seq(min_year,max_year,5))+
            labs(
                title = "GDP movement with time",
                x = NULL,
                y = NULL,
                col = NULL
            )+
            theme_minimal()+
            theme(
                plot.title = element_text(size = 15, face = "bold"),
                legend.position = "bottom",
                axis.text = element_text(face = "italic", size = 12),
                strip.text = element_text(face = "italic", size = 14)
            )+
            guides(col = guide_legend(
                label.position = "top",
                keywidth = 7,
                label.theme = element_text(size = 12, face = "italic")
            ))
        ifelse(input$v2_split == "Yes",
               print(gdp_plot +
                         facet_wrap(~name, scales = "free")+
                         theme(legend.position = "none")
               ),
               print(gdp_plot))
    })
    
    ## Top 10 Countries
    output$v_burger_cont <- renderPlot({
        top_bcountries_plot(yr = input$v2_year)
    })
    
    ## Currency value
    output$v_BMI_USD <- renderReactable({
      burgernomics(yr = input$v2_year, countries = input$v2_country)
    })
    
    ## Static plots
    output$v2_static_plot <- renderPlot({
      plot_dd(type = input$v2_plot_filter)
    })

    ## downloads
    ## 1
    output$v_con_price_download <- downloadHandler(
      filename = function(){paste(input$v_con_price, '.png',sep = '')},
      
      content =  function(file){
        ggsave(filename = file, width = 25,  height = 19,  units = "cm",device = "png",
          type = "cairo-png", dpi = 72)})
    ## 2
    output$v_curr_movement_download <- downloadHandler(
      filename = function(){paste(input$v_curr_movement, '.png',sep = '')},
      
      content =  function(file){
        ggsave(filename = file, width = 25,  height = 19,  units = "cm",device = "png",
               type = "cairo-png", dpi = 72)})
    ## 3
    output$v_price_search_download <- downloadHandler(
      filename = function(){paste(input$v_price_search, '.png',sep = '')},
      
      content =  function(file){
        ggsave(filename = file, width = 25,  height = 19,  units = "cm",device = "png",
               type = "cairo-png", dpi = 72)})
    ## 4
    output$v_gdp_download <- downloadHandler(
      filename = function(){paste(input$v_gdp, '.png',sep = '')},
      
      content =  function(file){
        ggsave(filename = file, width = 25,  height = 19,  units = "cm",device = "png",
               type = "cairo-png", dpi = 72)})
    ## 5 
    output$v_burger_cont_download <- downloadHandler(
      filename = function(){paste(input$v_burger_cont, '.png',sep = '')},
      
      content =  function(file){
        ggsave(filename = file, width = 25,  height = 19,  units = "cm",device = "png",
               type = "cairo-png", dpi = 72)})
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
