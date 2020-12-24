## Helper Functions
library(tidyverse)

big_mac <- read_csv("big_mac_edited.csv")

options(scipen = 999, digits = 3)


big_mac <-  big_mac %>%
  mutate(year = year(date)) %>%
  mutate(month = lubridate::month(date, label = T))

all_countries <- 
  big_mac %>%
  select(name) %>%
  distinct() %>%
  drop_na() %>%
  arrange(name) %>%
  pull(name)


country_gdp <- big_mac %>%
  select(name, gdp_dollar) %>%
  drop_na() %>%
  distinct(name) %>%
  arrange(name) %>%
  pull(name)

plot_dd <- function(type){
  type = as.integer(type)
  
  ifelse(type == 0, return(top_bottom_average_price),
         ifelse(type == 1, return(top_average_price), return(least_average_price)))
}
 

top_bcountries_plot <- function(yr){
  
  BMI_value <-
    big_mac %>%
    filter(year == yr) %>%
    select(year,month,name,currency_code,local_price,dollar_ex,USD_price) %>%
    mutate(BMI_cur_usd = local_price/USD_price) %>%
    mutate(compare_BMI_USD = round(BMI_cur_usd - dollar_ex,3)) %>%
    mutate(perc = round((abs(compare_BMI_USD)/dollar_ex),2)) %>%
    filter(name != "United States") %>%
    mutate(uv_ov = ifelse(BMI_cur_usd > dollar_ex,"under valued","over valued")) %>%
    mutate(final = str_c(abs(compare_BMI_USD),currency_code,sep = " ")) %>%
    select(name,compare_BMI_USD,perc,uv_ov,currency_code,final) 
  
  top_bottom <- bind_rows(
    BMI_value %>%
      slice_max(n = 5,order_by = compare_BMI_USD),
    
    BMI_value %>%
      slice_min(n = 5,order_by = compare_BMI_USD)) 
  
  plot_countries <- 
    top_bottom %>%
    ggplot(aes(perc,reorder(name,perc),fill =uv_ov, label = final))+
    geom_col()+
    scale_x_continuous(labels = scales::percent_format())+
    ggrepel::geom_label_repel(show.legend = F)+
    theme_minimal()+
    labs(
      title = "Top valued Currencies",
      subtitle = paste0("against the USD for the year ",yr, collapse = "\n"),
      y = NULL,
      x = NULL
    )+
    scale_fill_manual(values = c("#EF6F6C","#68A691"))+
    theme(
      plot.background = element_rect(fill = "#FDFFFC", colour = "#FDFFFC"),
      plot.title = element_text(face = "bold", size = 13, hjust = 0),
      legend.position = "bottom",
      axis.text = element_text(face = "italic", size = 11),
      strip.text = element_blank(),
      legend.title = element_blank()
    )+
    guides(fill = guide_legend(
      label.position = "top",
      keywidth = 9,
      keyheight = 0.4,
      label.theme = element_text(face = "italic",size = 13)
    ))
  
  print(plot_countries)
}



burgernomics <- function(yr,countries){
  
  country_summ <- big_mac %>%
    mutate(year = year(date)) %>%
    mutate(month = lubridate::month(date, label = T)) %>%
    filter(year == yr, name %in% countries) %>%
    select(year,month,name,currency_code,local_price,dollar_ex,USD_price) %>%
    mutate(BMI_cur_usd = local_price/USD_price) %>%
    mutate(compare_BMI_USD = round(BMI_cur_usd - dollar_ex,3)) %>%
    mutate(perc = round((abs(compare_BMI_USD)/dollar_ex)*100,2)) %>%
    mutate(final_pred = ifelse(
      test = BMI_cur_usd > dollar_ex,
      
      yes = paste0("In the year ",year," (",month,")",". The ",
                   currency_code," was undervalued by ",
                   abs(compare_BMI_USD)," ",currency_code," per U.S. dollar ","(",perc,"%)"),
       
      no =  paste0("In the year ",year," (",month,")",". The ",
                   currency_code," was overvalued by ",
                   abs(compare_BMI_USD)," ",currency_code," per U.S. dollar ","(",perc,"%)")
    )) %>%
    filter(name != "United States") %>%
    select(Country = name, Summary = final_pred) %>%
    reactable::reactable(
      columns = list(
        Country = colDef(align = "center",
                         sortable = TRUE,
                         minWidth = 50),
        Summary = colDef(align = "left")
      ),
      pagination = TRUE,
      defaultPageSize = 6,
      minRows = 6,
      language = reactableLang(
        noData = "Please Select Country or another year"
      )
    )
  
  return(country_summ)
  
}


## Static plots

top_bottom_average_price <- 
  big_mac %>%
  count(name, wt = round(mean(dollar_price, na.rm = T),2), sort = T) %>%
  slice_head(n = 7) %>%
  bind_rows(big_mac %>%
              count(name, wt =  round(mean(dollar_price, na.rm = T),2), sort = T) %>%
              slice_tail(n = 7)) %>%
  ggplot(aes(reorder(name,n),n, fill = name)) +
  geom_col(show.legend = F) +
  scale_y_continuous(labels = scales::dollar_format())+
  coord_flip()+
  labs(title = "Countries and their average price for Big Mac",
       subtitle = "Top Six vs Bottom Six",
       y = NULL,
       x = NULL)+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "italic", size = 12)
  )


top_average_price <- 
  big_mac %>%
  count(name, wt = round(mean(dollar_price, na.rm = T),2), sort = T) %>%
  slice_head(n = 10) %>%
  ggplot(aes(reorder(name,n),n, fill = name)) +
  geom_col(show.legend = F) +
  scale_y_continuous(labels = scales::dollar_format())+
  coord_flip()+
  labs(title = "Countries and their average price for Big Mac",
       subtitle = "Top 10",
       y = NULL,
       x = NULL)+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "italic", size = 12)
  )


least_average_price <-
  big_mac %>%
  count(name, wt = round(mean(dollar_price, na.rm = T),2), sort = T) %>%
  slice_tail(n = 10) %>%
  ggplot(aes(reorder(name,-n),n, fill = name)) +
  geom_col(show.legend = F) +
  scale_y_continuous(labels = scales::dollar_format())+
  coord_flip()+
  labs(title = "Countries and their average price for Big Mac",
       subtitle = "Bottom 10",
       y = NULL,
       x = NULL)+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "italic", size = 12)
  )





