library(rvest)
years <- seq(2012,2017)

year_month_indexes <- purrr::map(years, ~seq(as.numeric(substr(.x,3,4))*100 + 1,as.numeric(substr(.x,3,4))*100 + 12, by = 1)) %>% 
  unlist()

get_daily_prices <- function(year_month_indexes,index){
  
  url <-
    paste("http://www.bunkerindex.com/prices/bixfree_",
          year_month_indexes[[1]],
          ".php?priceindex_id=",
          index,
          sep = "")
  
  table <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="center"]/table[2]') %>%
    html_table(fill = TRUE)
  
  table[[1]] %>%
    slice(5:n()) %>%
    select(date = X1, price = X2) %>%
    filter(!is.na(price)) %>%
    head(-1)
}

daily_fuel_prices_380 <- purrr::map_df(year_month_indexes, get_daily_prices,2)  %>%
  mutate(fuel_index = "BIX 380 CST")

daily_fuel_prices_180 <- purrr::map_df(year_month_indexes, get_daily_prices,3)  %>%
  mutate(fuel_index = "BIX 180 CST")

bind_rows(daily_fuel_prices_380,
          daily_fuel_prices_180) %>%
  write_csv( path = "processed_data/daily_fuel_prices.csv")