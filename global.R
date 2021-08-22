my_packages <- c("shiny","shinydashboard","rjson","rvest","dplyr","magrittr","DT","lubridate","purrr","ggplot2")
new_packages <- my_packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(my_packages)){
  install.packages(my_packages)
}
lapply(my_packages, require, character.only = TRUE)

gbm_loop <- function(nsim, t, mu, sigma, S0) {
  gbm <- matrix(ncol = nsim, nrow = t)
  for (s in 1:nsim) {
    gbm[1, s] <- S0
    for (d in 2:t) {
      epsilon <- rnorm(1)
      dt = 1 / t
      gbm[d, s] <-
        gbm[(d - 1), s] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  return(gbm)
}

stockprice <-
  function(ticker = NULL,
           startDate = NULL,
           endDate = NULL) {
    url <-
      paste0(
        "https://www.isyatirim.com.tr/_layouts/15/Isyatirim.WebSite/Common/Data.aspx/HisseTekil?hisse=",
        ticker,
        "&startdate=",
        format(as.Date(startDate), "%d-%m-%Y"),
        "&enddate=",
        format(as.Date(endDate), "%d-%m-%Y"),
        ".json"
      )
    
    result <- fromJSON(file = url)
    df_hist <- as.data.frame(invoke(rbind, result$value))
    
  }

url <- "https://www.isyatirim.com.tr/tr-tr/analiz/hisse/Sayfalar/Tarihsel-Fiyat-Bilgileri.aspx"
stockList <- read_html(url) %>% 
  html_nodes(xpath='//*[@class="select"]/option') %>%
  html_attr("value") %>% 
  as.data.frame() %>% 
  rename("stock"=1) %>% 
  na.omit() %>% 
  filter(!(stock %in% c("Duzeltilmis","Duzeltilmemis"))) %>% 
  pull(stock)