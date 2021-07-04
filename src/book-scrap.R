rm(list = ls())
# install.packages("remotes")
# remotes::install_github("fsingletonthorn/words_to_numbers")

if(!require(pacman)){install.packages("pacman")}
require(pacman)
p_load(tidyverse, rvest, wordstonumbers, here, progress)

# Funciones de extracción

get_books <- function(url){
  
  books_cards <- read_html(url) %>%
    html_elements("article")
  
  books_info <- map_df(books_cards, get_article_data)
  
  return(books_info)
}

get_article_data <- function(article){
  title <- article %>% 
    html_element("h3 a") %>% 
    html_attr("title") 
  
  price <- article %>% 
    html_elements("p.price_color") %>% 
    html_text2()
  
  star <- article %>%
    html_elements("p.star-rating") %>%
    html_attr("class") %>%
    str_remove(., "star-rating ") %>%
    map_chr(., words_to_numbers)
  
  link <- article %>%
    html_elements("h3 a") %>%
    html_attr("href")
  
  if(exists("link")){
    desc_url <- paste0("https://books.toscrape.com/catalogue/", link)
    
    more_info <- read_html(desc_url) 
    
    description <- more_info%>% 
      html_element("#product_description + p") %>% 
      html_text2()
    
    genre <- more_info %>% 
      html_element(".breadcrumb li:nth-child(3) a") %>% 
      html_text2()
    
  } else {"no hay link"}
  
  values <- list(title = title, price = price, star = star, link = link, description = description, genre = genre) %>% 
    map(., ~ifelse(is.null(.x) | is_empty(.x), NA, .x)) %>% 
    unlist() 
  
  return(values)
}

# Loop (podríamos usar un map_df pero los progress bar no están tan cool todavía)

urls <- c(paste0("https://books.toscrape.com/catalogue/page-", as.character(c(1:50)), ".html"))
base_libros <- tibble()

pb <- txtProgressBar(min = 0, max = length(urls), style = 3) 

for(i in 1:length(urls)){
  # Sys.sleep(sample(1:3, 1))
  base_libros <- get_books(urls[i]) %>% 
    bind_rows(base_libros)
  
  setTxtProgressBar(pb, i)
}
rm(pb, i, urls)
  
# Guardamos base 

saveRDS(base_libros, here("output/scrap-data.rds"))


  