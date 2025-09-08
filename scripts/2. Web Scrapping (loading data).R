
#1. Web Scrapping

#1.1 Urls from 10 data chunks

urls_master <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

#son 10 chunks de datos, todos los url siguen la misma estructura del 1 al 10

#1.2 Define the function to read each table from each chunk

tables <- function(url) {
  html <- read_html(url)
  html %>%
    html_element(xpath = "(//table)[1]") %>%
    html_table(fill = TRUE)
}

#1.3 Read the 10 data chunks and store them as a list of dataframes

dataframes_chunks <- lapply(urls_master, tables)

#1.4 Append chunks into one data frame

master_data <- bind_rows(dataframes_chunks)
View(master_data)