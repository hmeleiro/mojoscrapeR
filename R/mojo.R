#' Scraps mojoboxoffice.com for revenue data
#'
#' This function scraps boxofficemojo.com and downloads box office data of each day in a given interval.
#'
#'
#' @param from character. A date in YYYY-MM-DD format.
#' @param to character. A date in YYYY-MM-DD format.
#' @param ruta character. A valid path in your computer where you want to create the csv file. By default the csv named mojo will be created in the working directory.
#' @return It returns a csv in the specified path
#'
#' @import stringr
#' @import rvest
#' @import httr
#' @importFrom  readr write_csv
#' @importFrom readr read_csv
#' @import utils
#' @import dplyr
#' @import xml2
#'
#' @export
mojo <- function(from , to, ruta = "~/mojo.csv") {

  start <- Sys.time()


  from <- as.Date(from)    ## Me aseguro de que los argumentos from y to sean de clase Date.
  to <- as.Date(to)

  days <- seq.Date(from, to, 1)  # Genero las fechas del intervalo.

  urls <- paste0("http://www.boxofficemojo.com/daily/chart/?view=1day&sortdate=", days, "&p=.htm")

  line <- data_frame("id", "movie", "studio", "days.in.theatres", "daily.gross", "gross.to.date", "theatres", "date", "url")
  write_csv(line, append = FALSE, col_names = FALSE, path = ruta)


  desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')


  for (url in urls) {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    titles <- x %>% read_html() %>% html_nodes("td td font a b") %>% html_text()
    links <- x %>% read_html() %>% html_nodes("td tr td font a") %>% html_attr(name = "href")
    links <- links[str_detect(links, "movies")]
    links <- paste0("http://www.boxofficemojo.com", links)


    studio <- x %>% read_html() %>% html_nodes("tr+ tr td:nth-child(4) font a") %>% html_text()
    studio <- studio[-1]

    dailygross <- x %>% read_html() %>% html_nodes("center td font > b") %>% html_text()
    dailygross <- str_remove_all(dailygross, "\\$|,")
    try(dailygross <- as.numeric(dailygross))

    gross.to.date <- x %>% read_html() %>% html_nodes("td:nth-child(10) font") %>% html_text()
    gross.to.date <- str_remove_all(gross.to.date, "\\$|,")
    try(gross.to.date <- as.numeric(gross.to.date))

    theatres <- x %>% read_html() %>% html_nodes("tr+ tr td:nth-child(8) font") %>% html_text()
    theatres <- theatres[-1]
    theatres <- str_remove_all(theatres, ",")

    days <- x %>% read_html() %>% html_nodes("td:nth-child(11) font") %>% html_text()
    try(days <- as.numeric(days))

    date <- str_remove_all(url, "http://www.boxofficemojo.com/daily/chart/\\?view=1day&sortdate=|&p=.htm")

    date <- as.Date(date)

    id <- str_remove_all(links, "http://www.boxofficemojo.com/movies/\\?page=daily&id=|\\.htm")

    line <- data_frame(id, titles, studio, days, dailygross, gross.to.date, theatres, date, links)
    print(line)
    write_csv(line, append = TRUE, col_names = FALSE, path = ruta)

  }

  stop <- Sys.time()
  print(difftime(stop, start, units = "auto"))

}
