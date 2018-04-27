#' Scraps mojoboxoffice.com for revenue data
#'
#' This function scraps boxofficemojo.com and downloads rank, lifetime gross box office data, year, and id from the domestic ranking: 'http://www.boxofficemojo.com/alltime/domestic.htm'.
#'
#' @param pages numeric. Number of pages you want to scrap from box office mojo's domestic ranking.
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
domesticRank <- function(pages, ruta = "~/Domestic_rank.csv") {

  start <- Sys.time()

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


  p <- 1:pages
  urls <- paste0("http://www.boxofficemojo.com/alltime/domestic.htm?page=", p, "&p=.htm")



  line <- data_frame("rank", "title", "year", "lifetime.gross", "link", "id", "multiple.releases")
  write_csv(line, append = FALSE, col_names = FALSE, path = ruta)

  for (url in urls) {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))

    rank <- x %>% read_html() %>% html_nodes("center td tr+ tr td:nth-child(1) font") %>% html_text()

    link <- x %>% read_html() %>% html_nodes("tr+ tr a") %>% html_attr(name = "href")
    link <- link[str_detect(link, "movies")]
    link <- paste0("http://www.boxofficemojo.com", link)

    title <- x %>% read_html() %>% html_nodes("tr+ tr a b") %>% html_text()
    title <-title[-c(1,2)]

    lifetime.gross <- x %>% read_html() %>% html_nodes("tr+ tr td~ td+ td b") %>% html_text()
    lifetime.gross <- lifetime.gross[-1]
    lifetime.gross <- str_remove_all(lifetime.gross, "\\$|,")

    year <- x %>% read_html() %>% html_nodes("tr+ tr td:nth-child(5) font") %>% html_text()
    year <- year[-1]

    id <- str_remove_all(link, "http://www.boxofficemojo.com/movies/\\?id=|\\.htm|http://www\\.boxofficemojo\\.com/movies/\\?page=releases&id=")

    multiple.releases <- FALSE

    try(line <- data_frame(rank, title, year, lifetime.gross, link, id, multiple.releases))

    try(line$multiple.releases[str_detect(line$year, "\\^") == TRUE] <- TRUE)
    try(line$year <- str_remove(line$year, "\\^"))

    print(line)
    try(write_csv(line, append = TRUE, col_names = FALSE, path = ruta))

    Sys.sleep(sample(1:2, 1))
  }

  stop <- Sys.time()
  print(difftime(stop, start,units = "auto"))

}
