#' Scraps mojoboxoffice.com for revenue data
#'
#' This function scraps mojoboxoffice.com and downloads box office data and other info on a series of movies.
#'
#'
#' @param movies character. A string of mojo box office movie ids. You can get them from the id column of a csv extract of mojo() function.
#' @param ruta character. A valid path in your computer where you want to create the csv file. By default the csv named movieinfo will be created in the working directory.
#' @return It returns a csv in the specified path
#'
#' @import stringr
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom dplyr data_frame
#' @importFrom  readr write_csv
#' @importFrom readr read_csv
#' @import utils
#' @import xml2
#'
#' @export
movieinfo <- function(movies, ruta = "~/movieinfo.csv") {

  start <- Sys.time()

  urls <- paste0("http://www.boxofficemojo.com/movies/?page=main&id=", movies, ".htm")



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

  urls <- unique(urls)

  line <- data_frame("id", "title", "genre", "distributor", "budget", "domestic.gross", "foreign.gross", "total.gross", "first.weekend", "widest.release", "in.release", "release.date", "rating", "runtime", "scrap.date", "url")
  write_csv(line, append = FALSE, col_names = FALSE, path = ruta)

  for (url in urls) {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))

    ## Film title
    title <- x %>% read_html() %>% html_node("br+ font b") %>% html_text(trim = TRUE)

    ## Domestic gross
    domestic.gross <- x %>% read_html() %>% html_node("td td td td tr:nth-child(1) td:nth-child(2) b") %>% html_text(trim = TRUE)
    domestic.gross <- str_remove_all(domestic.gross, "\\$|,")
    try(domestic.gross <- as.numeric(domestic.gross))
    ####


    ## Foreign gross
    foreign.gross <- x %>% read_html() %>% html_node("td td td td:nth-child(1) tr:nth-child(2) td:nth-child(2)") %>% html_text(trim = TRUE)
    foreign.gross <- str_remove_all(foreign.gross, "\\$|,")

    if (foreign.gross == "n/a" | is.na(foreign.gross)) {
      foreign.gross <- NA
    }

    try(foreign.gross <- as.numeric(foreign.gross))
    ####


    ## Total gross
    total.gross <- x %>% read_html() %>% html_node("td td td td tr+ tr td+ td b") %>% html_text(trim = TRUE)
    total.gross <- str_remove_all(total.gross, "\\$|,")

    if (total.gross == "n/a" | is.na(total.gross)) {
      total.gross <- NA
    }

    try(total.gross <- as.numeric(total.gross))
    ####


    ## First weekend
    first.weekend <- x %>% read_html() %>% html_node("td td td td .mp_box+ .mp_box table:nth-child(1) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)

    if (first.weekend == "n/a" | is.na(first.weekend)) {
      first.weekend <- NA
    }

    if (str_detect(first.weekend, "limited") == TRUE | is.na(first.weekend)) {
      first.weekend <- x %>% read_html() %>% html_node("td td td td .mp_box+ .mp_box table:nth-child(2) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
    }

    if (str_detect(first.weekend, "\\$") == FALSE | is.na(first.weekend)) {
      first.weekend <- NA
    }

    first.weekend <- str_remove_all(first.weekend, "\\$|,")
    try(first.weekend <- as.numeric(first.weekend))
    ###


    ## Number of theater in widest release
    widest.release <- x %>% read_html() %>% html_node(".mp_box_content table:nth-child(2) td+ td") %>% html_text(trim = TRUE)

    if (widest.release == "n/a" | is.na(widest.release)) {
      widest.release <- NA
    }

    if (str_detect(widest.release, "theaters") == FALSE | is.na(widest.release)) {
      widest.release <- x %>% read_html() %>% html_node("table:nth-child(4) td+ td") %>% html_text(trim = TRUE)
    }

    widest.release <- str_remove_all(widest.release, ",|theaters")
    try(widest.release <- as.numeric(widest.release))
    ####


    ## Number of days in theaters
    in.release <- x %>% read_html() %>% html_node("table~ table+ table td+ td") %>% html_text(trim = TRUE)

    if (is.na(in.release)) {
      in.release <- x %>% read_html() %>% html_node("td td td td .mp_box+ .mp_box table:nth-child(2) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
    } else if (str_detect(in.release, "days|day") == FALSE) {
      in.release <- x %>% read_html() %>% html_node("table:nth-child(4) td+ td") %>% html_text(trim = TRUE)
    }

    if (str_detect(in.release, "days|day") == FALSE | is.na(in.release)) {
      in.release <- x %>% read_html() %>% html_node(".mp_box_content table:nth-child(5) td+ td") %>% html_text(trim = TRUE)
    }

    if (str_detect(in.release, "days|day") == FALSE | is.na(in.release)) {
      in.release <- x %>% read_html() %>% html_node(".mp_box_content table:nth-child(6) td+ td") %>% html_text(trim = TRUE)
    }

    if (str_detect(in.release, "days|day") == FALSE | is.na(in.release)) {
      in.release <- NA
    }

    try(in.release <- str_split(in.release, "/", n = 2, simplify = TRUE)[1])
    in.release <- str_remove_all(in.release, "days")
    try(in.release <- as.numeric(in.release))

    try(in.release <- str_split(in.release, "days", n = 2, simplify = TRUE)[1])
    try(in.release <- as.numeric(in.release))
    ####


    ## Distributor
    distributor <- x %>% read_html() %>% html_node("b > a") %>% html_text(trim = TRUE)

    ## Release date
    release.date <- x %>% read_html() %>% html_node("nobr a") %>% html_text(trim = TRUE)   ## Scrap release date and format to Date class variable

    if (is.na(release.date)) {
      release.date <- x %>% read_html() %>% html_node("nobr") %>% html_text(trim = TRUE)
    }

    dates <- str_split(release.date, " |,", n = 3, simplify = TRUE)
    try(day <- dates[,2])
    try(months <- match(dates[,1], month.name))
    try(year <- dates[,3])
    try(release.date <- paste(year, months, day, sep = "-"))
    try(release.date <- as.Date(release.date))
    #####

    ## Rating
    rating <- x %>% read_html() %>% html_node("center tr:nth-child(4) td:nth-child(1) b") %>% html_text(trim = TRUE)

    ## Budget
    budget <- x %>% read_html() %>% html_node("center tr:nth-child(4) td+ td b") %>% html_text(trim = TRUE)

    if (budget == "N/A" | is.na(budget)) {
      budget <- NA
    } else if (budget != "N/A") {
      budget <- str_remove_all(budget, "\\$")
      try(budget <- str_replace(budget, " million| Million", "000000"))
      try(budget <- as.numeric(budget))
    }

    ## Runtime
    try(runtime <- x %>% read_html() %>% html_node("center tr:nth-child(3) td+ td b") %>% html_text(trim = TRUE))
    runtime <- str_remove_all(runtime, "hrs\\.|min\\.")
    try(runtime <- str_replace(runtime, "  ", ":"))
    try(runtime <- as.difftime(runtime, "%H:%M", units = "mins"))
    try(runtime <- as.numeric(runtime))

    ## Genre
    genre <- x %>% read_html() %>% html_nodes("center tr:nth-child(3) td:nth-child(1) b") %>% html_text(trim = TRUE)

    ## Film id
    id <- str_remove_all(url, "http://www.boxofficemojo.com/movies/\\?page=main&id=|\\.htm")  ## Creates the id

    ## Scrap date
    scrap.date <- Sys.Date()


    ## Creates data frame, prints it and writes line in csv
    try(line <- data_frame(id, title, genre, distributor, budget, domestic.gross, foreign.gross, total.gross, first.weekend, widest.release, in.release, release.date, rating, runtime, scrap.date, url))
    print(line)
    try(write_csv(line, append = TRUE, col_names = FALSE, path = ruta))

    Sys.sleep(sample(1:3, 1))
  }

  stop <- Sys.time()
  print(difftime(stop, start, units = "auto"))

}
