#' Scraps mojoboxoffice.com for revenue data
#'
#' This function scraps boxofficemojo.com and downloads box office data and other info on a series of movies.
#'
#'
#' @param movies character. A string of box office mojo movie ids. You can get them from the id column of a csv extract of mojo() function.
#' @param cpi numeric. A year to adjust the inflation of the revenue data. If 1 is selected it will retrieve the estimated number of tickets as well.
#' @param ruta character. A valid path in your computer where you want to create the csv file. By default the csv named movieinfo will be created in the working directory.
#' @return It returns a csv in the specified path
#'
#' @import stringr
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom  readr write_csv
#' @importFrom readr read_csv
#' @import utils
#' @import xml2
#'
#' @export
movieinfo <- function(movies, cpi = NA, ruta = "~/movieinfo.csv") {
  start <- Sys.time()


  if (!is.na(cpi)) {
    urls <- paste0("http://www.boxofficemojo.com/movies/?page=main&id=", movies, ".htm", "&adjust_yr=", cpi,"&p=.htm")
  } else if (is.na(cpi)) {
    urls <- paste0("http://www.boxofficemojo.com/movies/?page=main&id=", movies, ".htm")
  }


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


  if (cpi == 1) {
    line <- data_frame("id", "title", "genre", "distributor", "budget", "domestic.gross", "foreign.gross", "total.gross", "est.tickets", "est.tickets.life", "first.weekend", "first.weekend.wide", "unique.release", "widest.release", "in.release", "release.date", "rating", "runtime", "cpi", "scrap.date", "url")
  } else {
    line <- data_frame("id", "title", "genre", "distributor", "budget", "domestic.gross", "foreign.gross", "total.gross", "first.weekend", "first.weekend.wide", "unique.release", "widest.release", "in.release", "release.date", "rating", "runtime", "cpi", "scrap.date", "url")
  }


  write_csv(line, append = FALSE, col_names = FALSE, path = ruta)


  n <- 1
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

    ### Estimated tickets sold if cpi is 1
    if (cpi == 1) {
      est.tickets <- x %>% read_html() %>% html_node("center font > b") %>% html_text(trim = TRUE)
      est.tickets <- str_remove_all(est.tickets, ",")
      est.tickets.life <- x %>% read_html() %>% html_node("center a") %>% html_text(trim = TRUE)

      if (str_detect(est.tickets.life, "Lifetime") == FALSE) {
        est.tickets.life <- NA
      } else {
        est.tickets.life <- str_remove_all(est.tickets.life, ",|Domestic Lifetime Est. Tickets: |Domestic Lifetime Est. Tickets:")
      }
    }



    ## First weekend
    first.weekend <- x %>% read_html() %>% html_node("td td td td .mp_box+ .mp_box table:nth-child(1) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
    first.weekend.wide <- NA


    if (first.weekend == "n/a" | is.na(first.weekend)) {
      first.weekend <- NA
    }

    ## detection of films with two releases
    if (str_detect(first.weekend, "limited") == TRUE | is.na(first.weekend)) {
      first.weekend <- x %>% read_html() %>% html_node("td td td td .mp_box+ .mp_box table:nth-child(2) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
      first.weekend.wide <- x %>% read_html() %>% html_node("table:nth-child(3) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
    }

    if (str_detect(first.weekend, "\\$") == FALSE | is.na(first.weekend)) {
      first.weekend <- NA
    }

    first.weekend <- str_remove_all(first.weekend, "\\$|,")
    first.weekend.wide <- str_remove_all(first.weekend.wide, "\\$|,")

    try(first.weekend <- as.numeric(first.weekend))
    try(first.weekend.wide <- as.numeric(first.weekend.wide))

    if (is.na(first.weekend.wide)) {
      first.weekend.wide <- first.weekend
    }

    if (first.weekend != first.weekend.wide | is.na(first.weekend.wide) | is.na(first.weekend)) {
      unique.release <- FALSE
    } else {
      unique.release <- TRUE
    }
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
    #id <- str_remove_all(url, "http://www.boxofficemojo.com/movies/\\?page=main&id=|\\.htm")  ## Creates the id
    id <- movies[n]

    ## Scrap date
    scrap.date <- Sys.Date()


    ## Creates data frame, prints it and writes line in csv


    if (cpi == 1) {
      try(line <- data_frame(id, title, genre, distributor, budget, domestic.gross, foreign.gross, total.gross, est.tickets, est.tickets.life, first.weekend, first.weekend.wide, unique.release, widest.release, in.release, release.date, rating, runtime, cpi, scrap.date, url))
    } else {
      try(line <- data_frame(id, title, genre, distributor, budget, domestic.gross, foreign.gross, total.gross, first.weekend, first.weekend.wide, unique.release, widest.release, in.release, release.date, rating, runtime, cpi, scrap.date, url))
    }

    print(line)
    try(write_csv(line, append = TRUE, col_names = FALSE, path = ruta))

    Sys.sleep(sample(1:2, 1))
    n <- n + 1
  }

  stop <- Sys.time()
  print(difftime(stop, start, units = "auto"))
}
