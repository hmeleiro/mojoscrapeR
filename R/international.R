#' Scraps mojoboxoffice.com for revenue data
#'
#' This function scraps boxofficemojo.com and downloads rank, lifetime gross box office data, year, and id from the domestic ranking: 'http://www.boxofficemojo.com/alltime/domestic.htm'.
#'
#' @param ctry character. Country from which you want to scrap revenue data. Make sure you type it the exact same way as the url. For example: http://www.boxofficemojo.com/intl/australia/?yr=2018&wk=23&p=.htm
#' @param wk integer. A numeric value or vector that indicates which weeks of the year you want to scrap. For example: 34 will scrap the 34th week. 1:52 will scrap the 52 weeks of the given year.
#' @param yr integer.A numeric value or vector that indicates which year you want to scrap.
#' @param path character. A valid path in your computer where you want to create the csv file.
#' @param serverHelp logical. Indicates if you want mojoscRaper to initiate a Docker server or you will initiate it youself. TRUE by default. Remember that this function NEEDS a Docker server running
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
#' @import RSelenium
#'
international <- function(ctry, wk, yr, path, serverHelp = TRUE) {

  # Runs Docker server (Docker must be online)
  if (serverHelp == TRUE) {
    port <- sample(4441:4447, 1)

    system("docker pull selenium/standalone-firefox")  #

    Sys.sleep(1)

    system(paste0("docker run -d -p ", port,":4444 selenium/standalone-firefox"))

    Sys.sleep(2)

    remDr <- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = "firefox")

    Sys.sleep(1)

    remDr$open()

    Sys.sleep(1)

    remDr$setTimeout(milliseconds = 3000)
  }



  start <- Sys.time()

  urls <- paste0("http://www.boxofficemojo.com/intl/", ctry,"/?yr=", yr, "&wk=", wk,"&currency=local&p=.htm")

  x <- data.frame("TW", "LW", "Movie","Studio","Weekend Gross", "Change", "Theaters", "Change in theaters", "Avg.", "Gross-to-Date", "Week", "Week.data", "Year.data", "link", "id")
  readr::write_csv(x, path = path, append = FALSE, col_names = FALSE)


  for (url in urls) {
    remDr$navigate(url)

    x <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes("table") %>% html_table(header = TRUE, fill = TRUE)

    links <- remDr$findElements(using = "css selector", value =  "td tr+ tr a")
    links <- unlist(lapply(links, function(x){x$getElementAttribute(attrName = "href")}))

    x <- as.data.frame(x[[6]])

    colnames(x)[8] <- "Change in theaters"
    colnames(x)[9] <- "Avg."

    x$`Weekend Gross` <- try(as.numeric(str_remove_all(x$`Weekend Gross`, ",|\u20ac")), silent = TRUE)
    x$`Change in theaters` <- try(as.numeric(str_remove_all(x$`Change in theaters`, "-")), silent = TRUE)
    x$Avg. <- try(as.numeric(str_remove_all(x$Avg., ",|\u20ac")), silent = TRUE)
    x$`Gross-to-Date` <- try(as.numeric(str_remove_all(x$`Gross-to-Date`, ",|\u20ac")), silent = TRUE)
    x$Change <- try(as.numeric(str_remove_all(x$Change, "%|-|,")), silent = TRUE)

    x$Week <- as.numeric(x$Week)

    x$Theaters <- str_remove_all(x$Theaters, "n/a")


    ## Extract week, year and id from urls
    k <- str_locate(url, "wk")[2]
    k2 <- k + 3
    weeklocation <- substr(url, k, k2)
    x$Week.data <- try(as.numeric(str_remove_all(weeklocation, "k|=|&")), silent = TRUE)

    r <- str_locate(url, "yr")[2]
    r2 <- r + 6
    yearlocation <- substr(url, r, r2)
    x$Year.data <- try(as.numeric(str_remove_all(yearlocation, "r|=|&")), silent = TRUE)

    try(x$links <- links)

    d <- str_locate(x$links, "id")[,2]
    d2 <- nchar(x$links)
    idlocation <- substr(x$links, d, d2)
    x$id <- str_remove_all(idlocation, "d=|\\.htm")

    x$id[str_detect(x$id, "&")] <- substr(x$id[str_detect(x$id, "&")], 1, str_locate(x$id[str_detect(x$id, "&")], "&")-1)

    print(x)

    readr::write_csv(x, path = path, append = TRUE, col_names = FALSE)
  }


  end <- Sys.time()
  print(difftime(end, start, units = "auto"))

  df <- readr::read_csv(path)
  mojo <<- df
}
