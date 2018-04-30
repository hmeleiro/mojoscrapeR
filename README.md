# mojoscrapeR

MojoscrapeR is a web scraper for [mojoboxoffice.com](http://www.boxofficemojo.com/). It easily downloads revenue data on films.


# Instalation

Type ```devtools::install_github("meneos/mojoscrapeR")```in your R console.

# How to use mojoscrapeR

At the moment mojoscrapeR is composed of three functions: one to scrap box office data of each day given a time interval, another one to extract information from a set of movies given a string of film ids, and a third one to scrap the domestic ranking of [homeboxmojo.com](http://www.boxofficemojo.com/alltime/domestic.htm).

## mojo()

Given a time interval ```mojo()``` function will scrap daily box office data. The ruta argument is used to specify a path in your computer where you want to create the csv file.

##### For example: 
```mojo(from = "2018-01-31", to = "2018-31-01")``` 

## movieinfo() 

Given a string of movie ids ```movieinfo()```will scrap info for each film. The simplest way of getting a set of movie ids is using the id column of the csv resultant of mojo() or domesticRank() functions. The ruta argument is used to specify a path in your computer where you want to create the csv file.

##### For example:

```
# Creates a string variable with all the star wars films id's
starwars <-c("starwars8","starwars2016","starwars7","starwars3","starwars2","starwars","starwars6","starwars5","starwars4","starwarsepisodeix")

# scrap scrap
movieinfo(starwars)
```

or:

```
# Add the resultant csv of scraping with mojo function
data <- read.csv(".../mojo.csv")

#scrap scrap
movieinfo(data$id)
```

## domesticRank()

With this function you can create a summary csv of the [domestic ranking](http://www.boxofficemojo.com/alltime/domestic.htm). The only mandatory argument of the function is the number of pages you want to scrap. The ruta argument is used to specify a path in your computer where you want to create the csv file.

##### For example:

```
# This will scrap the first ten pages of the domestic ranking (1.000 titles)
domesticRank(10)
```
