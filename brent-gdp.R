#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Script tryes to find dependance b/n oil prices and other dependant variables
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(RCurl); library(XML); 
library(data.table)
library(writexl)
library(dplyr)
library(tidyverse)
library(readxl)
library(lobstr)
library(dplyr)
library(tidyr)
library(stringi)
setwd("C:/Users/INSAGNIFICANT/Downloads/R")
#-------ALL CHAPTERS-------------
#::::::::::::::::::::::::::::::::
#OIL PRICES
get.oil <- function() {
  oil.price <- read.csv2("brent/Brent Oil Futures Historical Data.csv", sep = ",")
  oil.price2.finam <- read.csv2("brent/ICE.BRN_790101_200813.csv", sep = ",")
  colnames(oil.price)[1] <- c("Date")
  Sys.setlocale("LC_ALL","English")
  prepare.oil.data <- function(oil.pr) {
    oil.pr$Date <- as.Date(levels(oil.pr$Date)[oil.pr$Date], "%b %d, %Y")
    oil.pr$Price <- as.numeric(levels(oil.pr$Price)[oil.pr$Price])
    oil.pr <- oil.pr[c(1,2)]
    colnames(oil.pr) <- c("Date", "Value")
    #average year oil price
    years <- format(oil.pr$Date, "%Y")
    oil.pr <- tapply(oil.pr$Value, years, mean)
    oil.pr <- cbind(rownames(oil.pr), oil.pr)
    oil.pr <- as.data.frame(oil.pr)
    colnames(oil.pr) <- c("Date", "Value")
    oil.pr$Date <- as.Date(paste(as.character(oil.pr$Date),"-01-01", sep = ""), "%Y-%m-%d")
    oil.pr$Value <- as.numeric(levels(oil.pr$Value)[oil.pr$Value])
    rownames(oil.pr) = NULL
    oil.pr
  }
  prepare.oil.data.finam <- function(oil.pr) {
    oil.pr$X.DATE. <- as.Date(as.character(oil.pr$X.DATE.), "%Y%m%d")
    oil.pr$X.CLOSE. <- as.numeric(levels(oil.pr$X.CLOSE.)[oil.pr$X.CLOSE.])
    oil.pr <- oil.pr[c(3,8)]
    colnames(oil.pr) <- c("Date", "Value")
    #average year oil price
    years <- format(oil.pr$Date, "%Y")
    oil.pr <- tapply(oil.pr$Value, years, mean)
    oil.pr <- cbind(rownames(oil.pr), oil.pr)
    oil.pr <- as.data.frame(oil.pr)
    colnames(oil.pr) <- c("Date", "Value")
    oil.pr$Date <- as.Date(paste(as.character(oil.pr$Date),"-01-01", sep = ""), "%Y-%m-%d")
    oil.pr$Value <- as.numeric(levels(oil.pr$Value)[oil.pr$Value])
    rownames(oil.pr) = NULL
    oil.pr
  }
  oil.price <- prepare.oil.data(oil.price)
  oil.price2 <- prepare.oil.data.finam(oil.price2.finam)
  oil.price <- merge(oil.price[oil.price$Date <= "1990-01-01",], oil.price2, all = T)
}
oil.price <- get.oil()
#GDP
  gdp <- read.csv("dat/stat/gdp_total_yearly_growth.csv")
  gdp$country <- as.character(levels(gdp$country))[gdp$country]
  country_data <- function(data, country_name, start_year, end_year) {
    if((country_name %in% data$country) == TRUE)
      countrydata <- filter(data, country == country_name)
    else
      return(paste("NO ", country_name, " IN THE COUNTRY LIST"))
    countrydata <- t(countrydata)
    countrydata <- cbind(rownames(countrydata), countrydata)
    countrydata <- as.data.frame(countrydata)
    countrydata = countrydata[-1,]
    rownames(countrydata) = NULL
    # convert factor() into numeric() 
    countrydata$V1 <- as.Date(paste(substr(countrydata$V1,2,5),'-01-01', sep = ""), "%Y-%m-%d")
    #change DATE format to YYYY-12-31 as Date
    countrydata$V2 <- as.numeric(levels(countrydata$V2))[countrydata$V2] 
    colnames(countrydata) <- c("Date", "Value")
    countrydata <- filter(countrydata, Date >= as.Date(as.character(start_year-1), "%Y") & Date <= as.Date(as.character(end_year), "%Y"))
    countrydata
  }
    rusgdp <- country_data(gdp, "Russia", 1988, 2013)
    brazilgdp <- country_data(gdp, "Brazil", 1988, 2013)
    usgdp <- country_data(gdp, "United States", 1988, 2013)
#add lost gdp data, 2014-2019 years
  lost.gdp.russia <- c(0.7, -1.97, 0.19, 1.83, 2.54, 1.34)
  lost.gdp.brazil <- c(0.51, -3.55, -3.29, 1.31, 1.31, 1.14)
  add.lost.gdp <- function(country.gdp, lost.data) {
    tmp <- as.Date(seq(from = as.Date("2014-01-01"), by = 'year', to = as.Date("2019-01-01")), "%Y-%m-%d")
    tmp <- data.frame(Date = tmp, Value = lost.data)
    country.gdp <- rbind(country.gdp, tmp)
  }
  rusgdp <- add.lost.gdp(rusgdp, lost.gdp.russia)
  brazilgdp <- add.lost.gdp(brazilgdp, lost.gdp.brazil)
#merge Oil and GDP
  merge.oil.gdp <- function(oil, country.gdp) {
    oil.gdp <- merge(oil, country.gdp, by = "Date", all = TRUE)
    colnames(oil.gdp)[c(2,3)] <- c("Oil", "GDP")
    oil.gdp <- na.omit(oil.gdp)
    oil.gdp
  }
  russia.oil.gdp <- merge.oil.gdp(oil.price, rusgdp)
  brazil.oil.gdp <- merge.oil.gdp(oil.price, brazilgdp)
#compute regressions and draw charts
  library(plotly)

draw.oil.gdp <- function(oil.gdp, country) {
  #oil.gdp <- oil.gdp[oil.gdp$Date >= '2000-01-01',]
  solution1 <- lm(GDP ~ Oil, data = oil.gdp) %>% fitted.values()
  solution2 <- lm(GDP ~ Oil, data = oil.gdp)
  oil.gdp %>%
    plot_ly(
      x = ~Oil, y = ~GDP, type = "scatter", mode = "markers"
    ) %>%
    add_text(
      text = ~format(oil.gdp$Date, "%Y"),
      textposition = "top left"
    ) %>%
    add_lines(x = ~Oil, y = solution1, mode = "lines") %>%
    layout(
      title = list(
        text = paste(country,", GDP Dependance of Oil (regression coeff. ",
                     format(solution2$coefficients[2], digits = 4),")", sep = "")
      ),
      showlegend = F
    )
}
draw.oil.gdp(russia.oil.gdp, "Russia")
draw.oil.gdp(brazil.oil.gdp, "Brazil")

#predict GDP by oil price
solution1 <- lm(GDP ~ Oil, data = russia.oil.gdp[russia.oil.gdp$Date < "2019-01-01",])
solution1 <- lm(GDP ~ Oil, data = brazil.oil.gdp)
predict.glm(solution1, data.frame(Date = "2019-01-01", Oil = c(27)))
summary(solution1)
