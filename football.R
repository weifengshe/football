## require the data
library(XML)
library(dplyr)
## url for running back
urlRB <- "http://games.espn.go.com/ffl/leaders?&slotCategoryId=2&seasonTotals=true&seasonId=2014&startIndex="
## url for quarter back
urlQB <- "http://games.espn.go.com/ffl/leaders?&slotCategoryId=0&seasonTotals=true&seasonId=2014&startIndex="
## url for wide receiver
urlWR <- "http://games.espn.go.com/ffl/leaders?&slotCategoryId=4&seasonTotals=true&seasonId=2014&startIndex="
## url for tight end
urlTE <- "http://games.espn.go.com/ffl/leaders?&slotCategoryId=6&seasonTotals=true&seasonId=2014&startIndex="
index = c(0, 50, 100)

## scrapping the data from web
## function to scrap the data from the webpage
getData = function(stats, url, index, names, selectedCol){
        for (i in index){
                urlto = paste(url, as.character(i), sep = "")
                tablefromURL <- readHTMLTable(urlto)
                table <- tablefromURL$playertable_0
                addto <- table[2:51, selectedCol]
                colnames(addto) <- names
                stats <- rbind(stats, addto)
        }
        return(stats)
}

## scrapping the data for running back
rbstats <- as.data.frame(matrix(ncol = 5))
colnamesRB <- c("player", "rush", "yards", "touchdown", "points")
colnames(rbstats) <- colnamesRB
selectedColRB <- c(1, 8:10, 21)

rbstats = getData(rbstats, urlRB, index, colnamesRB, selectedColRB)

## scrapping the data for quarter back
qbstats <- as.data.frame(matrix(ncol = 6))
colnamesQB <- c("player", "compAttam", "yards", "touchdown", "intercept", "points")
colnames(qbstats) <- colnamesQB
selectedColQB <- c(1, 3:6, 21)
qbstats <- getData(qbstats, urlQB, index, colnamesQB, selectedColQB)

## scrapping the data for wide receiver
wrstats <- as.data.frame(matrix(ncol = 6))
colnamesWR <- c("player", "received", "yards", "touchdown", "targeted", "points")
colnames(wrstats) <- colnamesWR
selectedColWR <- c(1, 12:15, 21)
wrstats <- getData(wrstats, urlWR, index, colnamesWR, selectedColWR)

## scrapping the data for tight end
testats <- as.data.frame(matrix(ncol = 6))
colnamesTE <- c("player", "received", "yards", "touchdown", "targeted", "points")
colnames(testats) <- colnamesTE
selectedColTE <- c(1, 12:15, 21)
testats <- getData(testats, urlTE, index, colnamesTE, selectedColTE)


## further check the data and clean the data
## for first conlumn, only leave the players names
rbstats <- tbl_df(rbstats)
rbstats <- rbstats[2:151, ]
rbstats[, 2:5] <- sapply(rbstats[, 2:5], as.numeric)
rbstats <- filter(rbstats, rush != "NA",  rush > 10)
## clean the name of player
extractName <- function(x){
         return(strsplit(x, ",")[[1]][1])
}
rbstats$player <- unlist(lapply(rbstats$player, extractName))
head(rbstats)
## converting columns of rush, yards, touchdown and points to numeric

rbstats <- mutate(rbstats, average = yards/rush)
dim(rbstats)
head(rbstats)
with(rbstats, hist(yards, breaks = 20))

##cleaning quarter back data
## for first conlumn, only leave the players names
qbstats <- tbl_df(qbstats)
## remove the first row of NA
qbstats <- qbstats[2:151, ]
qbstats <- filter(qbstats, yards != "NA")
## clean the player column
qbstats$player <- unlist(lapply(qbstats$player, extractName))
qbstats[, 3:6] <- sapply(qbstats[, 3:6], as.numeric)
#qbstats <- filter(qbstats, rush != "NA",  rush > 10)
## 
extractComp <- function(x){
        return(strsplit(x, "/")[[1]][1])
}

extractAttam <- function(x){
        return(strsplit(x, "/")[[1]][2])
}


qbstats$complete <- unlist(lapply(qbstats$compAttam, extractComp))
qbstats$attam <- unlist(lapply(qbstats$compAttam, extractAttam))
## clean the name of player
## delete the CompAttam column
qbstats$compAttam <- NULL
## create two more variables
qbstats[, 6:7] <- sapply(qbstats[, 6:7], as.numeric)
qbstats <- mutate(qbstats, percent = 100 * complete / attam, average = yards / attam)
qbstats <- filter(qbstats, complete > 10)
head(qbstats)
dim(qbstats)
## converting columns of rush, yards, touchdown and points to numeric

## further clean wide reseiver's data
wrstats <- tbl_df(wrstats)
wrstats <- wrstats[2:151, ]
wrstats$player <- unlist(lapply(wrstats$player, extractName))
wrstats[, 2:6] <- sapply(wrstats[, 2:6], as.numeric)
wrstats <- filter(wrstats, received > 10)
wrstats <- mutate(wrstats, average = yards / received)

head(wrstats)
## further clean tight end's data
testats <- tbl_df(testats)
testats <- testats[2:151, ]
testats$player <- unlist(lapply(testats$player, extractName))
testats[, 2:6] <- sapply(testats[, 2:6], as.numeric)

testats <- mutate(testats, average = yards / received)
testats <- filter(testats, received > 10)
testats


## save the data into .RData
save(qbstats, rbstats, wrstats, testats, file = "stats.RData")
