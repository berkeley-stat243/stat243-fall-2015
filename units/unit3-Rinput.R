##################################################
### Demo code for Unit 3 of Stat243,
### "Reading and Writing to/from R"
### Chris Paciorek, September 2015
##################################################

#####################################################
# 2: Reading data from text files into R
#####################################################

### 2.1 Core R functions

## @knitr readcsv
getwd()  # a common error is not knowing what directory R is looking at
setwd('../data')
dat <- read.table('RTADataSub.csv', sep = ',', head = TRUE)
sapply(dat, class)
levels(dat[ ,2])
dat2 <- read.table('RTADataSub.csv', sep = ',', head = TRUE,
   na.strings = c("NA", "x"), stringsAsFactors = FALSE)
unique(dat2[ ,2])
## hmmm, what happened to the blank values this time?
which(dat[ ,2] == "")
dat2[which(dat[, 2] == "")[1], ] # deconstruct it!

# using 'colClasses'
sequ <- read.table('hivSequ.csv', sep = ',', header = TRUE,
  colClasses = c('integer','integer','character',
    'character','numeric','integer'))
## let's make sure the coercion worked - sometimes R is obstinant
sapply(sequ, class)
## that made use of the fact that a data frame is a list

## @knitr readLines
dat <- readLines('../data/precip.txt')
id <- as.factor(substring(dat, 4, 11) )
year <- substring(dat, 18, 21)
year[1:5]
class(year)
year <- as.integer(substring(dat, 18, 21))
month <- as.integer(substring(dat, 22, 23))
nvalues <- as.integer(substring(dat, 28, 30))

## @knitr connections
dat <- readLines(pipe("ls -al"))
dat <- read.table(pipe("unzip dat.zip"))
dat <- read.csv(gzfile("dat.csv.gz"))
dat <- readLines("http://www.stat.berkeley.edu/~paciorek/index.html")

## @knitr curl
library(curl)
# equivalent to readLines(url("https://wikipedia.org")):
# reports that https not supported by default method:
wikip <- readLines("https://wikipedia.org")

wikip <- readLines(curl("https://wikipedia.org"))

## @knitr streaming
con <- file("../data/precip.txt", "r")
## "r" for 'read' - you can also open files for writing with "w"
## (or "a" for appending)
class(con)
blockSize <- 1000 # obviously this would be large in any real application
nLines <- 300000
for(i in 1:ceiling(nLines / blockSize)){
    lines <- readLines(con, n = blockSize)
    # manipulate the lines and store the key stuff
}
close(con)

## @knitr stream-curl
library(jsonlite)
URL <- "http://www.stat.berkeley.edu/share/paciorek/2008.csv.gz"
con <- gzcon(curl(URL, open = "r"))
# url() would work here for http too
for(i in 1:8) {
	print(i)
	print(system.time(tmp <- readLines(con, n = 100000)))
	print(tmp[1])
}
close(con)

## @knitr text-connection
dat <- readLines('../data/precip.txt')
con <- textConnection(dat[1], "r")
read.fwf(con, c(3,8,4,2,4,2))

## @knitr

### 2.2 The readr package

## @knitr readr
library(readr)
setwd('~/staff/workshops/r-bootcamp-2015/data') 
system.time(dat <- read.csv('airline.csv', stringsAsFactors = FALSE)) 
system.time(dat2 <- read_csv('airline.csv'))

## @knitr

#####################################################
# 3: Webscraping and working with XML and JSON
#####################################################

### 3.1 Reading HTML 


## @knitr https
library(XML)
library(curl)
URL <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
html <- readLines(curl(URL))
# alternative
# library(RCurl); html <- getURLContent(URL)
tbls <- readHTMLTable(html)
sapply(tbls, nrow)
pop <- readHTMLTable(html, which = 1)
head(pop)

## @knitr htmlLinks
links <- getHTMLLinks("http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year")
head(links, n = 10)

## @knitr XPath
tutors <- htmlParse("http://statistics.berkeley.edu/computing/training/tutorials")
listOfANodes <- getNodeSet(tutors, "//a[@href]")
head(listOfANodes)
sapply(listOfANodes, xmlGetAttr, "href")[1:20]
sapply(listOfANodes, xmlValue)[1:20]

## @knitr XPath2
doc <- htmlParse("http://www.nytimes.com") 
storyDivs <- getNodeSet(doc, "//h2[@class = 'story-heading']")
sapply(storyDivs, xmlValue)[1:5]

## @knitr

### 3.2 XML

## @knitr xml
doc <- xmlParse("http://api.kivaws.org/v1/loans/newest.xml")
data <- xmlToList(doc, addAttributes = FALSE)
names(data)
length(data$loans)
data$loans[[2]][c('name', 'activity', 'sector', 'location', 'loan_amount')]
# let's try to get the loan data into a data frame
loansNode <- xmlRoot(doc)[["loans"]]
loans <- xmlToDataFrame(xmlChildren(loansNode))
head(loans)
# suppose we only want the country locations of the loans
countries <- sapply(xmlChildren(loansNode), function(node) 
   xmlValue(node[['location']][['country']]))
countries[1:10]
countries <- sapply(xmlChildren(loansNode), function(node) 
   xmlValue(node$location$country)) # node is not a standard list...

## @knitr

### 3.3 Reading JSON

## @knitr json
library(jsonlite)
data <- fromJSON("http://api.kivaws.org/v1/loans/newest.json")
names(data)
class(data$loans) # nice!
head(data$loans)

## @knitr

### 3.4 Using web APIs to get data

### 3.4.1 HTTP requests

## @knitr http-get
URL <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
library(RCurl)
html <- getURLContent(URL)
tbls <- readHTMLTable(html)

## @knitr http-get2
txt <- getForm("http://ichart.finance.yahoo.com/table.csv", 
s = "AAPL", a = 2, b = 27, c = 2014, d = 7, e = 30, f = 2015,
               g = "d", ignore = ".csv")
aapl <- read.csv(textConnection(txt))
head(aapl)

## @knitr http-post
URL <- "http://somewhere.com"
txt <- postForm(URL, "start-year" = "1995", "end-year" = "2005",
                style = "post")
result <- readHTMLTable(txt, header = TRUE)

## @knitr

### 3.4.2 REST- and SOAP-based web services

## @knitr REST
times <- c(1980, 1999)
countryCode <- 'USA'
baseURL <- "http://climatedataapi.worldbank.org/climateweb/rest/v1/country"
type <- "mavg"
var <- "pr"
data <- read.csv(paste(baseURL, type, var, times[1], times[2],
                       paste0(countryCode, '.csv'), sep = '/'))
head(data)

## @knitr

#####################################################
# 4: Output from R
#####################################################

### 4.2 Formatting output

## @knitr print
val <- 1.5
cat('My value is ', val, '.\n', sep = '')
print(paste('My value is ', val, '.', sep = ''))

## @knitr cat
# input
x <- 7
n <- 5
display powers
cat("Powers of", x, "\n")
cat("exponent   result\n\n")
result <- 1
for (i in 1:n) {
	result <- result * x
	cat(format(i, width = 8), format(result, width = 10),"\n", sep = "")
}
x <- 7
n <- 5
# display powers
cat("Powers of", x, "\n")
cat("exponent result\n\n")
result <- 1
for (i in 1:n) {
	result <- result * x
	cat(i, '\t', result, '\n', sep = '')
}

## @knitr sprintf
temps <- c(12.5, 37.234324, 1342434324.79997234, 2.3456e-6, 1e10)
sprintf("%9.4f C", temps)
city <- "Boston"
sprintf("The temperature in %s was %.4f C.", city, temps[1])
sprintf("The temperature in %s was %9.4f C.", city, temps[1])

## @knitr

#####################################################
# 5: File and string encodings
#####################################################


## @knitr locale
Sys.getlocale()

## @knitr iconv
text <- "_Melhore sua seguran\xe7a_"
textUTF8 <- iconv(text, from = "latin1", to = "UTF-8")
Encoding(textUTF8)
textUTF8
iconv(text, from = "latin1", to = "ASCII", sub = "???")

## @knitr encoding
x <- "fa\xE7ile" 
Encoding(x) <- "latin1" 
x
## playing around... 
x <- "\xa1 \xa2 \xa3 \xf1 \xf2" 
Encoding(x) <- "latin1" 
x 

## @knitr encoding-error
load('../data/IPs.RData') # loads in an object named 'text'
tmp <- substring(text, 1, 15)
## the issue occurs with the 6402th element (found by trial and error):
tmp <- substring(text[1:6401],1,15)
tmp <- substring(text[1:6402],1,15)
text[6402] # note the Latin-1 character
## Interesting:
table(Encoding(text))
## Option 1
Encoding(text) <- "latin1"
tmp <- substring(text, 1, 15)
## Option 2
load('../data/IPs.RData') # loads in an object named 'text'
tmp <- substring(text, 1, 15)
text <- iconv(text, from = "latin1", to = "UTF-8")
tmp <- substring(text, 1, 15)

