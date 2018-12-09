
library(plumber)

r <- plumb("E:/rprojects/esquare_v5/myfile.R")  # Where 'myfile.R' is the location of the file shown above
r$run(port=8080)

?read.csv

library(httr)
posttoaws <- POST("http://13.232.35.8:8080/known?id=1&count=7&nonce=122", body = FALSE, verbose())
postF

library(RCurl)
library(stringi)
url = "http://13.232.35.8:8080/known?id=1&count=7&nonce=122"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
class(xdata)
str_replace_all(xdata,"(\)" , "")

gsub
stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')


