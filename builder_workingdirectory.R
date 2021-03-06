rm(list=ls())


#content of first file and building first file 

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

Sys.setenv(wd = "C:/Users/user/Desktop/Tally/ESA_Attributes")
currentdirectory <-  Sys.getenv(x = "wd", unset = "", names = NA)
setwd(currentdirectory)
getwd()

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file1.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/testencrptanddecrpt.txt"

# filesourcepath <- paste(getwd(), "/file1.txt", sep = "") 
# filepath <-  paste(getwd(), "/01.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <- "http://13.232.35.8:8080/known?id=1&count=1&nonce=a"
url <- paste(url, noncechar , sep = "" )
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=FALSE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)


filepathofencryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/01.txt"
filepathofdecryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file1.txt"

# filepathofencryptedfile <- paste(getwd(), "/02.txt", sep = "" ) 
# filepathofdecryptedfile <- paste(getwd(), "/file2.txt", sep = "") 


key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))
url <- "http://13.232.35.8:8080/known?id=240433&count=1&nonce=a"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
noncechar <- stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')[[1]]
nonce <- charToRaw(noncechar)

decryptandrun <- function(filepathofencryptedfile, filepathofdecryptedfile, key, nonce){
  dataencrypted <- read_file_raw(filepathofencryptedfile)
  dataafterdecrypt <-  unserialize(data_decrypt(dataencrypted, key = key, nonce = nonce))
  dataafterdecrypt <- as.vector(dataafterdecrypt)
  write(dataafterdecrypt, filepathofdecryptedfile)
  source(filepathofdecryptedfile, local = TRUE)
}


decryptandrun(filepathofencryptedfile, filepathofdecryptedfile, key, nonce)


################when building for first time run only the encrypt functions

#1

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

Sys.setenv(wd = "C:/Users/user/Desktop/Tally/ESA_Attributes")
Sys.setenv(id = 240433)
Sys.setenv(tallyurl = "http://localhost:9001")
currentdirectory <-  Sys.getenv(x = "wd", unset = "", names = NA)
id <-  Sys.getenv(x = "id", unset = "", names = NA)
tallyurl <-  Sys.getenv(x = "tallyurl", unset = "", names = NA)

setwd(currentdirectory)
getwd()

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file1.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/01.txt"

filesourcepath <- paste(getwd(), "/file1.txt", sep = "") 
filepath <-  paste(getwd(), "/01.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=1&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)

#2

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file2.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/02.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file2.txt", sep = "") 
filepath <-  paste(getwd(), "/02.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=2&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)


#3

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file3.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/03.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file3.txt", sep = "") 
filepath <-  paste(getwd(), "/03.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=3&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)

#4

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file4.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/04.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file4.txt", sep = "") 
filepath <-  paste(getwd(), "/04.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=4&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)



#5

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file5.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/05.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file5.txt", sep = "") 
filepath <-  paste(getwd(), "/05.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=5&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)



#6

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file6.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/06.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file6.txt", sep = "") 
filepath <-  paste(getwd(), "/06.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=6&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)




#7

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file7.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/07.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file7.txt", sep = "") 
filepath <-  paste(getwd(), "/07.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=7&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)




#8

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file8.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/08.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file8.txt", sep = "") 
filepath <-  paste(getwd(), "/08.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=8&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)



#9

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file9.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/09.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file9.txt", sep = "") 
filepath <-  paste(getwd(), "/09.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=9&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)



#10

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file10.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/10.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- paste(getwd(), "/file10.txt", sep = "") 
filepath <-  paste(getwd(), "/10.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[a-z]")
nonce <- charToRaw(noncechar)
url <-  paste("http://13.232.35.8:8080/known?id=",id,"&count=10&nonce=",noncechar,sep = "")
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)



