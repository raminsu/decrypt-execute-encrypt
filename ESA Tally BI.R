

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)
library(data.table)


Sys.setenv(wd = "C:/Users/user/Desktop/Tally/ESA_Attributes")
Sys.setenv(id = 240433)
Sys.setenv(tallyurl = "http://localhost:9001")
Sys.setenv(ipaddress = "http://13.232.35.8:8080/")

currentdirectory <-  Sys.getenv(x = "wd", unset = "", names = NA)
id <-  Sys.getenv(x = "id", unset = "", names = NA)
tallyurl <-  Sys.getenv(x = "tallyurl", unset = "", names = NA)
ipaddress <-  Sys.getenv(x = "ipaddress", unset = "", names = NA)
setwd(currentdirectory)

if(file.exists(paste(getwd(), "/updatevoucher.txt" , sep = ""))){
# write("i have finished", "C:/Users/user/Desktop/completed1.txt")



filepathofencryptedfile <-  paste(getwd(), "/01.txt", sep = "") 
filepathofdecryptedfile <- paste(getwd(), "/file1.txt", sep = "") 

key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))
url <- paste(ipaddress,"known?id=",id,"&count=1&nonce=a", sep = "") 
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

}

# #used
# writecompanynameforfutureuse <- function(companynamerequestpath){
#   xmldf_companyname <- getxmlresponse(companynamerequestpath)
#   xmldf_companyname <- xmlstringsubset(xmldf_companyname, "<DATA>" , "</DATA>")
#   xmldf_companyname <- convertnestedxmltodf(xmldf_companyname, "//COMPANY")
#   write(as.character(xmldf_companyname[1,1]),pathofcompanynamefile)
# }


#write("i have finished", "C:/Users/user/Desktop/completed.txt")
# site_path = R.home(component = "home")
# # fname = file.path(site_path, "etc", "Rprofile.site")
# # file.exists(fname)
# file.edit("~/.Rprofile")
# 
# Sys.sleep(10)
# write("i have finished", "C:/Users/user/Desktop/completed.txt")
# 
# 
# library("PythonInR")
# autodetectPython(pythonExePath = NULL)
# pyConnect()
# PythonInR::pyExecfile("C:/Users/user/Desktop/sampleypython.py")
# pyExit()
# 
# memory.limit(size = 2048)
# 
# 
# source("E:\rprojects\esquare_v5\rlog.r")
