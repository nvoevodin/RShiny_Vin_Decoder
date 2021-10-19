    library(shiny)
    library(shinydashboardPlus)
    library(shinydashboard)
    library(shinyalert)
    library(data.table)
    library(dplyr)
    library(shinyWidgets)
    library(filesstrings)
    library(shinycssloaders)
    library(rintrojs)
    library(tools)
    library(shinyjs)
    library(lubridate)
    library(pbapply)
    library(httr)
    library(jsonlite)
    library(RCurl)
    library(DT)
library(echarts4r)
library(curl)
library(httr)
library(reticulate)

# tab <- c()
data <- fread('vehs.csv')

data1 <- head(data$vin,5)

empty_vins = data1

return_vins <-  function(my_vin){
  #vinme <- paste0('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/',my_vin,'?format=json')
  
  
  
  
  #print(vinme) #prints the url
  #req <- curl_fetch_memory(vinme)
  #result = jsonlite::prettify(rawToChar(req$content)) #this reads the url
  #result = as.data.table(result)  #turns it into a nice data set
  print(json)
}
  #4JGCB5HE1CA138466

a <- Sys.time()

for (i in vins){
  
  #pushes past the error if the api cant find something
  # tricatch_result=
  #   tryCatch({
  json  <- fromJSON(paste0('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/',i,'?format=json'))
      print(json)
     
  #     
  #   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # 
  # print(tricatch_result)
  
  # tryCatch({
  #   fwrite(tricatch_result,paste0("data/vin",tricatch_result$`Results.VIN`,".csv"))
  # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # 
  # Sys.sleep(.001) #.001 second pause after each ping so that it won't time out
  # print(empty_vin)
  # tricatch_result
}

b <-Sys.time()

print(b-a)



  headers = c(
    `Content-Type` = 'application/json'
  )
  
  data = '[{"data":"4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466"}]'
  
  httr::POST(url = 'https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/', httr::add_headers(.headers=headers), body = data)
  print(r$status_code)



  #data <- list(data='4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466')
  POST('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/', 
       body = jsonlite::unbox(body), encode = "json")
  
  
  httr::POST('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/',
             body = '4JGCB5HE1CA138466;4JGCB5HE1CA138466', encode = 'raw',verbose())
  body = list(data = '4JGCB5HE1CA138466;4JGCB5HE1CA138466')
  
  request_body_json <- toJSON(list(data=data), auto_unbox = TRUE)
  result <- POST('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/',
                 body = data)
  
  Output <- content(result)


apiurl <- "http://geocoding.geo.census.gov/geocoder/locations/addressbatch"

addresses <- "1, 1 Infinite Loop, Cupertino, CA, 95014
2, 1 Hacker Way, Menlo Park, CA, 94025"

addresseFile <- "addresses.csv"
writeLines(addresses , addresseFile )

resp<-POST(apiurl, body=list(addressFile=upload_file(addresseFile), benchmark=4), 
           encode="multipart")
content(resp)


########################################################################
  
library(jsonlite)
library(httr)
library(purrr)

install.packages('fivethirtyeight')
library(fivethirtyeight)
data()

da <- candy_rankings

request <- function(x){
  r <- httr::POST(
    url = "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/",
    body = list(
      format = "json",
      data = paste(x, collapse = ";")
    ),
    encode = "form",
    verbose()
  )
  
  results1 = jsonlite::fromJSON(httr::content(r, as = "text")) #this reads the url
  result2 = as.data.table(results1) 
  return(result2)
}


#I added purrr to the list of libraries because it's really great for working with lists like the one that the NHTSA API returns.

#Here is our "list" of VINS

vins <- rep("4JGCB5HE1CA138466", 100)

vins <- c("4JGCB5HE1CA138466","4JGCB5HE1CA138466","4JGCB5HE1CA138466","4JGCB5HE1CA138466","4JGCB5HE1CA138466", "4JGCBgggCA138466","4JGCB5HE1CA138466","4JGCB5HE1CA138466")
# It took some trial and error, but from looking at the examples the NHTSA provided in other languages, such as JavaScript, I was able to piece together that the body of the message should be a JSON object with entries for format = "json" and data to hold the VINs concatenated into a single string separated by ;. Note that httr will automatically convert lists to JSON for us.
# 
# The final detail was realizing that the NHTSA API wanted the POST body encoded as a form, requiring the encode = "form".
# 
# I also added verbose() so that httr prints information about the request. It can be helpful during debugging.






looop <- function(x){
  
if (length(x) <=500){
  
request(x)
  
  
}
 else {
   data1 <- x[1:500]
   data2 <- x[501:1000]
   data3 <- x[1001:1500]
   print(data2)
 }
  
}

a <- Sys.time()
data <- looop(vins)
b <-Sys.time()

print(b-a)

#results <- httr::content(r)
#httr automatically converts the returned JSON into an R list. Here we can see that the NHTSA API returns a JSON object with four top-level entries.

#str(results, max.level = 1)
#> List of 4
#>  $ Count         : int 3
#>  $ Message       : chr "Results returned successfully. NOTE: Any missing decoded values should be interpreted as NHTSA does not have da"| __truncated__
#>  $ SearchCriteria: chr ""
#>  $ Results       :List of 3
#The individual VIN results are returned in .$Results, and here I show just the first 10 entries with text of the first result...






results$Results[[1]] %>%
  purrr::keep(~ . != "") %>%
  .[1:10] %>%
  str()

  
vins <-"4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466;4JGCB5HE1CA138466"


r <- httr::POST(
  url = "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/",
  body = list(
    format = "json",
    data = vins
  ),
  encode = "application/json",
  verbose()
)

results1 = jsonlite::fromJSON(httr::content(r, as = "text")) #this reads the url
result2 = as.data.table(results1) 
return(result2)
  
