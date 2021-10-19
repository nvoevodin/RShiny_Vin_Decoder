
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
library(waiter)
library(RMySQL)
library(pool)

# tab <- c()


mydb <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "plagueDB",
      host = "68.198.26.175",
    port = 3306,
  username = "root",
  password = "Nv0ev0din!"
 
)

# mydb <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "plagueDB",
#   host = "67.83.57.25",
#   username = "remote",
#   password = "GreenAppl3!"
# )


# onStop(function() {
#   poolClose(mydb)
# })

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
# colnames(data)[10] <- 'Results.FuelTypePrimary'
# colnames(data)[11] <- 'Results.FuelTypeSecondary'
# 
# tab$n <- data
# 
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Gasoline']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
# tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & is.na(tab$n$Results.FuelTypeSecondary),Type:='Unknown']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Diesel' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Diesel']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Flexible Fuel Vehicle (FFV)',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Electric']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Electric' & tab$n$Results.FuelTypeSecondary == 'Electric, Gasoline',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Compressed Natural Gas (CNG)',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline, Electric',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
# tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Gasoline']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Compressed Natural Gas (CNG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='CNG']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Ethanol (E85)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Ethanol']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
# tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Liquefied Petroleum Gas (propane or LPG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='LPG']
# 
# data <- tab$n
# 
# fwrite(data, 'vehs.csv')

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


looop <- function(x){
  
  if (length(x) <=50){
    
    data <- request(x)
    return(data)
    
    
  }
  else if (length(x) > 50 && length(x) <=500) {
    choppedVins <- chunk2(x,5)
    out = NULL
    for (i in choppedVins){
      data <- request(i)[,c('Results.VIN','Results.Make','Results.Model','Results.ModelYear','Results.FuelTypePrimary','Results.FuelTypeSecondary')]
      out=rbind(out,data)
    }
    # data1 <- x[1:300]
    # out1<-request(data1)
    # print(head(out1))
    # data2 <- x[301:600]
    # out2<-request(data2)
    # data3 <- x[601:900]
    # out3<-request(data3)
    # data4 <- x[901:1200]
    # out4<-request(data4)
    # out <- bind_rows(out1,out2,out3,out4)
    return(out)
  }
  else if (length(x) > 500) {
    test1 <- dbGetQuery(mydb,"SELECT * from decodedVINS")
    #print(head(test1))
    data <- as.data.table(x)
    colnames(data)[1] <- 'vin'
    #print(head(data))
    data <- left_join(data,test1)
    return(data)
    
  }
  
}


download1 <- function(){showModal(modalDialog(
  title = "Download Decoded Vehicles",
  paste0("This will download all decoded active TLC vehicles as of ",Sys.Date(),". Note: Automatic update runs every Tue and Thur, so..."),
  downloadButton("btnn", "Download")
))}

# return_vins <-  function(my_vin){
#   vinme <- paste0('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/',my_vin,'?format=json')
#   print(vinme) #prints the url
#   vinme = httr::GET(vinme)
#   result = jsonlite::fromJSON(httr::content(vinme, as = "text")) #this reads the url
#   result = as.data.table(result)  #turns it into a nice data set
# }

# f <- read.csv('H:\\Nikita\\vehicles\\vehs.csv')
# f <- f[f$make == 'Tesla',]
#  fwrite(f,'sample.csv')

# perc <- c(data %>%
#     dplyr::filter(entity %in% c("CAR",'SHL','MED'), status == 'active') %>%
#     dplyr::group_by(entity) %>%
#     tally() %>%
#     dplyr::mutate(prc = round(n/sum(n) * 100, 2)))

