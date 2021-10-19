  


server = (function(input, output,session) {

    
    
    shinyalert("Welcome To 
              TLC VIN DECODER!", paste0("Upload a single VIN or a batch to get engine and other info on your vehicles.

By:
Nikita Voevodin - Senior Data Analyst

                         "), imageUrl = 'http://tlc-mag.com/images/Oct15/TLCNYC_LOGO.jpg')
    
    
  
  
  output$title0 <- renderText({'TLC VIN DECODER'})

 #Decoder-------------------------------------------------------   
    
    
    #counter--------------------------------------    
 
tab <- reactiveValues(n = data.frame())    
       
observeEvent(input$submit,{
  hide(id = "content", anim = TRUE, animType = "fade") 
  show_waiter(tagList(
    spin_folding_cube(),
    span("Loading ...", style = "color:white;")
  ))
     empty_vins = c(input$vin)
     
    # print(empty_vins)

     # vin_results <- pblapply(empty_vins,function(empty_vin){
     # 
     #   #pushes past the error if the api cant find something
     #   tricatch_result=
     #     tryCatch({
     #       return_vins(empty_vin)
     # 
     #     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
     # 
     # 
     # 
     #   tryCatch({
     #     fwrite(tricatch_result,paste0("data/vin",tricatch_result$`Results.VIN`,".csv"))
     #   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
     # 
     #   Sys.sleep(.001) #.001 second pause after each ping so that it won't time out
     #   print(empty_vin)
     #   tricatch_result
     # })



       tab$n = looop(empty_vins)[,c('Results.VIN','Results.Make','Results.Model','Results.ModelYear','Results.FuelTypePrimary','Results.FuelTypeSecondary')]
       # list.files('data/', pattern = '.csv') %>%
       # pblapply(function(x){
       #   read.csv(paste0('data/',x),stringsAsFactors=FALSE)[,c('Results.VIN','Results.Make','Results.Model','Results.ModelYear','Results.FuelTypePrimary','Results.FuelTypeSecondary')]
       # }) %>% rbindlist()
     tab$n$Results.FuelTypeSecondary[tab$n$Results.FuelTypeSecondary == ""] <- NA
     tab$n$Results.FuelTypePrimary[tab$n$Results.FuelTypePrimary == ""] <- NA
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Gasoline']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
     tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & is.na(tab$n$Results.FuelTypeSecondary),Type:='Unknown']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Diesel' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Diesel']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Flexible Fuel Vehicle (FFV)',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Electric']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Electric' & tab$n$Results.FuelTypeSecondary == 'Electric, Gasoline',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Compressed Natural Gas (CNG)',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline, Electric',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
     tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Gasoline']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Compressed Natural Gas (CNG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='CNG']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Ethanol (E85)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Ethanol']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
     tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Liquefied Petroleum Gas (propane or LPG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='LPG']  
     
   
   
   # output$table <- renderDataTable(tab$n, extensions = 'Scroller', options = list(
   #   deferRender = TRUE,
   #   scrollY = 390,
   #   scroller = TRUE
   # ))
   
     output$vin1 <- renderText({paste0('VIN NUMBER: ',tab$n$Results.VIN[1])})
     output$make <- renderText({paste0('MAKE: ',tab$n$Results.Make[1])})
     output$model <- renderText({paste0('MODEL: ',tab$n$Results.Model[1])})
     output$year1 <- renderText({paste0('YEAR: ',tab$n$Results.ModelYear[1])})
   output$type1 <- renderText({paste0('TYPE: ',tab$n$Type[1])})
   hide_waiter()
#    mydir <- "data/"
#    delfiles <- dir(path=mydir ,pattern="*.csv")
# file.remove(file.path(mydir, delfiles))   
   
   })


#####################################################################

observeEvent(input$vins,{
  
  
  
  hide(id = "content", anim = TRUE, animType = "fade") 

 b <- read.csv(input$vins$datapath)
 

    empty_vins = c(trimws(b$vin))
    
    show_waiter(tagList(
      spin_folding_cube(),
      if (length(empty_vins) <= 50){
        span("Loading ... Estimated Time: Less then a minute", style = "color:white;")
      } else if (length(empty_vins) > 50 && length(empty_vins) < 500){
        span("Loading ... Estimated Time: This will take a while (5-10 min).", style = "color:white;")
      }
      
    ))   

    output$download1 <- renderUI(downloadButton('downloadData3', 'Download Data Set'))
    # print(empty_vins)

    # vin_results <- pblapply(empty_vins,function(empty_vin){
    # 
    #     #pushes past the error if the api cant find something
    #     tricatch_result=
    #         tryCatch({
    #             return_vins(empty_vin)
    # 
    #         }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    # 
    # 
    # 
    #     tryCatch({
    #         fwrite(tricatch_result,paste0("data/vin",tricatch_result$`Results.VIN`,".csv"))
    #     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    # 
    #     Sys.sleep(.001) #.001 second pause after each ping so that it won't time out
    #     print(empty_vin)
    #     tricatch_result
    # })
    output$title1 <- renderText({'Total Vehicles'})
    output$title2 <- renderText({'Fuel Split'})
    output$title3 <- renderText({'Average Age'})

    if (length(empty_vins) <= 500){
    tab$n = looop(empty_vins)[,c('Results.VIN','Results.Make','Results.Model','Results.ModelYear','Results.FuelTypePrimary','Results.FuelTypeSecondary')]
    
        # list.files('data/', pattern = '.csv') %>%
        # pblapply(function(x){
        #     read.csv(paste0('data/',x),stringsAsFactors=FALSE)[,c('Results.VIN','Results.Make','Results.Model','Results.ModelYear','Results.FuelTypePrimary','Results.FuelTypeSecondary')]
        # }) %>% rbindlist()
    
    tab$n$Results.FuelTypeSecondary[tab$n$Results.FuelTypeSecondary == ""] <- NA
    tab$n$Results.FuelTypePrimary[tab$n$Results.FuelTypePrimary == ""] <- NA
    
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Gasoline']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
    tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & is.na(tab$n$Results.FuelTypeSecondary),Type:='Unknown']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Diesel' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Diesel']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Flexible Fuel Vehicle (FFV)',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Ethanol (E85)',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Electric']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Electric' & tab$n$Results.FuelTypeSecondary == 'Electric, Gasoline',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline' & tab$n$Results.FuelTypeSecondary == 'Compressed Natural Gas (CNG)',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline, Electric',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Gasoline, Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
    tab$n <- setDT(tab$n)[is.na(tab$n$Results.FuelTypePrimary) & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Gasoline']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Compressed Natural Gas (CNG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='CNG']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Ethanol (E85)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Ethanol']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV)' & tab$n$Results.FuelTypeSecondary == 'Electric',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Flexible Fuel Vehicle (FFV), Gasoline' & tab$n$Results.FuelTypeSecondary == 'Gasoline',Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Electric, Gasoline' & is.na(tab$n$Results.FuelTypeSecondary),Type:='Hybrid']
    tab$n <- setDT(tab$n)[tab$n$Results.FuelTypePrimary == 'Liquefied Petroleum Gas (propane or LPG)' & is.na(tab$n$Results.FuelTypeSecondary),Type:='LPG']
    
    colnames(tab$n) <- c('VIN','Make','Model','Year','Primary Fuel','Secondary Fuel', 'Type')
    
    output$table <- renderDataTable(
      if (input$type == 'All'){setDT(tab$n)} 
      else {setDT(tab$n)[tab$n$Type == input$type,]}, 
      extensions = 'Scroller', 
      options = list(
        deferRender = TRUE,
        scrollY = 390,
        scroller = TRUE
      ))
    
    output$byType <- renderEcharts4r(
      tab$n %>%
        
        dplyr::group_by(Type) %>%
        dplyr::tally()%>%
        dplyr::mutate(prc = round(n/sum(n) * 100, 2)) %>%
        e_charts(Type)%>%
        e_pie(prc, radius = c("45%", "70%")) %>%
        e_tooltip(trigger = 'item')%>%
        
        e_legend(show = FALSE)
    )
    
    output$countOfVehs <- renderText({paste0(if(input$type == 'All') {tab$n %>% count()} else {tab$n %>% dplyr::filter(tab$n$Type == input$type) %>% count()})})
    output$averageAge <- renderText({paste0(if(input$type == 'All'){tab$n %>% dplyr::select(Year) %>% filter(!is.na(Year)) %>% dplyr::summarise(Year = round(as.integer(format(Sys.Date(), "%Y")) - mean(as.numeric(Year)),2))}
                                            else {tab$n %>% dplyr::filter(tab$n$Type == input$type) %>% dplyr::select(Year) %>% filter(!is.na(Year)) %>% dplyr::summarise(Year = round(as.integer(format(Sys.Date(), "%Y")) - mean(as.numeric(Year)),2))})})
  
    output$downloadData3 = downloadHandler(
      filename = function() {
        
        paste('Decoded', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        data <- tab$n
        write.csv(data, con, row.names = F)
      }
    )
    
    } else {
      datar <- reactive({
       data = looop(empty_vins)
       
       return(data)
      })
      
      output$table <- renderDataTable(
        if (input$type == 'All'){setDT(datar())} 
        else {setDT(datar())[datar()$Type == input$type,]}, 
        extensions = 'Scroller', 
        options = list(
          deferRender = TRUE,
          scrollY = 390,
          scroller = TRUE
        ))
      
      output$byType <- renderEcharts4r(
        datar() %>%
          
          dplyr::group_by(Type) %>%
          dplyr::tally()%>%
          dplyr::mutate(prc = round(n/sum(n) * 100, 2)) %>%
          e_charts(Type)%>%
          e_pie(prc, radius = c("45%", "70%")) %>%
          e_tooltip(trigger = 'item')%>%
          
          e_legend(show = FALSE)
      )
      
      output$countOfVehs <- renderText({paste0(if(input$type == 'All') {datar() %>% count()} else {datar() %>% dplyr::filter(datar()$Type == input$type) %>% count()})})
      output$averageAge <- renderText({paste0(if(input$type == 'All'){datar() %>% dplyr::select(year) %>% filter(!is.na(year)) %>% dplyr::summarise(year = round(as.integer(format(Sys.Date(), "%Y")) - mean(as.numeric(year)),2))}
                                              else {datar() %>% dplyr::filter(datar()$Type == input$type) %>% dplyr::select(year) %>% filter(!is.na(year)) %>% dplyr::summarise(year = round(as.integer(format(Sys.Date(), "%Y")) - mean(as.numeric(year)),2))})})
      
    
      output$downloadData3 = downloadHandler(
        filename = function() {
          
          paste('Decoded', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          data <- datar()
          write.csv(data, con, row.names = F)
        }
      )
      }
      




    # mydir <- "data/"
    # delfiles <- dir(path=mydir ,pattern="*.csv")
    # file.remove(file.path(mydir, delfiles))
    

    hide_waiter()
})




#Vehicles-------------------------------------------------------




observeEvent(input$submit1,{
  data <- dbGetQuery(mydb,"SELECT * from decodedVINS")
  
  
  
  output$table2 <- renderTable(
    head(data %>% dplyr::group_by(make,model) %>% dplyr::count() %>% arrange(desc(n)),20) %>% rename(MAKE = make, MODEL = model, COUNT = n)
    )
  
  output$title4 <- renderText({'Total Vehicles'})
  output$title6 <- renderText({'Top Vehicles'})
  output$title5 <- renderText({'Average Age'})
  
  output$byType1 <- renderEcharts4r(
    data %>%
      
      dplyr::group_by(Type) %>%
      dplyr::tally()%>%
      dplyr::mutate(prc = round(n/sum(n) * 100, 2)) %>%
      e_charts(Type)%>%
      e_pie(prc, radius = c("55%", "90%")) %>%
      e_tooltip(trigger = 'item')%>%
      
      e_legend(show = FALSE)
  )
  
  output$countOfVehs1 <- renderText({paste0(data %>% count())})
  print(str(data))
  output$averageAge1 <- renderText({paste0(data %>% dplyr::select(year) %>% filter(!is.na(year)) %>% dplyr::summarise(year = round(as.integer(format(Sys.Date(), "%Y")) - mean(year),2)))})
})


observeEvent(input$submit2,{
  
  
  download1()
  
  
})



output$btnn = downloadHandler(
  filename = function() {
    paste('decodedTlcVehs', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data <- dbGetQuery(mydb,"SELECT * from decodedVINS")
    write.csv(data, con, row.names = F)
  }
)  
    
    
})