punts <- readRDS("punts.Rds")

shinyServer(function(input, output) {
  library(ggplot2)
  
  punterData <- reactive({
    if (input$punter == "All"){
      returned2 <- punts
      #trim out the punters with <= 5 punts to get rid of some noise
      returned2[returned2$punter %in% names(table(returned2$punter)[table(returned2$punter) <= 10]), "punter"] <- NA 
      returned2 <- returned2[!is.na(returned2$punter),]  
      
      return(returned2)
    } else{
      thisPunter <- punts[punts$punter == input$punter & !is.na(punts$punter),] 
      
      return(thisPunter)
    }
  })
  
  teams <- reactive({
    if (input$punter != "All"){      
      teams <- unique(as.character(punterData()$off))
      return(teams)
    }
  })
  
  output$main_plot <- renderPlot({
    if (input$punter == "All"){
      returned2 <- punts
      #trim out the punters with <= 5 punts to get rid of some noise
      returned2[returned2$punter %in% names(table(returned2$punter)[table(returned2$punter) <= 10]), "punter"] <- NA 
      returned2 <- returned2[!is.na(returned2$punter),]  
      print(ggplot(returned2, aes(punter, retYards)) + geom_boxplot() + theme(axis.text.x = element_text(vjust=0.5,angle=90)) + ggtitle("Yards on Returned Punts by Punter"))
    } else{
      thisPunter <- punterData()
      
      thisTeam <- punts[punts$off %in% teams() & !is.na(punts$off),] 
      thisTeam$group <- as.character(thisTeam$off)
      thisTeam[thisTeam$punter == input$punter,]$group <- input$punter
      
      print(ggplot(thisTeam, aes(group, retYards)) + geom_boxplot() + xlab("Group") + ylab("Yards on Returned Punts") + ggtitle("Comparison of Returned Punts Yards by Group"))
    }
  })
  
  output$resultsBar <- renderPlot({
    print(ggplot(punterData(), aes(as.factor(result))) + geom_bar(binwidth=1) + xlab("Result of Punts") + theme(axis.text.x = element_text(vjust=0.5,angle=90)))
  })
  
  output$years <- renderPlot({
    print(ggplot(punterData(), aes(as.factor(year))) + geom_bar(binwidth=1) + xlab("Year"))
  })
  
  output$data <- renderTable({
    teams <- teams()
    years <- unique(as.character(punterData()$year))
    
    data.frame(Key=c("Teams", 
                     "Years", 
                     "Total Punts"), 
               Value=c(paste(teams, collapse=","), 
                       paste(min(years), max(years), sep="-"),
                       nrow(punterData())))
    
  })
})