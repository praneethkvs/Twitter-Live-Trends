library(shiny)
library(twitteR)

locs <- read.table("locations.txt",header = T)

#Insert your own keys here.
consumer_key <- "consumerKey"
consumer_secret <- "consumerSecret"
access_token <- "accesToken"
access_secret <- "accessSecret"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

 ui <- navbarPage(strong("Twitter Live"),windowTitle = "Live Twitter Trends",
                         tabPanel(title = "Trends",value = "trends",
                                      sidebarLayout(
                                        sidebarPanel(width=3,selectizeInput(inputId = "loc", label=h4("Select one or More Locations"),selected="Worldwide",multiple= TRUE,choices=locs$location,options = list(placeholder="Start typing a location")),
                                                      actionButton(inputId="updatelocs", label="Submit"),
                                                      br(),hr(),"You can select multiple locations",br(),br(),
                                                      strong("Keep Live track of the latest Twitter trending topics through out the day locally and globally."),br(),br(),
                                                      strong("Click on the trends to open up Twitter and get tweeting - maximise your reach and audience.")),
                                        mainPanel(dataTableOutput("locout"))
                                  )))
 

 server <- function(input,output,session){
   session$onSessionEnded(stopApp)
   
   
   trendsdf <- eventReactive(input$updatelocs,{
   sellocs <- locs[match(input$loc,locs$location),1]
   res <- lapply(sellocs,function(f){ df <- data.frame(paste0("<a href=",getTrends(f)$url," target='_blank'>",getTrends(f)$name,"</a>"),stringsAsFactors = F)
     
     df
	})
     
     lens <- sapply(res,nrow)
     lmax <- max(lens)
     
	 res2 <- lapply(res,function(f){ df <- f 
     df[nrow(df):lmax,] <- " "
     df
     })
     
     finaldf <- as.data.frame(res2)
     colnames(finaldf) <- input$loc
     finaldf
     
   })
   
   output$locout <- renderDataTable(trendsdf(),escape = FALSE)
   
 }
 
 shinyApp(ui=ui,server=server)
