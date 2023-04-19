
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(janitor)
library(maps)
library(geosphere)
library(DT)
library(lubridate)

# obtain the world cities data
world_cities_data = maps::world.cities



# Define UI for app ----
ui <- dashboardPage(skin="purple",
              
                    # title in the header
                    dashboardHeader(title = "Your flying adventures"),
                    dashboardSidebar(
                      hr(),
                      sidebarMenu(id="tabs",
                   menuItem("Show Data",tabName = "show_data", icon = icon("database")),
                menuItem("About",tabName = "about", icon = icon("question")),
                conditionalPanel(condition = "input.tabs == 'show_data'", {
                  
                  fileInput("file",NULL,
                            buttonLabel = "Choose File",
                            accept = c(".xls",".xlsx"),
                            #accept = c('text/csv','text/comma-separated-values','text/tab-separated-values', 'text/plain', '.csv', '.tsv'),
                             multiple = FALSE)
                }),
                conditionalPanel(condition = "input.tabs == 'show_data'", {
                  actionButton("run", "Use your data")
                }),
                conditionalPanel(condition = "input.tabs == 'show_data'", {
                  actionButton("run_demo", "Use demo data instead")
                }),
                br(),
                conditionalPanel(condition = "input.tabs == 'show_data'", {
                  selectInput('mydropdown', label = 'Select an airline', choices = 'No choices here yet',multiple = F)
                }),
                conditionalPanel(condition = "input.tabs == 'show_data'", {
                  selectInput('mydropdown_date', label = 'Select a year', choices = 'No choices here yet',multiple = F)
                })
                # conditionalPanel(condition = "input.tabs == 'show_data'", {
                #   ImageOutput("img1")
                # })
                
                
                
                ) # end of sidebarMenu
  ),
  
  
  dashboardBody(
    
    tabItems(

      tabItem(tabName = "show_data",
              fluidRow(
                h1("Your flying stats"),
                shinydashboard::valueBoxOutput("Miles_flown", width=3),
                shinydashboard::valueBoxOutput("N_Airlines", width=3),
                shinydashboard::valueBoxOutput("Cities", width=3),
                shinydashboard::valueBoxOutput("Countries", width=3),
                mainPanel(plotOutput('world_map'),width = 10),
                DT::dataTableOutput("table")
              )
      ),
      tabItem(tabName = "about",
              fluidPage(
                h2("About this tool", style = "color:black; font-weight: 600"),
                p("This shiny app was developed for exploring and visualizing the patterns and statistics of your flights. 
                Although currently in its basic form, new features are expected to be added in the near future."),
                h2("Instructions of use:", style = "color:black; font-weight: 600"),
                p("The app takes as input an Excel file that requires to contain the following column names:"),
                p("Date, Departure City, Departure Country, Arrival City, Arrival Country, Miles flown, Airline", style = "color:blue"),
                HTML(paste0("If you want to calculate the miles flown for each of your flight, you can use the following website: ", tags$a(href = "https://www.airmilescalculator.com","www.airmilescalculator.com"))),
                p("In case you just want to have a look at the app, you can use the demo data available by pressing the demo data button instead"),
                h2("Necessary packages:", style = "color:black; font-weight: 600"),
                p("This Shiny app was build using the following packages: shiny(v1.6.0), readxl (v1.3.1), maps (v3.4.0), geosphere (v1.5-14), DT (v0.16) in R 4.0.2"),
                h2("Contact", style = "color:black; font-weight: 600"),
                p("If you have any trouble using this tool or would like to suggest anything, then please raise an issue."),
                HTML(paste0("You can also find me on twitter: ", tags$a(href = "https://twitter.com/Fimereli_Danai", "@Fimereli_Danai"))),
                br(),
                br(),
                h4("Disclaimer:"),
                p("Airline logos and images were obtained from wikipedia and I hold no copyright.")
              )
      )
    ) # end of tabItems
  ) # end of dashboard body
  
  
  
)



server <- function(input, output,session) {
  

  
  
  # This first code is for putting the empty boxes and the map ---------------------------------------
  output$Miles_flown <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Miles flown", 
      icon=icon("plane"), 
      color = "purple"
    )
  })
  
  output$N_Airlines <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Number of airlines", 
      icon=icon("plane"), 
      color = "blue"
    )
  })
  
  output$Cities <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Cities visited", 
      icon=icon("plane"), 
      color = "orange"
    )
  })
  
  output$Countries <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Countries visited", 
      icon=icon("plane"), 
      color = "red"
    )
  })
  
  
  output$world_map <- renderPlot ({
    map("world", fill=TRUE, col="#f2f2f2", bg="white",lwd=0.05, ylim=c(-60, 100), mar=c(0,0,0,0))
  })
  
  
  
  # Here is what happens when user data button is pushed  ---------------------------------------
  observeEvent(input$run, {
    # Get the file data
    fileData <- input$file
    if (is.null(fileData)) return(NULL)
    req(fileData)
    tbl = read_excel(fileData$datapath, sheet=1)
    tbl = as.data.frame(tbl)
    #tbl = read.table(fileData$datapath,sep="\t",header=T,stringsAsFactors = F)
    
    tbl$Date = lubridate::as_date(tbl$Date)
    tbl$dupDate = tbl$Date
    tbl = tidyr::separate(data = tbl, col = dupDate, into = c('year', 'month','day'), sep = '-')
    tbl = janitor::clean_names(tbl)
    
    
    updateSelectInput(session, "mydropdown", label = "Select an airline", choices = c("All",unique(tbl$airline)),selected="All")
    updateSelectInput(session, "mydropdown_date", label = "Select a year", choices = c("All",unique(tbl$year)),selected="All")
    
    # get the value from the drop down list
    subset_data_user <- reactive({
      if (input$mydropdown=="All" & input$mydropdown_date=="All") { # this is to take the whole table
        res = tbl 
      } else if (input$mydropdown!="All" & input$mydropdown_date=="All") { # only when the airline is chosen
        res = subset(tbl, airline == input$mydropdown) 
      } else if (input$mydropdown=="All" & input$mydropdown_date!="All") { # only when the year is chosen
        res = subset(tbl, year == input$mydropdown_date) 
      } else if (input$mydropdown!="All" & input$mydropdown_date!="All") { # when both airline and year are chosen
        res = subset(tbl, airline == input$mydropdown & year == input$mydropdown_date)
      }
    })

    
    
    output$Miles_flown <- renderValueBox({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      valueBox(
        value = sum(as.numeric(subset_data_user_tbl[,"miles_flown"])),
        subtitle = "Miles flown",
        icon=icon("plane"),
        color = "purple"
      )
    })
    
    output$N_Airlines <- renderValueBox({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_user_tbl[,"airline"])),
        subtitle = "Number of airlines",
        icon=icon("plane"),
        color = "blue"
      )
    })

    output$Cities <- renderValueBox({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_user_tbl[,"arrival_city"])),
        subtitle = "Cities visited",
        icon=icon("plane"),
        color = "orange"
      )
    })

    output$Countries <- renderValueBox({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_user_tbl[,"arrival_country"])),
        subtitle = "Countries visited",
        icon=icon("plane"),
        color = "red"
      )
    })
    
    
    output$table <- DT::renderDataTable({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      if (is.null(subset_data_user_tbl)) return(NULL)
      datatable(subset_data_user_tbl)
    })
    
    output$world_map <- renderPlot ({
      subset_data_user_tbl = subset_data_user() # here you get the value based on the drop down list
      map("world", fill=TRUE, col="#f2f2f2", bg="white",lwd=0.05, ylim=c(-60, 100), mar=c(0,0,0,0))
      if (nrow(subset_data_user_tbl) != 0) {
        for(i in 1:nrow(subset_data_user_tbl)) {
          dep_city = subset_data_user_tbl[i,"departure_city"]
          dep_country = subset_data_user_tbl[i,"departure_country"]
          arrival_city = subset_data_user_tbl[i,"arrival_city"]
          arrival_country = subset_data_user_tbl[i,"arrival_country"]
          dep_city_point = world_cities_data[world_cities_data$name==dep_city & world_cities_data$country.etc==dep_country,][1,]
          arrival_city_point = world_cities_data[world_cities_data$name==arrival_city & world_cities_data$country.etc==arrival_country,][1,]
          points(x=dep_city_point$long, y=dep_city_point$lat, col="slateblue", cex=3, pch=20)
          points(x=arrival_city_point$long, y=arrival_city_point$lat, col="slateblue", cex=3, pch=20)
          inter <- gcIntermediate(dep_city_point[,c("long","lat")],  arrival_city_point[,c("long","lat")], n=50, addStartEnd=TRUE, breakAtDateLine=F)
          lines(inter, col="slateblue", lwd=2)
          text(dep_city, x=dep_city_point$long, y=dep_city_point$lat,  col="slateblue", cex=1, pos=4)
          text(arrival_city, x=arrival_city_point$long, y=arrival_city_point$lat,  col="slateblue", cex=1, pos=4)
        } # end of for loop
      } # end of if loop
    }) # end of renderPlot
    
  }) # the observeEvent ends here
  
  
  # Here is what happens when demo data button is pushed ---------------------------------------
  observeEvent(input$run_demo, {
    # Get the file data
    #demo_data = read.table("./data/flight_demo_data.tsv",sep="\t",header=T,stringsAsFactors = F)
    demo_data = read_excel("./data/flight_demo_data.xlsx", sheet=1)
    demo_data = as.data.frame(demo_data)
    demo_data$Date = lubridate::as_date(demo_data$Date)
    demo_data$dupDate = demo_data$Date
    demo_data = tidyr::separate(data = demo_data, col = dupDate, into = c('year', 'month','day'), sep = '-')
    demo_data = janitor::clean_names(demo_data)
    updateSelectInput(session, "mydropdown", label = "Select an airline", choices = c("All",unique(demo_data$airline)),selected="All")
    updateSelectInput(session, "mydropdown_date", label = "Select a year", choices = c("All",unique(demo_data$year)),selected="All")
    
    # get the value from the drop down list
    subset_data <- reactive({
      if (input$mydropdown=="All" & input$mydropdown_date=="All") { # this is to take the whole table
        res = demo_data 
        } else if (input$mydropdown!="All" & input$mydropdown_date=="All") { # only when the airline is chosen
          res = subset(demo_data, airline == input$mydropdown) 
        } else if (input$mydropdown=="All" & input$mydropdown_date!="All") { # only when the year is chosen
          res = subset(demo_data, year == input$mydropdown_date) 
        } else if (input$mydropdown!="All" & input$mydropdown_date!="All") { # when both airline and year are chosen
          res = subset(demo_data, airline == input$mydropdown & year == input$mydropdown_date)
        }
    })
    
    
    
    output$Miles_flown <- renderValueBox({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      valueBox(
        value = sum(as.numeric(subset_data_tbl[,"miles_flown"])),
        subtitle = "Miles flown", 
        icon=icon("plane"), 
        color = "purple"
      )
    })
    
    output$N_Airlines <- renderValueBox({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_tbl[,"airline"])),
        subtitle = "Number of airlines", 
        icon=icon("plane"), 
        color = "blue"
      )
    })
    
    output$Cities <- renderValueBox({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_tbl[,"arrival_city"])),
        subtitle = "Cities visited", 
        icon=icon("plane"), 
        color = "orange"
      )
    })
    
    output$Countries <- renderValueBox({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      valueBox(
        value = length(unique(subset_data_tbl[,"arrival_country"])),
        subtitle = "Countries visited", 
        icon=icon("plane"), 
        color = "red"
      )
    })
    
    # Output table
    output$table <- DT::renderDataTable({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      datatable(subset_data_tbl)
    })
    
    # Output map
    output$world_map <- renderPlot ({
      subset_data_tbl = subset_data() # here you get the value based on the drop down list
      map("world", fill=TRUE, col="#f2f2f2", bg="white",lwd=0.05, ylim=c(-60, 100), mar=c(0,0,0,0))
      if (nrow(subset_data_tbl) != 0) {
        for(i in 1:nrow(subset_data_tbl)) {
          dep_city = subset_data_tbl[i,"departure_city"]
          dep_country = subset_data_tbl[i,"departure_country"]
          arrival_city = subset_data_tbl[i,"arrival_city"]
          arrival_country = subset_data_tbl[i,"arrival_country"]
          dep_city_point = world_cities_data[world_cities_data$name==dep_city & world_cities_data$country.etc==dep_country,][1,]
          arrival_city_point = world_cities_data[world_cities_data$name==arrival_city & world_cities_data$country.etc==arrival_country,][1,]
          points(x=dep_city_point$long, y=dep_city_point$lat, col="slateblue", cex=3, pch=20)
          points(x=arrival_city_point$long, y=arrival_city_point$lat, col="slateblue", cex=3, pch=20)
          inter <- gcIntermediate(dep_city_point[,c("long","lat")],  arrival_city_point[,c("long","lat")], n=50, addStartEnd=TRUE, breakAtDateLine=F)
          lines(inter, col="slateblue", lwd=2)
          text(dep_city, x=dep_city_point$long, y=dep_city_point$lat,  col="slateblue", cex=1, pos=4)
          text(arrival_city, x=arrival_city_point$long, y=arrival_city_point$lat,  col="slateblue", cex=1, pos=4)
        } # end of for loop
      } # end of if loop
    }) # end of renderPlot
    
  }) # end of demo observeEvent
  
  
}
  
 
  
  
  
 
 


# define names
shinyApp(ui = ui, server = server)

