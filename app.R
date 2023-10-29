################################################################
# Shiny app by Will Lehnherr
# Oct 30, 2023
#
# Shows the location of fatal accidents in 2022 in the US
#
# Deployed at https://wlstat415.shinyapps.io/Assignment10/
# Source code at GitHub: https://......
################################################################

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(readxl)
library(maps)
library(mapproj)

accident <- read_csv("accident.csv", col_types =
                       cols(MONTHNAME = col_factor(
                         levels = c(
                           "January",
                           "February",
                           "March",
                           "April",
                           "May",
                           "June",
                           "July",
                           "August",
                           "September",
                           "October",
                           "November",
                           "December"
                         )
                       )))
Pop2019 <- read_excel("Pop2019.xlsx")

accident2 <- merge(accident,Pop2019, by = "STATENAME")

accident3 <- accident2 %>%
  group_by(STATENAME, WEATHERNAME, Population) %>%
  summarise(NumAccidents = n()) %>%
  mutate(AccidentsPer10k = round(10000*NumAccidents/Population,3))

accident4 <- accident3 %>%
  filter(WEATHERNAME == "Clear" || WEATHERNAME == "Rain" || WEATHERNAME == "Snow")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Accident Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio",
                   "Select Graph Type:",
                   choices = list("Bar Graph" = 1,
                                  "Map" = 2)),
      conditionalPanel(
        condition = "input.radio == 1",
        selectInput(
          "select",
          label = h3("Select box"),
          choices = unique(accident$STATENAME)
        ),
        selectInput(
          "Color",
          "Color Palette:",
          c(
            "Blue" = "blue",
            "Red"  = "red",
            "Green" = "green"
          )
        )
      ),
      conditionalPanel(
        condition = "input.radio == 2",
        selectInput(
          "weather",
          label = h3("Select Weather"),
          choices = unique(accident4$WEATHERNAME)
        )
      ),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"), )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$value <- renderPrint({
    input$select
  })
  output$value <- renderPrint({
    input$radio
  })
  
  output$distPlot <- renderPlot({

      x    <<- accident %>%
        filter(STATENAME == input$select)
      stringTitle <- paste("Number of Accidents in",input$select, "by Month")
    if(input$radio == 1){
    ggplot(x, aes(x = MONTHNAME, fill = MONTHNAME)) +
      geom_bar(fill = input$Color) +
      ggtitle(stringTitle)
    }
    else if(input$radio == 2){
      y <- accident4 %>%
        filter(WEATHERNAME == input$weather)
      yTitle <- paste("Accidents Caused by",input$weather,"Weather Conditions Per 10k People")
      
      
      y$region <- tolower(y$STATENAME)
      states <- map_data("state")
      map <- merge(states, y, by = "region", all.x = TRUE)
      map <- map[order(map$order),]
      ggplot(map, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = AccidentsPer10k)) +
        geom_path() +
        scale_fill_distiller(palette = "Purples", direction = 1) +
        coord_map() +
        labs(x = "", y = "") +
        guides(fill = guide_legend(title = "Accidents Per 10K"))+
        ggtitle(yTitle)+
        theme(legend.background = element_rect(fill="gray", 
                                               linewidth =0.5, linetype="solid"),
              legend.text=element_text(size=15),
              legend.title=element_text(size=15))
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
