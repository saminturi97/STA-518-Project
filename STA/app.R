library(tidyverse)
library(plotly)
library(highcharter)
library(shiny)
library(tidyverse)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
library(urltools)
library(dplyr)
library(rworldmap)
library(plotly)
airbnb <- read.csv("airbnb-listings-2.csv", sep = ";")

# We will select important variables for our analysis and remove the missing values.

df <- airbnb %>% 
    select(Name, City, State, Zipcode, Market, Country.Code, Country, Latitude, Longitude, Property.Type, Room.Type, Accommodates, Bathrooms, Bedrooms, Beds, Bed.Type, Price, Guests.Included, Geolocation)%>%
    filter(Property.Type%in%c("Apartment","House","Gesthouse","Condominium","Loft","Townhouse","Villa","Bed & Breakfast","Cabin","
Bungalow","Castle","Camper/RV"))

df <- df[complete.cases(df), ]
df$Bed.Type <- factor(as.character(df$Bed.Type))
df$Price <- as.double(df$Price)
df1 <- data.frame(price = df$Price, Bed.Type = df$Bed.Type)

#  Data preparation for US Map


library(usmap)
library(ggplot2)
library(rgdal)


df3 <- df %>% filter(Country.Code == "US") %>% 
    group_by(State) %>% 
    summarise(count = n())
df4 <- statepop %>% 
    left_join(df3, by = c("abbr" = "State"))
df4 <- df4[complete.cases(df4), ]
df6<-airbnb%>%
    filter(Country.Code=="US")%>%
    group_by(State)%>%
    summarise(Count=n())
df7<-airbnb%>%
    select(Price,Beds,Accommodates,Bedrooms,Bathrooms,Number.of.Reviews,Property.Type)






library(shiny)

# Define UI for application 
ui <- fluidPage(
    titlePanel("Airbnb data Exploration"),
    tabsetPanel(
        tabPanel("Plot for Accommodates vs Bedrooms ",
                 sidebarLayout(
                     sidebarPanel(selectInput("Property.Type", "Property.Type", choices = unique(df$Property.Type), selected = "Guesthouse")),
                     mainPanel(
                         p(""),
                         plotOutput("scatter_plot")
                     )
                 )
        ),
        tabPanel("Boxplot of Price for different Bed types",
                 sidebarLayout(
                     sidebarPanel(selectInput("Bed.Type", "Bed.Type:", choices = unique(df$Bed.Type), selected = "Futon")),
                     mainPanel(
                         p(""),
                         plotOutput("boxplot")
                     )
                 )
        ),
        
        tabPanel("Bar plot of Room type for States",
                 sidebarLayout(
                     sidebarPanel(checkboxGroupInput("State", "State:", choices = unique(df$State), selected = unique(df$State)[1:3])),
                     mainPanel(
                         p(""),
                         plotOutput("barplot")
                     )
                 )
        ),
        tabPanel("US map for Number of Hotels",
                 
                 mainPanel(
                     p(""),
                     plotlyOutput("Map")
                 )
        ),
        tabPanel("Price comaprison",
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectInput('xcol','X Variable', names(df7)),
                         selectInput('ycol','Y Variable', names(df7)),
                         selected = names(df7)[[2]]),
                     mainPanel(
                         p(""),
                         plotlyOutput("scat")) 
                 )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    output$scatter_plot <- renderPlot({
        df %>% filter(Property.Type == input$Property.Type) %>% 
            ggplot(aes(x = Accommodates, y = Bedrooms, colour = Room.Type, size = Beds)) + geom_point()+theme_minimal()+
            labs()
        
        
    })
    
    output$boxplot <- renderPlot({
        df1 %>% filter(Bed.Type == input$Bed.Type) %>% 
            ggplot(aes(x=Bed.Type,y = price)) + geom_boxplot()+theme_minimal()
        
        
    })
    
    output$barplot <- renderPlot({
        df %>% filter(State %in% input$State) %>% 
            ggplot(mapping = aes(x=State,fill=Room.Type))+
            geom_bar(position = "dodge")+theme_minimal()
        
    })
    
    output$Map <- renderPlotly({
        
        plot_ly(type="choropleth", locations=df6$State, locationmode="USA-states", z=df6$Count)%>% layout(geo=list(scope="usa"))
        
    })
    
    x <- reactive({
        df7[,input$xcol]
    })
    
    y <- reactive({
        df7[,input$ycol]
    })
    output$scat <- renderPlotly({
        plot_ly(data=df7, y = ~y(), x = ~x(),color=~Property.Type,type='scatter',colors = "Set1")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

