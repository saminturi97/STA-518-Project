library(tidyverse)
airbnb <- read.csv("airbnb-listings-2.csv", sep = ";")

# We will select important variables for our analysis and remove the missing values.

df <- airbnb %>% 
    select(Name, City, State, Zipcode, Market, Country.Code, Country, Latitude, Longitude, Property.Type, Room.Type, Accommodates, Bathrooms, Bedrooms, Beds, Bed.Type, Price, Guests.Included, Geolocation)

df <- df[complete.cases(df), ]
df$Bed.Type <- factor(as.character(df$Bed.Type))
df$Price <- as.double(df$Price)
df1 <- data.frame(price = df$Price, Bed.Type = df$Bed.Type)

#  Data preparation for US Map


library(usmap)
library(ggplot2)
library(rgdal)


df3 <- df %>% filter(Country.Code == "US") %>% group_by(State) %>% summarise(count = n())
df4 <- statepop %>% left_join(df3, by = c("abbr" = "State"))
df4 <- df4[complete.cases(df4), ]





library(shiny)

# Define UI for application 
ui <- fluidPage( theme = shinytheme("darkly"),
    titlePanel("Airbnb data Exploration"),
    tabsetPanel(
        tabPanel("Plot for Accommodates vs Bedrooms ",
                 sidebarLayout(
                     sidebarPanel(selectInput("Property.Type", "Property.Type", choices = unique(df$Property.Type), selected = "Guesthouse")),
                     mainPanel(
                         plotOutput("scatter_plot")
                     )
                 )
        ),
        tabPanel("Boxplot of Price for different Bed types",
                 sidebarLayout(
                     sidebarPanel(selectInput("Bed.Type", "Bed.Type:", choices = unique(df$Bed.Type), selected = "Futon")),
                     mainPanel(
                         plotOutput("boxplot")
                     )
                 )
        ),
        
        tabPanel("Bar plot of Room type for States",
                 sidebarLayout(
                     sidebarPanel(checkboxGroupInput("State", "State:", choices = unique(df$State), selected = unique(df$State)[1:3])),
                     mainPanel(
                         plotOutput("barplot")
                     )
                 )
        ),
        tabPanel("US map for Number of Hotels",
                 
                 mainPanel(
                     plotOutput("Map")
                 )
                 
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    output$scatter_plot <- renderPlot({
        df %>% filter(Property.Type == input$Property.Type) %>% 
            ggplot(aes(x = Accommodates, y = Bedrooms, colour = Room.Type, size = Beds)) + geom_point()+theme_minimal()
        
        
    })
    
    output$boxplot <- renderPlot({
        df1 %>% filter(Bed.Type == input$Bed.Type) %>% 
            ggplot(aes(y = price)) + geom_boxplot()+ coord_flip() + theme_minimal()
        
        
    })
    
    output$barplot <- renderPlot({
        df %>% filter(State %in% input$State) %>% ggplot(mapping = aes(x=State,fill=Room.Type))+
            geom_bar(position = "dodge")+theme_minimal()
        
    })
    
    output$Map <- renderPlot({
        plot_usmap(data = df4, values = "count", color = "blue") + 
            scale_fill_continuous(
                low = "white", high = "red", name = "Number of Hotels", label = scales::comma
            ) + theme(legend.position = "right")
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
