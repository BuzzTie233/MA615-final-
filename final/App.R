library(shiny)
library(leaflet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

# Load and preprocess data
madagascar_data <- read_csv("data_new/df993292-7ac5-4c8b-9730-79a2610df483_Data.csv")

# Clean column names
colnames(madagascar_data)[5:ncol(madagascar_data)] <- gsub("\\s\\[YR\\d{4}\\]", "", colnames(madagascar_data)[5:ncol(madagascar_data)])

# Define the indicators of interest
selected_indicators <- c('GDP (current US$)', 'GDP growth (annual %)', 
                         'GDP per capita (current US$)', 'Population, total', 
                         'Population growth (annual %)', 'Life expectancy at birth, total (years)')

# Filter and reshape data
filtered_data <- madagascar_data |> 
  filter(`Series Name` %in% selected_indicators) |> 
  select(-c(`Country Name`, `Country Code`, `Series Code`))

long_data <- filtered_data |> 
  pivot_longer(cols = `1974`:`2019`, names_to = "Year", values_to = "Value") |> 
  mutate(
    Year = as.numeric(Year), 
    Value = as.numeric(gsub("[^0-9.-]", "", Value))
  )

# UI part
ui <- fluidPage(
  titlePanel("Madagascar Analysis"),
  
  navbarPage("Navigator",
             
             # Introduction tab
             tabPanel("Introduction", 
                      fluidRow(
                        column(6, 
                               h3("About Madagascar"),
                               p("Madagascar is an island nation in the Indian Ocean off the southeastern coast of Africa, known for its unique ecosystem and diverse biological species. Madagascar has a strategic location along the Mozambique Channel. The government system is a republic; the chief of state is the president, and the head of government is the prime minister. Madagascar transitioned from socialist economic policies to a market economy with policies of privatization and liberalization. Madagascar is a member of the Common Market for Eastern and Southern Africa (COMESA) and the Southern African Development Community (SADC)."),
                               p("Capital City: Antananarivo"),
                               p("Currency: Malagasy ariary (MGA)"),
                               p("Land area: 587,041 square kilometers")
                        ),
                        column(6, 
                               img(src = "madagascar.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        ),
                        column(6,
                               img(src = "Bread tree.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
                      )
             ),
             
             # Biodiversity tab
             tabPanel("Biodiversity",
                      fluidRow(
                        column(12,
                               h3("Biodiversity in Madagascar"),
                               p("Madagascar has a unique biodiversity due to its long period of geographical isolation, where numerous endemic species such as lemurs, endemic plants, birds and reptiles thrive. This ecosystem is of global importance for nature conservation. Biodiversity formation in Madagascar is associated with precipitation-driven erosion, land retreat in high relief topography, and habitat isolation and reconnection. With its rich vegetation types of dry forests, humid forests, spiny forests and grassland-woodland interspersed zones, the island is one of the most important biodiversity hotspots in the world. About 80% or more of the vascular plant species are unique to the island. However, global climate change, the uncontrolled expansion of farming activities and the overexploitation of natural resources are putting unprecedented pressure on Madagascar's biodiversity.")
                        ),
                        column(6, 
                               img(src = "M.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        ),
                        column(6, 
                               img(src = "H.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
                      )
             ),
             
             # Map tab
             tabPanel("Map", 
                      leafletOutput("madagascarMap", height = 600)
             ),
             
             # Economic indicators tab
             tabPanel("Economic indicators", 
                      sidebarLayout(
                        sidebarPanel(
                          # English names as internal values
                          selectInput("selectedIndicator", "Selected Indicator:", 
                                      choices = c("Total GDP" = "GDP (current US$)", 
                                                  "GDP growth rate" = "GDP growth (annual %)")),
                          sliderInput("yearRange", "Select Year:", 
                                      min = 1974, max = 2019, value = c(2000, 2019))
                        ),
                        mainPanel(
                          plotOutput("gdpPlot")
                        )
                      )
             ),
             
             # Population Statistics tab
             tabPanel("Population Statistics", 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("chartType", "Select chart type:", 
                                       choices = c("Population change", "Expected Age")),
                          sliderInput("yearRangePop", "Select Year:", 
                                      min = 1974, max = 2019, value = c(2000, 2019))
                        ),
                        mainPanel(
                          plotOutput("populationPlot")
                        )
                      )
             ),
             
             # SWOT Analysis tab
             tabPanel("SWOT Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          # Dropdown to select SWOT category
                          selectInput("swotCategory", "Select Category:", 
                                      choices = c("Strengths", "Weaknesses", "Opportunities", "Threats"))
                        ),
                        mainPanel(
                          h3("SWOT analysis of Madagascar's Tourism sector"),
                          
                          # Conditional panels display different content depending on user choice
                          conditionalPanel(
                            condition = "input.swotCategory == 'Strengths'",
                            h4("KEY STRENGTHS"),
                            tags$ul(
                              tags$li("One of the world's top 'biodiversity hotspots'"),
                              tags$li("Rich natural heritage: more than 80% of Madagascar's flora and fauna are found nowhere else in the world, and some taxonomic groups (including reptiles and amphibians) are over 95% endemic"),
                              tags$li("Coastal attractions")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.swotCategory == 'Opportunities'",
                            h4("KEY OPPORTUNITIES"),
                            tags$ul(
                              tags$li("Investment finance through the 'Funds For the Promotion of Private Business'"),
                              tags$li("Tourism product diversification e.g. adventure, special interest, etc."),
                              tags$li("Establishment of ecotourism investment zones"),
                              tags$li("Emerging markets in Africa and Asia")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.swotCategory == 'Weaknesses'",
                            h4("KEY WEAKNESSES"),
                            tags$ul(
                              tags$li("Lack of supportive policies and regulations"),
                              tags$li("Investment climate not conducive to tourism development"),
                              tags$li("Weak country credit rating"),
                              tags$li("Unfavorable labour relations in the hospitality sector"),
                              tags$li("Poor airline connectivity"),
                              tags$li("Inadequately skilled labour force"),
                              tags$li("Inadequate funding for tourism initiatives"),
                              tags$li("Stiff competition from neighbouring tourism destinations such as Seychelles and Mauritius")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.swotCategory == 'Threats'",
                            h4("KEY THREATS"),
                            tags$ul(
                              tags$li("Lack of an 'open skies' policy"),
                              tags$li("Perceptions of poor governance and political instability"),
                              tags$li("High degree of environmental and forest degradation"),
                              tags$li("Climate change"),
                              tags$li("Disaster vulnerability (e.g. drought, cyclones, flooding, etc.)")
                            )
                          )
                        )
                      )
             )
  )
)

# Server logic
server <- function(input, output) {
  
  # Render dynamic map
  output$madagascarMap <- renderLeaflet({
    madagascar_coords <- c(-18.7669, 46.8691)
    leaflet() |> 
      addProviderTiles(providers$OpenStreetMap) |> 
      setView(lng = madagascar_coords[2], lat = madagascar_coords[1], zoom = 6) |> 
      addMarkers(lng = 47.5361, lat = -18.8792, popup = "Antananarivo (Capital City)") |> 
      addMarkers(lng = 48.2732, lat = -13.0027, popup = "Diego Suarez (Major port city)") |> 
      addMarkers(lng = 49.3832, lat = -17.8239, popup = "Toamasina (Second largest city)") 
  })
  
  # Generate GDP plot
  output$gdpPlot <- renderPlot({
    req(input$selectedIndicator)
    selected_data <- long_data |> 
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2], 
             `Series Name` == input$selectedIndicator)
    
    ggplot(data = selected_data, aes(x = Year, y = Value)) +
      geom_col(fill = "#D00000") +
      labs(title = paste("Indicator:", input$selectedIndicator), 
           x = "Year", y = input$selectedIndicator) +
      theme_minimal()
  })
  
  # Generate population plot
  output$populationPlot <- renderPlot({
    population_data <- long_data |> 
      filter(Year >= input$yearRangePop[1], Year <= input$yearRangePop[2])
    
    switch(input$chartType,
           "Population change" = {
             data <- population_data |> 
               filter(`Series Name` == "Population, total")
             ggplot(data, aes(x = Year, y = Value)) +
               geom_line(color = "#0077B6") +
               labs(title = "Population Change Trend", x = "Year", y = "Total Population") +
               theme_minimal()
           },
           
           "Expected Age" = {
             data <- population_data |> 
               filter(`Series Name` == "Life expectancy at birth, total (years)")
             ggplot(data, aes(x = Year, y = Value)) +
               geom_line(color = "#6A0572") +
               labs(title = "Life expectancy in Madagascar", x = "Year", y = "Life expectancy (years)") +
               theme_minimal()
           }
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

