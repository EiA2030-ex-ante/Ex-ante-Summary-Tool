library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(leaflet)
library(DT)

# Load the shapefile with countries, farming systems, population, and spam data
All_shapefile <- sf::read_sf("spam_RuralPop_Country_Farmsyst.shp")
All_csv <- read.csv("FarmSys_pop_spam_world.csv")
variable_descriptions <- read.csv("variables_description.csv")

# Pre-select the first country, farming system, and crop
default_country <- unique(All_csv$country)[1]
default_farming_system <- unique(All_csv$FARMSYS[All_csv$country == default_country])[1]
default_crop <- "ACOF_A"

ui <- fluidPage(
  titlePanel("Geospatial Data For Global Targeting of Investments in Agronomic Gains"),
  tabsetPanel(
    tabPanel("Selection",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("multiple_selected_countries", "Select Countries", 
                                choices = unique(All_csv$country), 
                                selected = default_country,
                                multiple = TRUE),
                 selectizeInput("multiple_selected_farming_systems", "Select Farming Systems", 
                                choices = NULL, 
                                selected = default_farming_system,
                                multiple = TRUE),  # Initialize with NULL choices
                 checkboxGroupInput("selected_crops", "Select Crops", 
                                    choices = sort(c(
                                      "ACOF_A", "ACOF_P", "ACOF_Y",
                                      "BANA_A", "BANA_P", "BANA_Y",
                                      "BARL_A", "BARL_P", "BARL_Y",
                                      "BEAN_A", "BEAN_P", "BEAN_Y",
                                      "CASS_A", "CASS_P", "CASS_Y",
                                      "CHIC_A", "CHIC_P", "CHIC_Y",
                                      "CNUT_A", "CNUT_P", "CNUT_Y",
                                      "COCO_A", "COCO_P", "COCO_Y",
                                      "COTT_A", "COTT_P", "COTT_Y",
                                      "COWP_A", "COWP_P", "COWP_Y",
                                      "GROU_A", "GROU_P", "GROU_Y",
                                      "LENT_A", "LENT_P", "LENT_Y",
                                      "MAIZ_A", "MAIZ_P", "MAIZ_Y",
                                      "OCER_A", "OCER_P", "OCER_Y",
                                      "OFIB_A", "OFIB_P", "OFIB_Y",
                                      "OILP_A", "OILP_P", "OILP_Y",
                                      "OOIL_A", "OOIL_P", "OOIL_Y",
                                      "OPUL_A", "OPUL_P", "OPUL_Y",
                                      "ORTS_A", "ORTS_P", "ORTS_Y",
                                      "PIGE_A", "PIGE_P", "PIGE_Y",
                                      "PLNT_A", "PLNT_P", "PLNT_Y",
                                      "PMIL_A", "PMIL_P", "PMIL_Y",
                                      "POTA_A", "POTA_P", "POTA_Y",
                                      "RAPE_A", "RAPE_P", "RAPE_Y",
                                      "RCOF_A", "RCOF_P", "RCOF_Y",
                                      "REST_A", "REST_P", "REST_Y",
                                      "RICE_A", "RICE_P", "RICE_Y",
                                      "SESA_A", "SESA_P", "SESA_Y",
                                      "SMIL_A", "SMIL_P", "SMIL_Y",
                                      "SORG_A", "SORG_P", "SORG_Y",
                                      "SOYB_A", "SOYB_P", "SOYB_Y",
                                      "SUGB_A", "SUGB_P", "SUGB_Y",
                                      "SUGC_A", "SUGC_P", "SUGC_Y",
                                      "SUNF_A", "SUNF_P", "SUNF_Y",
                                      "SWPO_A", "SWPO_P", "SWPO_Y",
                                      "TEAS_A", "TEAS_P", "TEAS_Y",
                                      "TEMF_A", "TEMF_P", "TEMF_Y",
                                      "TOBA_A", "TOBA_P", "TOBA_Y",
                                      "TROF_A", "TROF_P", "TROF_Y",
                                      "VEGE_A", "VEGE_P", "VEGE_Y",
                                      "WHEA_A", "WHEA_P", "WHEA_Y",
                                      "YAMS_A", "YAMS_P", "YAMS_Y"
                                    )),
                                    selected = default_crop)  # Pre-select first crop
               ),
               mainPanel(
                 tableOutput("data_table_multiple"),
                 leafletOutput("map_multiple"),
                 downloadButton("downloadCSV_multiple", "Download CSV (Selection)")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 img(src = "https://s3.amazonaws.com/eventtia/event_logos/31142/medium/eialogovrgb16569197111656919711.png?1656919711", width = "10%"),
                 img(src = "https://www.cgiar.org/wp/wp-content/themes/cgiar/assets/images/logo-04dd455e58-04dd455e58.png", width = "10%")
               )
             )
    ),
    tabPanel("About",
             fluidRow(
               column(
                 width = 12,
                 h4("About this App"),
                 HTML("This decision support tool was developed to support the identification of targeting objectives (e.g., number of targeted beneficiaries) and associated initial ex ante impact estimates for agronomy projects in cases where detailed data are not available. This work was developed as part of Excellence in Agronomy (EiA), one of the initiatives of <a href='https://www.cgiar.org/food-security-impact/one-cgiar/'>One CGIAR</a>. Code used in assembly of this tool is available at the <a href='https://github.com/EiA2030-ex-ante'>EiA GitHub Repository</a>."
                 ),
                 h5("Variable Descriptions"),
                 div(style = "overflow-x: auto;",
                     DTOutput("variable_descriptions_table")
                 ),
                 fluidRow(
                   column(
                     width = 6, 
                     img(src = "https://www.cgiar.org/wp/wp-content/themes/cgiar/assets/images/logo-04dd455e58-04dd455e58.png", width = "10%")
                   ),
                   column(
                     width = 6,
                     img(src = "https://s3.amazonaws.com/eventtia/event_logos/31142/medium/eialogovrgb16569197111656919711.png?1656919711", width = "10%")
                   )
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Update the choices for the "Select Farming System" dropdown based on the selected country
  observe({
    selected_countries <- input$multiple_selected_countries
    if (!is.null(selected_countries)) {
      farming_systems <- unique(All_csv$FARMSYS[All_csv$country %in% selected_countries])
      updateSelectInput(session, "multiple_selected_farming_systems", choices = farming_systems, selected = default_farming_system)
    }
  })
  
  # Filter data based on multiple selected countries and farming systems for multiple selection
  multiple_filtered_data <- reactive({
    All_csv[All_csv$country %in% input$multiple_selected_countries &
              All_csv$FARMSYS %in% input$multiple_selected_farming_systems, ]
  })
  
  # Filter data based on selected crops
  filtered_data <- reactive({
    dataset_columns <- input$selected_crops
    multiple_filtered_data() %>%
      select(country, iso3, area_sqkm, FARMSYS, Rural_Pop, pop_densit, !!!dataset_columns)
  })
  
  # Render the filtered data table for multiple selection
  output$data_table_multiple <- renderTable({
    if (!is.null(input$multiple_selected_countries) && 
        !is.null(input$multiple_selected_farming_systems) &&
        !is.null(input$selected_crops)) {
      filtered_data()
    }
  })
  
  # Render the map based on selected country and farming system for multiple selection
  output$map_multiple <- renderLeaflet({
    selected_country_multiple <- input$multiple_selected_countries
    selected_farming_system_multiple <- input$multiple_selected_farming_systems
    
    if (!is.null(selected_country_multiple) && !is.null(selected_farming_system_multiple) && !is.null(input$selected_crops)) {
      filtered_shape_multiple <- All_shapefile %>%
        filter(country %in% selected_country_multiple, FARMSYS %in% selected_farming_system_multiple)
      
      leaflet(data = filtered_shape_multiple) %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addPolygons(stroke = TRUE,
                    fillColor = "blue", 
                    fillOpacity = 0.5, 
                    color = "white", 
                    weight = 1,
                    popup = ~glue::glue("<b>{FARMSYS}</b><br>{area_sqkm}"))
    }
  })
  
  # Define a download handler for the download button for multiple selection
  output$downloadCSV_multiple <- downloadHandler(
    filename = function() {
      "Filtered_data_multiple.csv"
    },
    content = function(file) {
      if (!is.null(input$multiple_selected_countries) && 
          !is.null(input$multiple_selected_farming_systems) &&
          !is.null(input$selected_crops)) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    }
  )
  
  # Render the variable descriptions table
  output$variable_descriptions_table <- renderDT({
    datatable(variable_descriptions, 
              options = list(pageLength = 10, paging = TRUE))
  })
}

shinyApp(ui, server)
