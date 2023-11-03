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

# Define shorter names that map to a larger set of column names
crop_names <- list(
  "COFFEE" = c("ACOF_A", "ACOF_P", "ACOF_Y"),
  "BANANA" = c("BANA_A", "BANA_P", "BANA_Y"),
  "BARLEY" = c("BARL_A", "BARL_P", "BARL_Y"),
  "BEAN" = c("BEAN_A", "BEAN_P", "BEAN_Y"),
  "CASSAVA" = c("CASS_A", "CASS_P", "CASS_Y"),
  "CHICKPEA" = c("CHIC_A", "CHIC_P", "CHIC_Y"),
  "COCONUT" = c("CNUT_A", "CNUT_P", "CNUT_Y"),
  "COCOA" = c("COCO_A", "COCO_P", "COCO_Y"),
  "COTTON" = c("COTT_A", "COTT_P", "COTT_Y"),
  "COWPEAS" = c("COWP_A", "COWP_P", "COWP_Y"),
  "GROUNDNUT" = c("GROU_A", "GROU_P", "GROU_Y"),
  "LENTIL" = c("LENT_A", "LENT_P", "LENT_Y"),
  "MAIZE" = c("MAIZ_A", "MAIZ_P", "MAIZ_Y"),
  "OTHER CEREALS" = c("OCER_A", "OCER_P", "OCER_Y"),
  "OTHER FIBERS" = c("OFIB_A", "OFIB_P", "OFIB_Y"),
  "OIL PALM" = c("OILP_A", "OILP_P", "OILP_Y"),
  "OTHER OIL CROPS" = c("OOIL_A", "OOIL_P", "OOIL_Y"),
  "OTHER PULSES" = c("OPUL_A", "OPUL_P", "OPUL_Y"),
  "OTHER ROOTS" = c("ORTS_A", "ORTS_P", "ORTS_Y"),
  "PIGEON PEAS" = c("PIGE_A", "PIGE_P", "PIGE_Y"),
  "PLANTAIN" = c("PLNT_A", "PLNT_P", "PLNT_Y"),
  "PEARL MILLET" = c("PMIL_A", "PMIL_P", "PMIL_Y"),
  "POTATO" = c("POTA_A", "POTA_P", "POTA_Y"),
  "RAPE SEED" = c("RAPE_A", "RAPE_P", "RAPE_Y"),
  "ROBUSTA COFFEE" = c("RCOF_A", "RCOF_P", "RCOF_Y"),
  "REST OF CROPS" = c("REST_A", "REST_P", "REST_Y"),
  "RICE" = c("RICE_A", "RICE_P", "RICE_Y"),
  "SESAME SEED" = c("SESA_A", "SESA_P", "SESA_Y"),
  "SMALL MILLET" = c("SMIL_A", "SMIL_P", "SMIL_Y"),
  "SORGHUM" = c("SORG_A", "SORG_P", "SORG_Y"),
  "SOY BEAN" = c("SOYB_A", "SOYB_P", "SOYB_Y"),
  "SUGAR BEET" = c("SUGB_A", "SUGB_P", "SUGB_Y"),
  "SUGAR CANE" = c("SUGC_A", "SUGC_P", "SUGC_Y"),
  "SUN FLOWER" = c("SUNF_A", "SUNF_P", "SUNF_Y"),
  "SWEET POTATO" = c("SWPO_A", "SWPO_P", "SWPO_Y"),
  "TEA" = c("TEAS_A", "TEAS_P", "TEAS_Y"),
  "TEMPERATE FRUIT" = c("TEMF_A", "TEMF_P", "TEMF_Y"),
  "TOBACCO" = c("TOBA_A", "TOBA_P", "TOBA_Y"),
  "TROPICAL FRUIT" = c("TROF_A", "TROF_P", "TROF_Y"),
  "VEGETABLE" = c("VEGE_A", "VEGE_P", "VEGE_Y"),
  "WHEAT" = c("WHEA_A", "WHEA_P", "WHEA_Y"),
  "YAMS" = c("YAMS_A", "YAMS_P", "YAMS_Y")
)

# Pre-select the first country, farming system
default_country <- unique(All_csv$COUNTRY)[1]
default_farming_system <- unique(All_csv$FARMSYS[All_csv$COUNTRY == default_country])[1]

ui <- fluidPage(
  titlePanel("Geospatial Data For Global Targeting of Investments in Agronomic Gains"),
  tabsetPanel(
    tabPanel("Selection",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("multiple_selected_countries", "Select Countries", 
                                choices = unique(All_csv$COUNTRY), 
                                selected = default_country,
                                multiple = TRUE),
                 selectizeInput("multiple_selected_farming_systems", "Select Farming Systems", 
                                choices = NULL, 
                                selected = default_farming_system,
                                multiple = TRUE),  # Initialize with NULL choices
                 checkboxGroupInput("selected_crops", "Select Crops", 
                                    choices = names(crop_names))
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
                     width = 12,
                     h5("Data Sources"),
                     HTML(' <a href="https://gadm.org/data.html">GADM</a>, <a href="https://data.apps.fao.org/map/catalog/static/api/records/4e463d70-7593-11db-b9b2-000d939bc5d8">FAO</a>, <a href="https://hub.worldpop.org/project/categories?id=3">WorldPop</a>, <a href="https://data.apps.fao.org/catalog/iso/59f7a5ef-2be4-43ee-9600-a6a9e9ff562a">SPAM</a>')
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
                   ),
                   
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
      farming_systems <- unique(All_csv$FARMSYS[All_csv$COUNTRY %in% selected_countries])
      updateSelectInput(session, "multiple_selected_farming_systems", choices = farming_systems, selected = default_farming_system)
    }
  })
  
  # Filter data based on multiple selected countries and farming systems for multiple selection
  multiple_filtered_data <- reactive({
    All_csv[All_csv$COUNTRY %in% input$multiple_selected_countries &
              All_csv$FARMSYS %in% input$multiple_selected_farming_systems, ]
  })
  
  # Filter data based on selected crops
  filtered_data <- reactive({
    selected_crops <- input$selected_crops
    dataset_columns <- unlist(crop_names[selected_crops])
    dataset_columns <- c("COUNTRY", "ISO3", "AREA_SQKM", "FARMSYS", "RURAL_POP", "POP_DENSIT", dataset_columns)
    dataset_columns <- which(names(multiple_filtered_data()) %in% dataset_columns) # Get column indices
    multiple_filtered_data() %>%
      select(all_of(dataset_columns))
  })
  
  # Render the filtered data table for multiple selection
  output$data_table_multiple <- renderTable({
    if (!is.null(input$multiple_selected_countries) && 
        !is.null(input$multiple_selected_farming_systems)) {
      filtered_data()
    }
  })
  
  # Render the map based on selected country and farming system for multiple selection
  output$map_multiple <- renderLeaflet({
    selected_country_multiple <- input$multiple_selected_countries
    selected_farming_system_multiple <- input$multiple_selected_farming_systems
    
    if (!is.null(selected_country_multiple) && !is.null(selected_farming_system_multiple)) {
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
          !is.null(input$multiple_selected_farming_systems)) {
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


