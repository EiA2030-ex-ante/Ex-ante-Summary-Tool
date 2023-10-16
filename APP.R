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


ui <- fluidPage(
  titlePanel("Geospatial Data For Global Targeting of Investments in Agronomic Gains"),
  tabsetPanel(
    tabPanel("Selection",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("multiple_selected_countries", "Select Countries", 
                                choices = unique(All_csv$country), multiple = TRUE),
                 selectizeInput("multiple_selected_farming_systems", "Select Farming Systems", 
                                choices = NULL, multiple = TRUE),  # Initialize with NULL choices
                 radioButtons("selected_crop", "Select Crop", 
                              choices = c(
                                "ACOF_A", "BANA_A", "BARL_A", "BEAN_A", "CASS_A", "CHIC_A", "CNUT_A", "COCO_A", 
                                "COTT_A", "COWP_A", "GROU_A", "LENT_A", "MAIZ_A", "OCER_A", "OFIB_A", "OILP_A", 
                                "OOIL_A", "OPUL_A", "ORTS_A", "PIGE_A", "PLNT_A", "PMIL_A", "POTA_A", "RAPE_A", 
                                "RCOF_A", "REST_A", "RICE_A", "SESA_A", "SMIL_A", "SORG_A", "SOYB_A", "SUGB_A", 
                                "SUGC_A", "SUNF_A", "SWPO_A", "TEAS_A", "TEMF_A", "TOBA_A", "TROF_A", "VEGE_A", 
                                "WHEA_A", "YAMS_A", "ACOF_P", "BANA_P", "BARL_P", "BEAN_P", "CASS_P", "CHIC_P", 
                                "CNUT_P", "COCO_P", "COTT_P", "COWP_P", "GROU_P", "LENT_P", "MAIZ_P", "OCER_P", 
                                "OFIB_P", "OILP_P", "OOIL_P", "OPUL_P", "ORTS_P", "PIGE_P", "PLNT_P", "PMIL_P", 
                                "POTA_P", "RAPE_P", "RCOF_P", "REST_P", "RICE_P", "SESA_P", "SMIL_P", "SORG_P", 
                                "SOYB_P", "SUGB_P", "SUGC_P", "SUNF_P", "SWPO_P", "TEAS_P", "TEMF_P", "TOBA_P", 
                                "TROF_P", "VEGE_P", "WHEA_P", "YAMS_P", "ACOF_Y", "BANA_Y", "BARL_Y", "BEAN_Y", 
                                "CASS_Y", "CHIC_Y", "CNUT_Y", "COCO_Y", "COTT_Y", "COWP_Y", "GROU_Y", "LENT_Y", 
                                "MAIZ_Y", "OCER_Y", "OFIB_Y", "OILP_Y", "OOIL_Y", "OPUL_Y", "ORTS_Y", "PIGE_Y", 
                                "PLNT_Y", "PMIL_Y", "POTA_Y", "RAPE_Y", "RCOF_Y", "REST_Y", "RICE_Y", "SESA_Y", 
                                "SMIL_Y", "SORG_Y", "SOYB_Y", "SUGB_Y", "SUGC_Y", "SUNF_Y", "SWPO_Y", "TEAS_Y", 
                                "TEMF_Y", "TOBA_Y", "TROF_Y", "VEGE_Y", "WHEA_Y", "YAMS_Y"
                              )),
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
      updateSelectInput(session, "multiple_selected_farming_systems", choices = farming_systems)
    }
  })
  
  # Filter data based on multiple selected countries and farming systems for multiple selection
  multiple_filtered_data <- reactive({
    All_csv[All_csv$country %in% input$multiple_selected_countries &
              All_csv$FARMSYS %in% input$multiple_selected_farming_systems, ]
  })
  
  # Filter data based on selected crop
  filtered_data <- reactive({
    dataset_column <- input$selected_crop
    multiple_filtered_data() %>%
      select(country, iso3, area_sqkm, FARMSYS, Rural_Pop, pop_densit, dataset_column)
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
