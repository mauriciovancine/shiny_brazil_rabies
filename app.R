
# load packages -----------------------------------------------------------

if (!require(shiny)) install.packages("shiny")
if (!require(leaflet)) install.packages("leaflet")
if (!require(sf)) install.packages("sf")
if (!require(shinyjs)) install.packages("shinyjs")

# load the dataset and build user interface UI ----------------------------

# load the dataset
spatial_data <- st_read("00_data/00_mun_data.gpkg") |>
  st_transform(4326)
spatial_data

# create a tab in the top containing info about the project
ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Inferindo a circulação do vírus da raiva e risco zoonótico com aplicações para a saúde pública"),
  
  div(
    style = "background-color: #f5f5f5; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
    fluidRow(
      column(6,
             HTML("<strong>Processo:</strong> 24/07056-6<br>"),
             HTML("<strong>Modalidade de apoio:</strong> Bolsas no Brasil - Iniciação Científica<br>"),
             HTML("<strong>Data de Início da vigência:</strong> 01 de julho de 2024<br>"),
             HTML("<strong>Data de Término da vigência:</strong> 30 de setembro de 2025<br>"),
             HTML("<strong>Área de conhecimento:</strong> Ciências Biológicas - Ecologia")
      ),
      column(6,
             HTML("<strong>Pesquisador responsável:</strong> Milton Cezar Ribeiro<br>"),
             HTML("<strong>Beneficiário:</strong> Maria Eduarda Furlan<br>"),
             HTML("<strong>Instituição Sede:</strong> Instituto de Biociências (IB). Universidade Estadual Paulista (UNESP). Campus de Rio Claro. Rio Claro , SP, Brasil<br>"),
             HTML("<strong>Bolsa(s) vinculada(s):</strong> 24/20504-8 - Priorização espacial do risco zoonótico da raiva e aplicações para a saúde pública no Brasil, BE.EP.IC")
      )
    )
  ),
  
  # add an option to select state, can only select one state at a time.  
  fluidRow(
    column(8, 
           selectInput("state_select", 
                       "Selecione o Estado:", 
                       choices = NULL,
                       selected = NULL),
           leafletOutput("map", height = "75vh")
    ),
    
    # tab where info about the select municipe will be displayed
    column(4, 
           h4("Dados do Município"),
           div(id = "loading", style = "display: none; text-align: center; padding: 20px;",
               tags$div(class = "spinner", style = "border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 1s linear infinite; margin: 0 auto;"),
               tags$p("Carregando dados do município..."),
               tags$style(HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"))
           ),
           verbatimTextOutput("municipal_data"),
           
           # add two options, one to download the information of the selected state and another one to download everything
           div(style = "margin-top: 20px;",
               h4("Baixar Dados"),
               downloadButton("download_all", 
                              "Baixar Dataset Completo", 
                              class = "btn btn-primary",
                              style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_state", 
                              "Baixar Dados do Estado Selecionado", 
                              class = "btn btn-success",
                              style = "width: 100%;")
           )
    )
  ),
  
  # Add logos section at the bottom
  br(),
  hr(),
  div(
    style = "background-color: #ffffff; padding: 20px; margin-top: 30px; text-align: center; border-top: 2px solid #e5e5e5;",
    h5("Apoio Institucional", style = "margin-bottom: 20px; color: #666;"),
    fluidRow(
      column(3,
             div(style = "text-align: center;",
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/0/0a/Logo_Unesp.svg", 
                     alt = "UNESP", 
                     style = "max-height: 80px; max-width: 100%; object-fit: contain;")
             )
      ),
      column(3,
             div(style = "text-align: center;",
                 img(src = "https://scontent.fcpq5-1.fna.fbcdn.net/v/t39.30808-6/300404000_506547754611842_924131957748489075_n.jpg?_nc_cat=102&ccb=1-7&_nc_sid=6ee11a&_nc_eui2=AeEd7f1yur83TUS9IBcfvsTRXaj2RscBpjZdqPZGxwGmNqRgs1sZpHfPgW9fGdnAsRorCzV40OzbeLDjJNDXsHXm&_nc_ohc=NyQtim2ljOQQ7kNvwGhf0Xk&_nc_oc=Adlodt_lsl9-ZCAcz7tLZjzGXrG0fSY1uhsmHGuxFk9Qlp1Jw5wfyaKsoC71i1iXBL1jpQPYvaG2ltKTj8BSSf1e&_nc_zt=23&_nc_ht=scontent.fcpq5-1.fna&_nc_gid=_Z8mO-nSnX45xpFY_2YhUQ&oh=00_AfMVS3yUjHJu7FrCVt77gYr8QdwITLaGrXJPR_Ml0hiAJg&oe=68607DD8", 
                     alt = "Instituto de Biociências", 
                     style = "max-height: 80px; max-width: 100%; object-fit: contain;")
             )
      ),
      column(3,
             div(style = "text-align: center;",
                 img(src = "https://ib.rc.unesp.br/Home/Departamentos47/ecologia/logo-leec.jpg", 
                     alt = "LEEC", 
                     style = "max-height: 80px; max-width: 100%; object-fit: contain;")
             )
      ),
      column(3,
             div(style = "text-align: center;",
                 img(src = "https://www.fcf.unicamp.br/wp-content/uploads/2022/05/fapesp.png", 
                     alt = "FAPESP", 
                     style = "max-height: 80px; max-width: 100%; object-fit: contain;")
             )
      )
    )
  )
)

# build reactive server-side ----------------------------------------------

server <- function(input, output, session) {
  
  output$municipal_data <- renderText({
    "Clique em um município para ver seus dados"
  })
  
  # It starts by showing a selected state. I chose Amazonas because it only has a few states and loads the app quickly
  observe({
    state_choices <- sort(unique(spatial_data$NM_UF))
    updateSelectInput(session, "state_select", choices = state_choices, selected = "Amazonas")
  })
  
  # Filter data based on selected state
  filtered_data <- reactive({
    req(input$state_select)
    spatial_data[spatial_data$NM_UF == input$state_select, ]
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles("CartoDB.Positron")
  })
  
  # Update map when state selection changes
  observeEvent(input$state_select, {
    req(input$state_select)
    
    leafletProxy("map") |>
      clearShapes() |>
      addPolygons(
        data = filtered_data(),
        fillColor = "steelblue",
        color = "white",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.6,
        label = ~NM_MUN,
        layerId = ~NM_MUN
      ) |>
      addProviderTiles("CartoDB.Positron", group = "Light") |>
      addProviderTiles("OpenStreetMap", group = "Street") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") |>
      addProviderTiles("Esri.WorldTopoMap", group = "Terrain") |>
      addLayersControl(
        baseGroups = c("Light", "Street", "Satellite", "Dark", "Terrain"),
        options = layersControlOptions(collapsed = FALSE, position = "topright")
      ) |>
      fitBounds(
        lng1 = as.numeric(st_bbox(filtered_data())[1]),
        lat1 = as.numeric(st_bbox(filtered_data())[2]),
        lng2 = as.numeric(st_bbox(filtered_data())[3]),
        lat2 = as.numeric(st_bbox(filtered_data())[4])
      )
  })
  
  observeEvent(input$map_shape_click, {
    clicked_municipality <- input$map_shape_click$id
    
    # Show loading animation - so the user will know the data is still being loaded 
    shinyjs::show("loading")
    shinyjs::hide("municipal_data")
    
    # Change the color of the selected state to darkgreen. There is a problem when switching colors, a delay where everything disappears and appears again. 
    # I believe it happens because the leaflet is rendered again.
    leafletProxy("map") |>
      clearShapes() |>
      addPolygons(
        data = filtered_data(),
        fillColor = ~ifelse(NM_MUN == clicked_municipality, "darkgreen", "steelblue"),
        color = "white",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.6,
        label = ~NM_MUN,
        layerId = ~NM_MUN
      )
    
    # Get data for clicked municipality, I decided to display columns 14-41 but you can add more.
    municipal_info <- filtered_data()[filtered_data()$NM_MUN == clicked_municipality, 14:41]
    
    output$municipal_data <- renderText({
      if (!is.null(clicked_municipality)) {
        
        # Convert to data frame and remove geometry for display
        data_df <- st_drop_geometry(municipal_info)
        
        # Create formatted output with each variable on its own line
        formatted_output <- paste0(
          "Município: ", clicked_municipality, "\n\n",
          paste(
            names(data_df), ": ", 
            as.character(data_df[1, ]), 
            collapse = "\n"
          )
        )
        
        shinyjs::hide("loading")
        shinyjs::show("municipal_data")
        
        formatted_output
      } else {
        shinyjs::hide("loading")
        shinyjs::show("municipal_data")
        "Clique em um município para ver seus dados"
      }
    })
  })
  
  # Download handlers
  output$download_all <- downloadHandler(
    filename = function() {
      paste("dados_municipais_completos_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      # Remove geometry column for CSV export
      data_to_export <- st_drop_geometry(spatial_data)
      write.csv(data_to_export, file, row.names = FALSE)
    }
  )
  
  output$download_state <- downloadHandler(
    filename = function() {
      req(input$state_select)
      paste(input$state_select, "_dados_municipais_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$state_select)
      
      # Remove geometry column for CSV export
      data_to_export <- st_drop_geometry(filtered_data())
      write.csv(data_to_export, file, row.names = FALSE)
    }
  )
}


# execute the app ---------------------------------------------------------


shinyApp(ui, server)