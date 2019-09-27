library(shiny) ; library(shinydashboard) ; library(shinyWidgets) ; library(tidyverse) ; library(sf) ; library(leaflet) ; library(htmltools) ; library(htmlwidgets) ; library(ggiraph) ; library(scales)

df <- read.csv("data/imd.csv") %>% 
  mutate(decile = factor(decile),
         rank = as.integer(rank))

lsoa <- st_read("data/best_fit_lsoa.geojson")

localities <- st_read("data/localities.geojson")

ui <- fluidPage(
  tags$head(includeCSS("styles.css")),
  titlePanel(
    div(
      class = "headerContainer",
      a(
        img(
          src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png",
          style = "position: relative; top: -5px;",
          height = 60
        ),
        href = "https://www.trafford.gov.uk",
        target = "_blank"
      ),
      "Indices of Deprivation, 2019"
    ),
    windowTitle = "imd19"
  ),
  fluidRow( 
    includeHTML("intro.html")
    ),
  fluidRow(
    br(),
    div(class = "col-md-4",
        box(width = '100%', 
            radioButtons(inputId = "year", label = NULL,
                         choices = list("2019" = 2019, "2015" = 2015), 
                         selected = 2019,
                         inline = TRUE),
            radioButtons(inputId = "domain", label = NULL,
                         choices = c("Index of Multiple Deprivation", "Income", "Employment", "Education, Skills and Training", 
                                     "Health Deprivation and Disability", "Crime", "Barriers to Housing and Services", "Living Environment",
                                     "Income Deprivation Affecting Children", "Income Deprivation Affecting Older People"),
                         selected = "Index of Multiple Deprivation"))
    ),
    div(class = "col-md-4",
        box(width = '100%', 
            ggiraphOutput("bar"))
    ),
    div(class = "col-md-4",
        box(width = '100%', 
            leafletOutput("map"),
            div(
              style = "position: absolute; left: 3em; bottom: 3.5em;",
              dropdown(
                checkboxInput("localities", label = "Add localities", value = FALSE),
                icon = icon("cog"),
                size = "xs",
                style = "jelly",
                width = "180px",
                up = TRUE
              ),
            tags$style(
              HTML(
                '.fa {color: #212121;}
          .bttn-jelly.bttn-default{color:#f0f0f0;}
          .bttn-jelly:hover:before{opacity:1};'
              )))
    ))
    )
)
  

server <- function(input, output){
  
  domain <- reactive({
    lsoa <- left_join(lsoa, filter(df, index_domain == input$domain), by = "lsoa11cd") %>% 
      filter(year == input$year)
  })
  
  output$map <- renderLeaflet({
    
    validate(need(nrow(domain()) != 0, message = FALSE))
    
    bbox <- st_bbox(lsoa) %>% as.vector()
    
    labels <- 
      paste0(
        "<strong>", domain()$lsoa11nm, "</strong>", paste0(" (", domain()$wd18nm, ")"), "<br />",
       # "IMD Score: ", domain()$score, "<br/>",
        "IMD Rank: ", comma(domain()$rank), "<br/>",
        "IMD Decile: ", domain()$decile
      ) %>% 
      lapply(htmltools::HTML)    
    
    pal <- colorFactor(c("#453B52", "#454F69", "#3F657E", "#317B8D", "#239296", "#26A898", "#43BD93", "#6AD189", "#98E37D", "#CAF270"), domain = 1:10, ordered = TRUE)
    
    
    if (input$localities) {
  
      
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addTiles(
        urlTemplate = "",
        attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2019)</a>',
        options = tileOptions(minZoom = 11, maxZoom = 17)
      ) %>%
        addMapPane("lsoa", zIndex = 410) %>% 
        addMapPane("localities", zIndex = 450) %>% 
      addPolygons(data = domain(), 
                  fillColor = ~pal(decile), 
                  weight = 1,  opacity = 1, color = "#FFF", dashArray = "1", fillOpacity = 1,
                  highlight = highlightOptions(
                    weight = 2, color = "#000", fillOpacity = 1, bringToFront = TRUE
                    ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = 'top',
                    offset=c(0,-5)),
                  options = pathOptions(pane = "lsoa")) %>%
      addPolylines(data = localities, stroke = TRUE, weight = 2, color = "#000", opacity = 1,
                   options = pathOptions(pane = "localities")) %>% 
      onRender(
        " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
      )
      
    }
    else {
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        addTiles(
          urlTemplate = "",
          attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2019)</a>',
          options = tileOptions(minZoom = 11, maxZoom = 17)
        ) %>%
        addPolygons(data = domain(), 
                    fillColor = ~pal(decile), 
                    weight = 1,  opacity = 1, color = "#FFF", dashArray = "1", fillOpacity = 1,
                    highlight = highlightOptions(
                      weight = 2, color = "#000", fillOpacity = 1, bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "13px",
                      direction = 'top',
                      offset=c(0,-5))) %>%
       onRender(
          " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
        )
      
    }
      
  })
  
  output$bar <- renderggiraph({
    
    validate(need(nrow(domain()) != 0, message = FALSE))
    
    palette <- c("#453B52", "#454F69", "#3F657E", "#317B8D", "#239296", "#26A898", "#43BD93", "#6AD189", "#98E37D", "#CAF270")
    
    gg <- domain() %>%
      st_set_geometry(value = NULL) %>% 
      count(decile) %>%
      mutate(pct = n/sum(n),
             tooltip = paste0("<strong>", percent(pct), "</strong>", paste0(" (", n, " LSOAs)"))) %>% 
      ggplot(aes(fct_rev(factor(decile)), n)) +
      geom_bar_interactive(aes(tooltip = tooltip, fill = factor(decile)), stat = "identity", position = "dodge") +
      scale_fill_manual(values = palette) +
      scale_y_continuous(expand = c(0,0)) +
      coord_flip() +
      labs(x = NULL, y = NULL,
           title = "% of LSOAs in each national deprivation decile",
           subtitle = "1 = most deprived, 10 = least deprived",
           caption = "Source: MHCLG") +
      theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0, margin = margin(9, 0, 9, 0)),
        plot.caption = element_text(size = 9, colour = "#757575", hjust = 1, margin = margin(t = 15)),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none"
    )
    
    gg <- girafe(ggobj = gg)
    
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
  })
  
}

shinyApp(ui = ui, server = server)
