library(shiny) ; library(shinydashboard) ; library(shinyWidgets) ; library(tidyverse) ; library(sf) ; library(leaflet) ; library(htmltools) ; library(htmlwidgets) ; library(ggiraph) ; library(scales)

df <- read.csv("data/IMD15.csv") %>% 
  mutate(decile = factor(decile))

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
      "Indices of Multiple Deprivation, 2019 (2015 data shown)"
    ),
    windowTitle = "imd19"
  ),
  fluidRow( 
    p(id = "intro", "This application visualises the English Indices of Deprivation 2019 for Lower-layer Super Output Areas (LSOA) within Trafford. Each LSOA contains approximately 1,500 residents or 650 households. 
      Each LSOA is ranked from 1 (most deprived) to 32,844 (least deprived) and assigned to a decile ranging from 1 (most deprived 10%) to 10 (least deprived 10%).")
    ),
  fluidRow(
    br(),
    div(class = "col-md-4",
        box(width = '100%', 
            radioButtons(inputId = "domain", label = NULL,
                         choices = c("Index of Multiple Deprivation", "Income", "Employment", "Education, Skills and Training", 
                                     "Health Deprivation and Disability", "Crime", "Barriers to Housing and Services", "Living Environment"),
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
              style = "position: absolute; left: 1.5em; bottom: 2.5em;",
              dropdown(
                checkboxInput("checkbox", label = "Show localities", value = FALSE),
                icon = icon("filter"),
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
    lsoa <- left_join(lsoa, filter(df, index_domain == input$domain), by = "lsoa11cd")
  })
  
  output$map <- renderLeaflet({
    
    labels <- 
      paste0(
        "<strong>", domain()$lsoa11nm, "</strong>", paste0(" (", domain()$wd18nm, ")"), "<br />",
        "IMD Score: ", domain()$score, "<br/>",
        "IMD Rank: ", comma(domain()$rank), "<br/>",
        "IMD Decile: ", domain()$decile
      ) %>% 
      lapply(htmltools::HTML)    
    
    pal <- colorFactor(c("#BD6C7D", "#DB8880", "#EDAA93", "#F7CFA6", "#FBECBF", "#EAF4BF", "#D0E6AD", "#ADD5A6", "#86BF99", "#73A289"), domain = 1:10, ordered = TRUE)
    
    
    if (input$checkbox) {
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      setMaxBounds(-2.478454, 53.357425,-2.253022, 53.480362) %>%
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
                    direction = "auto")) %>%
      addPolylines(data = localities, stroke = TRUE, weight = 3, color = "#212121", opacity = 1) %>% 
      onRender(
        " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
      )
      
    }
    else {
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        setMaxBounds(-2.478454, 53.357425,-2.253022, 53.480362) %>%
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
                      direction = "auto")) %>%
       onRender(
          " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
        )
      
    }
      
  })
  
  output$bar <- renderggiraph({
    
    palette <- c("#BD6C7D", "#DB8880", "#EDAA93", "#F7CFA6", "#FBECBF", "#EAF4BF", "#D0E6AD", "#ADD5A6", "#86BF99", "#73A289")
    
    gg <-domain() %>%
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
           title = "% of LSOAs in Trafford by IMD decile",
           subtitle = "1 = most deprived, 10 = least deprived",
           caption = "Source: MHCLG | @traffordDataLab") +
      theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, vjust = 2, hjust = 0),
        plot.subtitle = element_text(size = 11, hjust = 0),
        plot.caption = element_text(size = 9, colour = "#757575", hjust = 1, margin = margin(t = 15)),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none"
    )
    
    gg <- girafe(ggobj = gg)
    
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
  })
  
}

shinyApp(ui = ui, server = server)
