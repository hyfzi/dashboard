library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(forcats)

adsl <- readRDS("data/adsl.rds")

# gallery_1_tab ----
gallery_1_tab <- tabItem(
  tabName = "sub_dist",
  fluidRow(
    column(
      width = 8,
      box(
        title = "Subject Distribution",
        width = 12,
        plotOutput("plot1", height = 250)
      ),
    ),
    column(
      width = 4,
      # endpoint filter（第二层）
      box(
        title = "Controls",
        width = 12,
        sliderInput("age", "Age range", 18, 80, c(30, 70))
      ),
      box(
        title = "Group Variable",
        width = 12,
        selectInput("group_var", "Select Group Variable",
                      choices = c("TRT01P", "SITEID", "SEX", "RACE", "RACEGR1", "EOSSTT"), 
                      selected = "TRT01P",
                      multiple = FALSE)
      )
    )
  )
)

shinyApp(
  ui = dashboardPage(
    title = "Basic",
    header = dashboardHeader(
      title = dashboardBrand(
        title = "临床试验",
        color = "primary"
      )
    ),
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "primary",
      id = "sidebar",
      sidebarHeader("gallery"),
      sidebarMenu(
        id = "current_tab",
        menuItem(
          text = "Galleries",
          icon = icon("cubes"),
          startExpanded = FALSE,
          menuSubItem(
            text = "受试者分布",
            tabName = "sub_dist",
            icon = icon("circle")
          ),
          menuSubItem(
            text = "Gallery 2",
            tabName = "gallery2"
          )
        )
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        controlbarItem(
          "全局控制",
          column(
            width = 12,
            align = "center",
            # population filter（第一层）
            selectInput("siteid", label = "Select Siteid", 
              choices = sort(unique(adsl$SITEID)), 
              selected = sort(unique(adsl$SITEID)),
              multiple = TRUE),
            selectInput("saffl", label = "Select SAFFL", 
              choices = sort(unique(adsl$SAFFL)), 
              selected = sort(unique(adsl$SAFFL)),
              multiple = TRUE)
          )
        )
      )
    ),
    footer = dashboardFooter(),
    body = dashboardBody(
      gallery_1_tab
    ),
    help = FALSE
  ),
  server = function(input, output) {

    population_filter <- reactive({
      adsl |> 
        filter(SITEID %in% input$siteid & SAFFL %in% input$saffl)
    })

    endpoint_filter <- reactive({
      population_filter() |> 
        filter(AGE >= input$age[1],
              AGE <= input$age[2])
    })

    stat_model <- reactive({
      endpoint_filter() |> 
        group_by(.data[[input$group_var]]) |> 
        summarise(
          n = n_distinct(USUBJID)
        ) |> 
          arrange(desc(n))
    })

    output$plot1 <- renderPlot({
      df <- stat_model()
      
      ggplot(
        df, 
        aes(
          x = fct_reorder(.data[[input$group_var]], n),
          y = n
        )
      ) +
        geom_col(fill = "#4C97FF") +
        coord_flip() +
        labs(
            x = NULL,
            y = "Count"
          ) + 
        theme_minimal(base_size = 13)
    })
  }
)
