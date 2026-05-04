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
        plotOutput("plot1")
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
        selectInput("group_var1", "Select Group Variable1",
                      choices = c("TRT01P", "SITEID", "SEX", "RACEGR1", "EOSSTT", "AGE"), 
                      selected = "SITEID",
                      multiple = FALSE),
        selectInput("group_var2", "Select Group Variable2",
                      choices = c("TRT01P", "SITEID", "SEX", "RACEGR1", "EOSSTT", "None"), 
                      selected = "TRT01P",
                      multiple = FALSE)
      )
    )
  )
)

# gallery_2_tab ----
gallery_2_tab <- tabItem(
  tabName = "xmind",
  fluidRow(
    column(
      width = 12,
      box(
        title = "Xmind",
        width = 12,
        "This is the Xmind tab content."
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
          text = "人口学与疾病特征",
          icon = icon("cubes"),
          startExpanded = FALSE,
          menuSubItem(
            text = "项目图谱",
            tabName = "xmind",
            icon = icon("th")
          ),
          menuSubItem(
            text = "参与者分布",
            tabName = "sub_dist",
            icon = icon("circle")
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
      tabItems(
        gallery_1_tab,
        gallery_2_tab
      )
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

    output$plot1 <- renderPlot({
      df <- endpoint_filter()

      if (is.character(df[[input$group_var1]]) & is.character(df[[input$group_var2]]) & input$group_var1 != input$group_var2) {
        stat_model <- reactive({
          endpoint_filter() |> 
            group_by(.data[[input$group_var1]], .data[[input$group_var2]]) |> 
            summarise(
              n = n_distinct(USUBJID),
              .groups = "drop"
            ) |> 
            ungroup() |> 
            mutate(
              pct = n / sum(n),
              !!input$group_var1 := reorder(.data[[input$group_var1]], pct, FUN = sum)
              )
        })
        ggplot(
          stat_model(), 
          aes(
            x = .data[[input$group_var1]],
            y = pct,
            fill = .data[[input$group_var2]]
          )
        ) +
          geom_col() +
          scale_y_continuous(labels = scales::percent) +
          coord_flip() +
          labs(
              x = "",
              y = "Percentage (Overall)",
              fill = input$group_var2
            ) + 
          theme_minimal(base_size = 13)

      }else if ((is.character(df[[input$group_var1]]) & input$group_var1 == input$group_var2) | 
        (is.character(df[[input$group_var1]]) & input$group_var2 == "None")) {
        stat_model <- reactive({
          endpoint_filter() |> 
            group_by(.data[[input$group_var1]]) |> 
            summarise(
              n = n_distinct(USUBJID),
              .groups = "drop"
            ) |> 
            ungroup() |> 
            mutate(
              pct = n / sum(n),
              !!input$group_var1 := reorder(.data[[input$group_var1]], pct, FUN = sum)
              )
        })
        ggplot(
          stat_model(), 
          aes(
            x = .data[[input$group_var1]],
            y = pct
          )
        ) +
          geom_col(fill = "#4C97FF",) +
          scale_y_continuous(labels = scales::percent) +
          coord_flip() +
          labs(
              x = "",
              y = "Percentage (Overall)"
            ) + 
          theme_minimal(base_size = 13)
        
      }else if (is.numeric(df[[input$group_var1]])) {
        ggplot(
          endpoint_filter(), 
          aes(x = .data[[input$group_var1]])
        ) +
          geom_histogram(fill = "#4C97FF", binwidth = 3) +
          labs(
              x = input$group_var1,
              y = "Count"
            )
      }
    })
  }
)
