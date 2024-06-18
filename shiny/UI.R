ui = function() {
  TITLE = paste0("ICCAT data dashboard / T1NC / ", META$LAST_UPDATE)
  return(
    fluidPage(
      shinyjs::useShinyjs(),
      title = TITLE,
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tags$div(
        class = "main-container",
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div(id = "glasspane",
                   tags$div(class = "loading", "Filtering data and preparing output...")
          )
        ),
        tags$div(
          fluidRow(
            column(
              width = 8,
              h2(
                style = "margin-top: 5px !important",
                img(src = "iccat-logo.jpg", height = "48px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              fluidRow(
                column(
                  width = 12,
                  sliderInput("years", "Year range",
                              width = "100%",
                              min = MIN_YEAR, max = MAX_YEAR,
                              value = c(1980, MAX_YEAR),
                              sep = "",
                              step  = 1)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  tabsetPanel(
                    tabPanel("Main filters",
                             icon = icon("filter"),
                             style = "padding-top: 1em",
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("species", "Species", ALL_SPECIES, "BFT")
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("stockAreas", "Stock area(s)", ALL_STOCK_AREAS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("flags", "Flag(s)", ALL_FLAGS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("gearGroups", "Gear group(s)", ALL_GEAR_GROUPS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("samplingAreas", "Sampling area(s)", ALL_SAMPLING_AREAS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("catchTypes", "Catch type(s)", ALL_CATCH_TYPES)
                               )
                             )
                    ),
                    tabPanel("Other filters",
                             icon = icon("filter"),
                             style = "padding-top: 1em", 
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("CPCs", "CPC(s)", ALL_CPCS)
                               )
                             ), 
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("CPCStatus", "CPC status(es)", ALL_CPC_STATUS)
                               )
                             ), 
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("fleets", "Fleet(s)", ALL_FLEETS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("gears", "Gear(s)", ALL_GEARS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("fishingZones", "Fishing zone(s)", ALL_FISHING_ZONES)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("qualityLevels", "Quality level(s)", ALL_QUALITIES)
                               )
                             )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  actionButton("resetFilters", "Reset all filters", icon = icon("filter-circle-xmark"))
                )
              ),
              #fluidRow(
              #  column(
              #    width = 12,
              #    h5(strong("Download current dataset:"))
              #  )
              #),
              #fluidRow(
              #  column(
              #    width = 4,
              #    downloadButton("downloadFiltered", "Filtered", style = "width: 100px")
              #  ),
              #  column(
              #    width = 4,
              #    span("as ", style = "vertical-align: -5px",
              #         code(".csv.gz")
              #    )
              #  )
              #),
              fluidRow(
                column(
                  width = 12,
                  hr(),
                  span("Data last updated on:"), 
                  strong(META$LAST_UPDATE)
                )
              )
            ),
            column(
              width = 10,
              div(style = "margin-top: 1em;",
                  tabsetPanel(
                    tabPanel("Bar charts",
                             div(style = "margin-top: 1em;",
                                 fluidRow(
                                   column(width = 6,
                                          #strong("Annual catches by species"),
                                          tabsetPanel(
                                            type = "pills",
                                            tabPanel("Absolute",
                                                     plotOutput("bySpecies")
                                                     ),
                                            tabPanel("Relative",
                                                     plotOutput("bySpeciesRel")
                                                     )
                                            )
                                          ),
                                   column(width = 6,
                                          #strong("Annual catches by catch type"),
                                          tabsetPanel(
                                            type = "pills",
                                            tabPanel("Absolute",
                                                     plotOutput("byCatchType")
                                                     ),
                                            tabPanel("Relative",
                                                     plotOutput("byCatchTypeRel")
                                                     )
                                            )
                                          )
                                   ),
                                 fluidRow(
                                   column(width = 6,
                                          #strong("Annual catches by stock"),
                                          tabsetPanel(
                                            type = "pills",
                                            tabPanel("Absolute",
                                                     plotOutput("byStock")
                                                     ),
                                            tabPanel("Relative",
                                                     plotOutput("byStockRel")
                                                     )
                                            )
                                          ),
                                   column(width = 6,
                                          #strong("Annual catches by sampling area"),
                                          tabsetPanel(
                                            type = "pills", 
                                            tabPanel("Absolute",
                                                     plotOutput("bySampling")
                                                     ),
                                            tabPanel("Relative",
                                                     plotOutput("bySamplingRel")
                                                     )
                                            )
                                          )
                                   )
                                 )
                             ),
                    tabPanel("Pareto charts",
                             div(style = "margin-top: 1em;",
                                 fluidRow(
                                   column(width = 12,
                                          #strong("Cumulative catches by flag and gear group"),
                                          plotOutput("byFleetGear")
                                          )
                                 ),
                                 fluidRow(
                                   column(width = 12,
                                          #strong("Cumulative catches by sampling area and gear group"),
                                          plotOutput("bySamplingGear")
                                   )
                                 )
                             )
                    )
                  )
              )
            )
          )
        )
      )
    )
  )
}
