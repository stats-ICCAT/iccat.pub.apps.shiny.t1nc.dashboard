ui = function() {
  TITLE = "ICCAT / Task 1 / NC / dashboard"
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
            column(width = 8,
              h2(
                style = "margin-top: 5px !important",
                img(src = "iccat-logo.jpg", height = "48px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(width = 2,
              fluidRow(
                column(width = 12,
                  sliderInput("years", 
                    "Year range",
                    width = "100%",
                    min = MIN_YEAR, max = MAX_YEAR,
                    value = c(1980, MAX_YEAR),
                    sep = "",
                    step  = 1
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                  tabsetPanel(
                    tabPanel("Main filters", icon = icon("filter"),
                      style = "padding-top: 1em",
                      fluidRow(
                        column(width = 12,
                          UI_select_input("species", "Species", ALL_SPECIES, "BFT")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("stockAreas", "Stock area(s)", ALL_STOCK_AREAS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("flags", "Flag(s)", ALL_FLAGS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("gearGroups", "Gear group(s)", ALL_GEAR_GROUPS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("samplingAreas", "Sampling area(s)", ALL_SAMPLING_AREAS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("catchTypes", "Catch type(s)", ALL_CATCH_TYPES)
                        )
                      )
                    ),
                    tabPanel("Other filters", icon = icon("filter"),
                      style = "padding-top: 1em", 
                      fluidRow(
                        column(width = 12, 
                          UI_select_input("CPCs", "CPC(s)", ALL_CPCS)
                        )
                      ), 
                      fluidRow(
                        column(width = 12,
                          UI_select_input("CPCStatus", "CPC status(es)", ALL_CPC_STATUS)
                        )
                      ), 
                      fluidRow(
                        column(width = 12,
                          UI_select_input("fleets", "Fleet(s)", ALL_FLEETS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("gears", "Gear(s)", ALL_GEARS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("areas", "Area(s)", ALL_AREAS)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("fishingZones", "Fishing zone(s)", ALL_FISHING_ZONES)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          UI_select_input("qualityLevels", "Quality level(s)", ALL_QUALITIES)
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                  actionButton("resetFilters", "Reset all filters", icon = icon("filter-circle-xmark"))
                )
              ),
              fluidRow(
                column(width = 12,
                  hr(),
                  span("Data last updated on:"), 
                  strong(META$LAST_UPDATE)
                )
              )
            ),
            column(width = 10,
              div(
                tabsetPanel(
                  selected = "Bar charts",
                  tabPanel("Bar charts",
                    icon = icon("chart-bar"),
                    div(style = "margin-top: .5em;",
                      tabsetPanel(
                        tabPanel("By gear group",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarGearGroup", "Download as image"),
                          plotOutput("byGearGroup", height = "320px"),
                          downloadButton("downloadBarGearGroupRel", "Download as image"),
                          plotOutput("byGearGroupRel", height = "320px")
                        ),
                        tabPanel("By catch type",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarCatchType", "Download as image"),
                          plotOutput("byCatchType", height = "320px"),
                          downloadButton("downloadBarCatchTypeRel", "Download as image"),
                          plotOutput("byCatchTypeRel", height = "320px")
                        ),
                        tabPanel("By species",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarSpecies", "Download as image"),  
                          plotOutput("bySpecies", height = "320px"),
                          downloadButton("downloadBarSpeciesRel", "Download as image"),
                          plotOutput("bySpeciesRel", height = "320px")
                        ),
                        tabPanel("By fishing zone",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarFishingZone", "Download as image"),
                          plotOutput("byFishingZone", height = "320px"),
                          downloadButton("downloadBarFishingZoneRel", "Download as image"),
                          plotOutput("byFishingZoneRel", height = "320px")
                        ),
                        tabPanel("By stock",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarStock", "Download as image"),
                          plotOutput("byStock", height = "320px"),
                          downloadButton("downloadBarStockRel", "Download as image"),
                          plotOutput("byStockRel", height = "320px")
                        ),
                        tabPanel("By sampling area",
                          style = "margin-top: .5em;",
                          downloadButton("downloadBarSampling", "Download as image"),
                          plotOutput("bySampling", height = "320px"),
                          downloadButton("downloadBarSamplingRel", "Download as image"),
                          plotOutput("bySamplingRel", height = "320px")
                        )
                      )
                    )
                  ),
                  tabPanel("Pareto charts",
                    icon = icon("chart-line"),
                    div(style = "margin-top: 1em;",
                      fluidRow(
                        column(width = 12,
                          downloadButton("downloadParetoByFleetGear", "Download as image"),
                          plotOutput("byFleetGear", height = 360)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          downloadButton("downloadParetoBySamplingGear", "Download as image"),
                          plotOutput("bySamplingGear", height = 360)
                        )
                      )
                    )
                  ),
                  tabPanel("Geospatial charts", icon = icon("globe-europe"),
                    div(style = "margin-top: 1em;",
                      fluidRow(
                        column(width = 6,
                          downloadButton("downloadMapBySamplingArea", "Download as image"),
                          plotOutput("mapBySamplingArea", height = 640)
                        ),
                        column(width = 6,
                          div(id = "mapBySamplingAreaTable",
                            dataTableOutput("mapBySamplingAreaTable")
                          )
                        )
                      )
                    )
                  ),
                  tabPanel("Tabular data", icon = icon("list-alt"),
                    div(id = "tabularData",
                      dataTableOutput("tabularData")
                    )
                  ),
                  tabPanel("Reference data", icon = icon("link"),
                    div(style = "margin-top: 1em;",
                      tabsetPanel(
                        type = "pills",
                        tabPanel("Species",
                          div(class = "tableContainer",
                             dataTableOutput("ref_species")
                          )
                        ),
                        tabPanel("Flags",
                          div(class = "tableContainer",
                            dataTableOutput("ref_flags")
                          )
                        ),
                        tabPanel("Fleets",
                          div(class = "tableContainer",
                            dataTableOutput("ref_fleets")
                          )
                        ),
                        tabPanel("CPCs",
                          div(class = "tableContainer",
                            dataTableOutput("ref_CPCs")
                          )
                        ),
                        tabPanel("CPC status",
                          div(class = "tableContainer",
                            dataTableOutput("ref_CPC_codes")
                          )
                        ),
                        tabPanel("Gear groups",
                          div(class = "tableContainer",
                            dataTableOutput("ref_gear_groups")
                          )
                        ),
                        tabPanel("Gears",
                          div(class = "tableContainer",
                            dataTableOutput("ref_gears")
                          )
                        ),
                        tabPanel("Stocks",
                          div(class = "tableContainer",
                            dataTableOutput("ref_stocks")
                          )
                        ),
                        tabPanel("Sampling areas",
                          div(class = "tableContainer",
                            dataTableOutput("ref_sampling_areas")
                          )
                        ),
                        tabPanel("Areas",
                          div(class = "tableContainer",
                            dataTableOutput("ref_areas")
                          )
                        ),
                        tabPanel("Fishing zones",
                          div(class = "tableContainer",
                            dataTableOutput("ref_fishing_zones")
                          )
                        ),
                        tabPanel("Catch types",
                          div(class = "tableContainer",
                            dataTableOutput("ref_catch_types")
                          )
                        ),
                        tabPanel("Quality levels",
                          div(class = "tableContainer",
                            dataTableOutput("ref_quality_levels")
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
  )
}
