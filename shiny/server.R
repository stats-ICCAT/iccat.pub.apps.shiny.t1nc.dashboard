server = function(input, output, session) {
  EMPTY_FILTER = 
    list(years = c(),
         species = c(),
         stockAreas = c(),
         flags = c(),
         gearGroups = c(),
         samplingAreas = c(),
         catchTypes = c(),
         CPCs = c(),
         CPCStatus = c(),
         fleets = c(),
         gears = c(),
         areas = c(),
         fishingZoneCodes = c(),
         qualities = c()
    )
  
  observeEvent(input$resetFilters, { session$reload() })

  default_filter_data = function(data, input = EMPTY_FILTER) {
    INFO("### Performing search")
    INFO(paste0("Years          : ", paste0(input$years,         collapse = "-")))
    INFO(paste0("Species        : ", paste0(input$species,       collapse = ", ")))
    INFO(paste0("Flags          : ", paste0(input$flags,         collapse = ", ")))
    INFO(paste0("Gear groups    : ", paste0(input$gearGroups,    collapse = ", ")))
    INFO(paste0("Stock areas    : ", paste0(input$stockAreas,    collapse = ", ")))
    INFO(paste0("Sampling areas : ", paste0(input$samplingAreas, collapse = ", ")))
    INFO(paste0("Catch types    : ", paste0(input$catchTypes,    collapse = ", ")))
    INFO(paste0("CPCs           : ", paste0(input$CPCs,          collapse = ", ")))
    INFO(paste0("CPC status     : ", paste0(input$CPCStatus,     collapse = ", ")))
    INFO(paste0("Fleets         : ", paste0(input$fleets,        collapse = ", ")))
    INFO(paste0("Gears          : ", paste0(input$gears,         collapse = ", ")))
    INFO(paste0("Areas          : ", paste0(input$areas,         collapse = ", ")))
    INFO(paste0("Fishing zones  : ", paste0(input$fishingZones,  collapse = ", ")))
    INFO(paste0("Quality levels : ", paste0(input$qualityLevels, collapse = ", ")))
    
    start = Sys.time()
    
    filtered = data
    
    has_years = length(input$years) == 2
    
    if(has_years) {
      first_year = input$years[1]
      last_year  = input$years[2]
      
      filtered = filtered[YEAR >= first_year & YEAR <= last_year]
    } else {
      first_year = min(data$YEAR)
      last_year  = max(data$YEAR)
    }

    if(!is.null(input$species)) {
      filtered = filtered[SPECIES_CODE %in% input$species]
    }
    
    if(!is.null(input$stockAreas)) {
      filtered = filtered[STOCK_AREA_CODE %in% input$stockAreas]
    }
    
    if(!is.null(input$flags)) {
      filtered = filtered[FLAG_CODE %in% input$flags]
    }
  
    if(!is.null(input$gearGroups)) {
      filtered = filtered[GEAR_GROUP_CODE %in% input$gearGroups]
    }
    
    if(!is.null(input$samplingAreas)) {
      filtered = filtered[SAMPLING_AREA_CODE %in% input$samplingAreas]
    }
    
    if(!is.null(input$catchTypes)) {
      filtered = filtered[CATCH_TYPE_CODE %in% input$catchTypes]
    }
    
    ### Other filters
    
    if(!is.null(input$areas)) {
      filtered = filtered[AREA_CODE %in% input$areas]
    }
    
    if(!is.null(input$CPCs)) {
      filtered = filtered[CPC_CODE %in% input$CPCs]
    }
    
    if(!is.null(input$CPCStatus)) {
      filtered = filtered[CPC_STATUS_CODE %in% input$CPCStatus]
    }
    
    if(!is.null(input$fleets)) {
      filtered = filtered[FLEET_CODE %in% input$fleets]
    }
    
    if(!is.null(input$gears)) {
      filtered = filtered[GEAR_CODE %in% input$gears]
    }
    
    if(!is.null(input$fishingZones)) {
      filtered = filtered[FISHING_ZONE_CODE %in% input$fishingZones]
    }
    
    if(!is.null(input$qualityLevels)) {
      filtered = filtered[QUALITY_CODE %in% input$qualityLevels]
    }
    
    end = Sys.time()
    
    INFO(paste0("Filtering data: ", end - start))
    
    INFO(paste0("Filtered data size: ", nrow(filtered)))
    INFO("")
    
    return(filtered[, .(YearC = YEAR, 
                        FlagName = FLAG_NAME_EN, 
                        Stock = STOCK_AREA_CODE, 
                        SampAreaCode = SAMPLING_AREA_CODE,
                        Species = SPECIES_CODE, 
                        GearGrp = GEAR_GROUP_CODE, 
                        CatchTypeCode = CATCH_TYPE_CODE, 
                        Qty_t = CATCH)])
  }
  
  filter_nc_data = reactive({
    return(
      filter_nc_data_(input)
    )
  })
  
  filter_nc_data_ = function(input = EMPTY_FILTER) {
    filtered = default_filter_data(NC_raw, input)

    return(filtered)
  }

  validate_filtering = function(filtered_data) {
    filtered_rows = nrow(filtered_data)
    
    if(filtered_rows == 0) {
      shinyjs::disable("downloadFiltered")
      
      showModal(modalDialog(title  = "No matching records", 
                            footer = NULL,
                            easyClose = TRUE,
                            fade = FALSE,
                            "Please refine your current filtering criteria!"))
    } else {
      shinyjs::enable("downloadFiltered")
    }
    
    #validate(need(filtered_rows > 0, "Current filtering criteria do not identify any valid record!"))
    
    return(filtered_data)
  }

  observe({ 
    filtered_data = validate_filtering(default_filter_data(NC_raw, input))
  })
  
  output$bySpecies = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0) {
        species_codes = sort(unique(t1nc_data$Species))
        
        SPECIES_COLORS = data.table(CODE = species_codes)
        SPECIES_COLORS$FILL = hue_pal()(nrow(SPECIES_COLORS))
        SPECIES_COLORS[, COLOR := darken(FILL, amount = .3)]
        
        iccat.pub.plots::t1nc.plot.bar(
          t1nc_data,
          category_column = "Species",
          colors = SPECIES_COLORS
        ) + 
        guides(
          fill =
            guide_legend(
              title = "Species"
            )
        ) + 
        labs(title = "Annual catches by species")
      }
    })
    
  output$bySpeciesRel = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0) {
        species_codes = sort(unique(t1nc_data$Species))
        
        SPECIES_COLORS = data.table(CODE = species_codes)
        SPECIES_COLORS$FILL = hue_pal()(nrow(SPECIES_COLORS))
        SPECIES_COLORS[, COLOR := darken(FILL, amount = .3)]
        
        iccat.pub.plots::t1nc.plot.bar(
          t1nc_data,
          category_column = "Species",
          colors = SPECIES_COLORS,
          relative = TRUE
        ) + 
        guides(
          fill =
            guide_legend(
              title = "Species"
            )
        ) + 
        labs(title = "Annual catches by species")
      }
    })
  
  output$byCatchType = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_catch_types(t1nc_data) + 
        labs(title = "Annual catches by type")
    })
  
  output$byCatchTypeRel = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_catch_types(t1nc_data, relative = TRUE) + 
        labs(title = "Annual catches by type")
    })
  
  output$byStock = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_stocks(t1nc_data) + 
        labs(title = "Annual catches by stock")
    })
  
  output$byStockRel = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_stocks(t1nc_data, relative = TRUE) + 
        labs(title = "Annual catches by stock")
    })
  
  output$bySampling = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_sampling_areas(t1nc_data) + 
        labs(title = "Annual catches by sampling area")
    })
  
  output$bySamplingRel = 
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.bar_sampling_areas(t1nc_data, relative = TRUE) + 
        labs(title = "Annual catches by sampling area")
    })
  
  output$byFleetGear =
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.pareto_fleet_gears(t1nc_data, vertical = FALSE, max_x = 30) + 
        labs(title = "Cumulative catches by fleet and gear")
    })
  
  output$bySamplingGear =
    renderPlot({
      t1nc_data = filter_nc_data()
      
      if(nrow(t1nc_data) > 0)
        iccat.pub.plots::t1nc.plot.pareto(t1nc_data, 
                                          x_column = "SampAreaCode", x_name = "Sampling area",
                                          category_column = "GearGrp", category_name = "Gear group", 
                                          category_levels = REF_GEAR_GROUPS$CODE, category_colors = iccat.pub.aes::REF_GEAR_GROUPS_COLORS,
                                          vertical = FALSE, max_x = 30, max_categories = 10, rotate_x_labels = FALSE) + 
        labs(title = "Cumulative catches by sampling area and gear")
    })
  
  output$filtered_data =
    renderDataTable({
      t1nc_data = filter_nc_data() 

      return(
        DT::datatable(
          filtered_data,
          options = list(
            pageLength = INITIAL_NUM_ENTRIES, 
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
          ),
          filter    = "none",
          selection = "none",
          rownames = FALSE
        ) 
        %>% DT::formatCurrency(columns = c("CATCH"), currency = "")
      )
    })
  
  serialize_last_update_date = function() {
    return(
      str_replace_all(META$LAST_UPDATE, "\\-", "")
    )
  }
  
  get_filename_components = function(input) {
    components = c(paste0(input$years,         collapse = "-"), 
                   paste0(input$species,       collapse = "-"), 
                   paste0(input$CPCs,          collapse = "+"), 
                   paste0(input$CPCStatus,     collapse = "+"),
                   paste0(input$flags,         collapse = "+"),
                   paste0(input$fleets,        collapse = "+"),
                   paste0(input$gearGroups,    collapse = "+"),
                   paste0(input$gears,         collapse = "+"),
                   paste0(input$stockAreas,    collapse = "+"),
                   paste0(input$samplingAreas, collapse = "+"),
                   paste0(input$areas,         collapse = "+"),
                   paste0(input$fishingZones,  collapse = "+"),
                   paste0(input$catchTypes,    collapse = "+"),
                   paste0(input$qualityLevels, collapse = "+"))
    
    components = components[which(components != "")]
    
    return(paste0(components, collapse = "_"))
  }
  
  output$downloadFiltered = downloadHandler(
    filename = function() {
      dataset = input$dataset
      
      filename_prefix = paste0("ICCAT_T1NC_", serialize_last_update_date())

      return(paste0(filename_prefix, "_", get_filename_components(input), ".csv.gz"))
    },
    content = function(file) {
      dataset = input$dataset
      
      to_download = filter_nc_data()
      
      write.csv(to_download, gzfile(file), row.names = FALSE, na = "")
    }
  )
}
