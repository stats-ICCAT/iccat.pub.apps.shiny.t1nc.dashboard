library(stringr)

library(iccat.pub.base)
library(iccat.pub.data)
library(iccat.pub.plots)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

library(RColorBrewer)
library(scales)

#library(plotly)

options(scipen = 9999)

# THIS IS ***FUNDAMENTAL*** TO HAVE THE DOCKER CONTAINER CORRECTLY LOAD THE .RData FILE WITH THE ORIGINAL UTF-8 ENCODING
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

load("./META.RData")
load("./NC_raw.RData")

# Removes deprecated areas 
REF_AREAS = REF_AREAS[DEPRECATED == 0]

ALL_CPCS           = setNames(as.character(REF_PARTIES$CODE),        paste0(REF_PARTIES$CODE,        " - ", REF_PARTIES$NAME_EN))
ALL_CPC_STATUS     = setNames(as.character(REF_PARTY_STATUS$CODE),   paste0(REF_PARTY_STATUS$CODE,   " - ", REF_PARTY_STATUS$NAME_EN))
ALL_FLAGS          = setNames(as.character(REF_FLAGS$CODE),          paste0(REF_FLAGS$CODE,          " - ", REF_FLAGS$NAME_EN))
ALL_FLEETS         = setNames(as.character(REF_FLEETS$CODE),         paste0(REF_FLEETS$CODE,         " - ", REF_FLEETS$NAME_EN))

ALL_GEAR_GROUPS    = setNames(as.character(REF_GEAR_GROUPS$CODE),    paste0(REF_GEAR_GROUPS$CODE,    " - ", REF_GEAR_GROUPS$NAME_EN))
ALL_GEARS          = setNames(as.character(REF_GEARS$CODE),          paste0(REF_GEARS$CODE,          " - ", REF_GEARS$NAME_EN))

ALL_STOCK_AREAS    = setNames(as.character(REF_STOCK_AREAS$CODE),    paste0(REF_STOCK_AREAS$CODE,    " - ", REF_STOCK_AREAS$NAME_EN))

SAMPLING_AREAS_ALB = REF_SAMPLING_AREAS[str_sub(CODE, 1, 2) == "AL"]
SAMPLING_AREAS_BFT = REF_SAMPLING_AREAS[str_sub(CODE, 1, 2) == "BF"]
SAMPLING_AREAS_BET = REF_SAMPLING_AREAS[str_sub(CODE, 1, 2) == "BE"]
SAMPLING_AREAS_SKJ = REF_SAMPLING_AREAS[str_sub(CODE, 1, 2) == "SJ"]
SAMPLING_AREAS_YFT = REF_SAMPLING_AREAS[str_sub(CODE, 1, 2) == "YF"]
SAMPLING_AREAS_BIL = REF_SAMPLING_AREAS[str_sub(CODE, 1, 3) == "BIL"]

ALL_SAMPLING_AREAS = list(
  "Albacore tuna " = setNames(SAMPLING_AREAS_ALB$CODE, paste0(SAMPLING_AREAS_ALB$CODE, " - ", SAMPLING_AREAS_ALB$NAME_EN)),
  "Bluefin tuna"   = setNames(SAMPLING_AREAS_BFT$CODE, paste0(SAMPLING_AREAS_BFT$CODE, " - ", SAMPLING_AREAS_BFT$NAME_EN)),
  "Bigeye tuna"    = setNames(SAMPLING_AREAS_BET$CODE, paste0(SAMPLING_AREAS_BET$CODE, " - ", SAMPLING_AREAS_BET$NAME_EN)),
  "Skipjack tuna"  = setNames(SAMPLING_AREAS_SKJ$CODE, paste0(SAMPLING_AREAS_SKJ$CODE, " - ", SAMPLING_AREAS_SKJ$NAME_EN)),
  "Yellowfin tuna" = setNames(SAMPLING_AREAS_YFT$CODE, paste0(SAMPLING_AREAS_YFT$CODE, " - ", SAMPLING_AREAS_YFT$NAME_EN)),
  "Billfish"       = setNames(SAMPLING_AREAS_BIL$CODE, paste0(SAMPLING_AREAS_BIL$CODE, " - ", SAMPLING_AREAS_BIL$NAME_EN))
)

ALL_AREAS          = setNames(as.character(REF_AREAS$CODE),          paste0(REF_AREAS$CODE,          " - ", REF_AREAS$NAME_EN))
ALL_FISHING_ZONES  = setNames(as.character(REF_FISHING_ZONES$CODE),  paste0(REF_FISHING_ZONES$CODE,  " - ", REF_FISHING_ZONES$NAME_EN))

CT_CATCHES  = REF_CATCH_TYPES[CATCH_TYPE_GROUP == "Catches"]
CT_LANDINGS = REF_CATCH_TYPES[CATCH_TYPE_GROUP == "Landings"]
CT_DISCARDS = REF_CATCH_TYPES[CATCH_TYPE_GROUP == "Discards"]

ALL_CATCH_TYPES    = list(
  "Catches"  = setNames(as.character(CT_CATCHES$CODE),  paste0(CT_CATCHES$CODE,  " - ", CT_CATCHES$NAME_EN)),
  "Landings" = setNames(as.character(CT_LANDINGS$CODE), paste0(CT_LANDINGS$CODE, " - ", CT_LANDINGS$NAME_EN)),
  "Discards" = setNames(as.character(CT_DISCARDS$CODE), paste0(CT_DISCARDS$CODE, " - ", CT_DISCARDS$NAME_EN))
)

ALL_QUALITIES      = setNames(as.character(REF_QUALITY_LEVELS$CODE), paste0(REF_QUALITY_LEVELS$CODE, " - ", REF_QUALITY_LEVELS$NAME_EN))

SP_TEMPERATE    = REF_SPECIES[SPECIES_GROUP == "Temperate tunas"]
SP_TROPICAL     = REF_SPECIES[SPECIES_GROUP == "Tropical tunas"]
SP_SMALL_TUNAS  = REF_SPECIES[SPECIES_GROUP_ICCAT == "Tunas (small)"]
SP_BILLFISH     = REF_SPECIES[SPECIES_GROUP == "Billfishes"]
SP_MAJOR_SHARKS = REF_SPECIES[SPECIES_GROUP_ICCAT == "Sharks (major)"]
SP_OTHER_SHARKS = REF_SPECIES[SPECIES_GROUP_ICCAT == "Sharks (other)"]

ALL_SPECIES = list(
  "Temperate tunas" = setNames(as.character(SP_TEMPERATE$CODE),    paste0(SP_TEMPERATE$CODE,    " - ", SP_TEMPERATE$NAME_EN)),
  "Tropical tunas"  = setNames(as.character(SP_TROPICAL$CODE),     paste0(SP_TROPICAL$CODE,     " - ", SP_TROPICAL$NAME_EN)),
  "Small tunas"     = setNames(as.character(SP_SMALL_TUNAS$CODE),  paste0(SP_SMALL_TUNAS$CODE,  " - ", SP_SMALL_TUNAS$NAME_EN)),
  "Billfish"        = setNames(as.character(SP_BILLFISH$CODE),     paste0(SP_BILLFISH$CODE,     " - ", SP_BILLFISH$NAME_EN)),
  "Sharks (major)"  = setNames(as.character(SP_MAJOR_SHARKS$CODE), paste0(SP_MAJOR_SHARKS$CODE, " - ", SP_MAJOR_SHARKS$NAME_EN)),
  "Sharks (other)"  = setNames(as.character(SP_OTHER_SHARKS$CODE), paste0(SP_OTHER_SHARKS$CODE, " - ", SP_OTHER_SHARKS$NAME_EN))
)

UI_select_input = function(id, label, choices, selected = NA) {
  return(
    virtualSelectInput(
      inputId = id, 
      label = label,
      width = "100%",
      multiple = TRUE,
      autoSelectFirstOption = FALSE,
      choices = choices,
      search = TRUE,
      showValueAsTags = FALSE,
      updateOn = "close", 
      selected = selected
    )
  )
}

INITIAL_NUM_ENTRIES = 45

set_log_level(LOG_INFO)

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(NC_raw$YEAR)

#NC_raw$YEAR = 
#  factor(
#    NC_raw$YEAR,
#    labels = MIN_YEAR:MAX_YEAR,
#    levelx = MIN_YEAR:MAX_YEAR,
#    ordered = TRUE
#  )

INFO(paste0(nrow(NC_raw), " rows loaded from NC"))