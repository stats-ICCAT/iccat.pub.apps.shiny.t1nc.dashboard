library(iccat.dev.data)

SPECIES_MAPPINGS =
  tabular_query(
    connection = DB_GIS(server = "ATENEA\\SQL22"),
    statement = "
    SELECT *
    FROM NewGIS.dbo.SPECIES_MAPPING_T2CE"
  )

SPECIES_ORDERED = c("BFT", "ALB", # Temperate tunas
                    "YFT", "BET", "SKJ", # Tropical tunas
                    "SWO", "BUM", "SAI", "SPF", "WHM", # Billfish
                    "BLF", "BLT", "BON", "BOP", "BRS", "CER", "FRI", "KGM", "LTA", "MAW", "SLT", "SSM", "WAH",  "DOL", # Small tunas
                    "BIL", "BLM", "MSP", "MLS", "RSP", # Other billfish
                    "SBF", # Southern bluefin tuna
                    "oTun", # Other tunas
                    "BSH", "POR", "SMA", # Main shark species
                    "oSks", # Other sharks
                    "oFis", # Other fish
                    "rest" # Everything else
                  )

# Loads the data from dbSTAT
NC = 
  tabular_query(
    connection = DB_T1(server = "ICARO\\SQL16"),
    statement = "SELECT * FROM V_T1NC_WEB WHERE YEAR >= 1950"
  )

NC_raw = copy(NC)

NC = merge(NC, SPECIES_MAPPINGS[, .(SRC_CODE, SPECIES_CODE = TRG_CODE)],
           by.x = "SPECIES_CODE", by.y = "SRC_CODE",
           all.x = TRUE)

NC[!is.na(SPECIES_CODE) & is.na(SPECIES_CODE.y), SPECIES_CODE.y := "rest"]

NC$SPECIES_CODE = NULL
NC$SPECIES_CODE = NC$SPECIES_CODE.y
NC$SPECIES_CODE.y = NULL

# Attaches flag metadata to the wide catch records
NC = 
  merge(
    NC, REF_FLAGS[, .(CODE, FLAG_NAME_EN = NAME_EN)],
    by.x = "FLAG_CODE", by.y = "CODE",
    all.x = TRUE
  )

# Attaches flag metadata to the wide catch records
NC_raw = 
  merge(
    NC_raw, REF_FLAGS[, .(CODE, FLAG_NAME_EN = NAME_EN)],
    by.x = "FLAG_CODE", by.y = "CODE",
    all.x = TRUE
  )


# Factorises the species code according to the order set by SPECIES_ORDERED
NC$SPECIES_CODE =
  factor(
    NC$SPECIES_CODE,
    levels = SPECIES_ORDERED,
    labels = SPECIES_ORDERED,
    ordered = TRUE
  )

NC_TOTALS = NC[, .(TOTAL = sum(CATCH, na.rm = TRUE)), keyby = .(FLAG_CODE, FLAG_NAME_EN,
                                                                FLEET_CODE,
                                                                CPC_CODE, CPC_STATUS_CODE,
                                                                GEAR_GROUP_CODE, GEAR_CODE,
                                                                YEAR,
                                                                STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
                                                                CATCH_TYPE_CODE, QUALITY_CODE)]

NC_STRATA = 
  unique(
    NC[, .(FLAG_CODE, FLAG_NAME_EN,
           FLEET_CODE,
           CPC_CODE, CPC_STATUS_CODE,
           GEAR_GROUP_CODE, GEAR_CODE,
           YEAR,
           STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
           CATCH_TYPE_CODE, QUALITY_CODE)]
  )

STRATA_COLUMNS = c("FLAG_CODE", "FLAG_NAME_EN",
                   "FLEET_CODE",
                   "CPC_CODE", "CPC_STATUS_CODE",
                   "GEAR_GROUP_CODE", "GEAR_CODE",
                   "YEAR",
                   "STOCK_AREA_CODE", "SAMPLING_AREA_CODE", "AREA_CODE", "FISHING_ZONE_CODE",
                   "CATCH_TYPE_CODE", "QUALITY_CODE")

# Long-to-wide conversion of catch records
NC_w = 
  dcast.data.table(
    NC,
    formula = paste0(paste0(STRATA_COLUMNS, collapse = " + "), " ~ SPECIES_CODE"),
    fun.aggregate = sum,
    drop = c(TRUE, FALSE),
    value.var = "CATCH",
    fill = NA
  )

# Attaches total catches to the wide catch records
NC_w =
  merge(
    NC_w, NC_TOTALS,
    by = STRATA_COLUMNS,
    all.x = TRUE
  )

# Attaches effort and strata columns to the wide catch records
NC_w =
  merge(NC_STRATA, NC_w,
        by = STRATA_COLUMNS,
        all.x = TRUE)

NC_w = NC_w[, .(FLAG_CODE, FLAG_NAME_EN, 
                FLEET_CODE,
                CPC_CODE, CPC_STATUS_CODE,
                GEAR_GROUP_CODE, GEAR_CODE, 
                YEAR, 
                STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
                CATCH_TYPE_CODE, QUALITY_CODE, 
                CATCH_UNIT_CODE = "T",
                TOTAL,
                BFT, ALB,
                YFT, BET, SKJ,
                SWO, BUM, SAI, SPF, WHM,
                BLF, BLT, BON, BOP, BRS, CER, FRI, KGM, LTA, 
                MAW, SLT, SSM, WAH, DOL,
                BIL, BLM, MSP, MLS, RSP, 
                SBF, 
                oTun,
                BSH, POR, SMA, 
                oSks,
                oFis,
                rest)][order(FLAG_CODE, FLAG_NAME_EN, 
                             FLEET_CODE, CPC_CODE, CPC_STATUS_CODE,
                             GEAR_GROUP_CODE, GEAR_CODE, 
                             YEAR, 
                             STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
                             CATCH_TYPE_CODE, QUALITY_CODE)]

#NC[YEAR >= 1950, YEAR_SHORT := str_sub(as.character(YEAR), 3, 4)]
NC[, YEAR_SHORT := str_sub(as.character(YEAR), 3, 4)]

#YEAR_SHORT_FACTORS = unique(NC[!is.na(YEAR_SHORT)][order(YEAR)]$YEAR_SHORT)
YEAR_SHORT_FACTORS = unique(NC[order(YEAR)]$YEAR_SHORT)

#NC[YEAR >= 1950]$YEAR_SHORT =
NC$YEAR_SHORT =
  factor(
    #NC[YEAR >= 1950]$YEAR_SHORT,
    NC$YEAR_SHORT,
    levels = YEAR_SHORT_FACTORS,
    labels = YEAR_SHORT_FACTORS,
    ordered = TRUE
  )

NC_l = NC[, .(DATASET_ID, STRATA_ID,
              FLAG_CODE, FLAG_NAME_EN,
              FLEET_CODE, CPC_CODE, CPC_STATUS_CODE,
              GEAR_GROUP_CODE, GEAR_CODE,
              YEAR,
              STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
              CATCH_TYPE_CODE, QUALITY_CODE, 
              CATCH_UNIT_CODE, SPECIES_CODE, CATCH)][order(FLAG_CODE, FLAG_NAME_EN, 
                                                           FLEET_CODE, CPC_CODE, CPC_STATUS_CODE,
                                                           GEAR_GROUP_CODE, GEAR_CODE, 
                                                           YEAR, 
                                                           STOCK_AREA_CODE, SAMPLING_AREA_CODE, AREA_CODE, FISHING_ZONE_CODE,
                                                           CATCH_TYPE_CODE, QUALITY_CODE)]

META = list(LAST_UPDATE = "2024-10-15", 
            FILENAME_LONG = "ICCAT_T1NC_20241015_raw_full.csv.gz",
            FILENAME_WIDE = "ICCAT_T1NC_20241015_full.csv.gz")

save("META", file = "./shiny/META.RData", compress = "gzip")
save("NC",   file = "./shiny/NC.RData",   compress = "gzip")
save("NC_l", file = "./shiny/NC_l.RData",   compress = "gzip")
save("NC_w", file = "./shiny/NC_w.RData", compress = "gzip")
save("NC_raw", file = "./shiny/NC_raw.RData", compress = "gzip")

write.table(NC_l, file = gzfile(paste0("./shiny/www/", META$FILENAME_LONG)), sep = ",", na = "", row.names = FALSE)
write.table(NC_w, file = gzfile(paste0("./shiny/www/", META$FILENAME_WIDE)), sep = ",", na = "", row.names = FALSE)