# ##############################################################################
# ##############################################################################
# 
# Setup Phase
# 
# ##############################################################################
# ##############################################################################

# Required Libraries -----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)


# PROGRAM CONSTANTS-------------------------------------------------------------
VERBOSE         = FALSE     # TRUE  FALSE
DEBUG           = FALSE
TEST            = TRUE
SYSTEM          = FALSE

ALL_FUNCTIONS                = FALSE
MAIN                         = FALSE
GENERATE_STORM_COST_TABLES     = TRUE
DOWNLOAD_DATA_FILE           = FALSE
LOAD_STORM_COST_DATA           = FALSE
CLEAN_RAW_DATA               = FALSE
TRANSFORM_RAW_TO_SOURCE      = FALSE
GENERATE_SOURCE_TABLE_LIST   = FALSE
GET_HUMAN_COST               = FALSE
GET_ECONOMIC_COST            = FALSE
MERGED_EVENT_LIST            = FALSE
GENERATE_AGENCY_MERGED_TABLE = FALSE

GENERATE_STORM_COST_ANALYSIS   = FALSE
GENERATE_TOP10_HUMAN_TABLE     = TRUE
GENERATE_TOP10_ECONOMCIC_TABLE = FALSE
GENERATE_CVC_IMAGE             = TRUE

PHASE_SETUP         = FALSE
PHASE_GENERATE_DATA = FALSE
PHASE_ANALYSIS      = TRUE
PHASE_CONTROL       = FALSE

COUNTY_SOURCE_INDEX      = 1
STATE_SOURCE_INDEX       = 2
FEDERAL_SOURCE_INDEX     = 3
COUNTY_HUMAN_INDEX       = 4
STATE_HUMAN_INDEX        = 5
FEDERAL_HUMAN_INDEX      = 6
COUNTY_ECONOMIC_INDEX    = 7
STATE_ECONOMIC_INDEX     = 8
FEDERAL_ECONOMIC_INDEX   = 9

HUMAN_COSTS      = 3        
ECONOMIC_COSTS   = 6        

REGION_NAME = c('COUNTY', 'STATE', 'FEDERAL',   
                'COUNTY_HUMAN_COST', 'STATE_HUMAN_COST', 'FEDERAL_HUMAN_COST',  
                'COUNTY_ECON_COST', 'STATE_ECON_COST', 'FEDERAL_ECON_COST')

COUNTY_FIELD   <- "c('EVTYPE', 'STATE', 'COUNTY', 'HUMAN_COST', 'ECON_COST')"
STATE_FIELD    <- "c('EVTYPE', 'STATE', 'HUMAN_COST', 'ECON_COST')"
FEDERAL_FIELD  <- "c('EVTYPE', 'HUMAN_COST', 'ECON_COST')"

AGENCY_FIELD     <- list(COUNTY_FIELD, STATE_FIELD, FEDERAL_FIELD)

AGENCY_FILTER    <-  list( 
  ".$'COUNTY' == COUNTY_ & .$'STATE' == STATE_",
  ".$'STATE' == STATE_",
  NULL
) # end AGENCY_FILTER

# USER DEFINED CONSTANTS  ------------------------------------------------------
URL      = 
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
DATAFILE = "storm.csv.bz2"

STATE_    <- "CA"
COUNTY_   <- "SAN BERNARDINO"

# ##############################################################################
# ##############################################################################
# 
# Program Control Phase
# 
# ##############################################################################
# ##############################################################################

Main <- function() {
  agencyTableList <- GenerateStormCostTables(URL, DATAFILE)
  resultList   <- GenerateStormCostAnalysis(agencyTableList)
  
  if (VERBOSE) {
    print("That's all folks")
  }

  return (resultList)
}

# ##############################################################################
# ##############################################################################
# 
# Generate Analysis Data Phase
# 
# ##############################################################################
# ##############################################################################

# GenerateStormCostTables #######################################################
GenerateStormCostTables <- function(url, dataFile)  {
  if (VERBOSE & GENERATE_STORM_COST_TABLES) {
    print("GenerateStormCostTables(): coordinate the generation of the ")
    print("agencyTableList from the origional source file")
  }
  
  # If the download file is not in the project directory, download it ----------  
  if (!file.exists(dataFile)) {
    DownloadDataFile(url, destfile)
  } else {
    if (VERBOSE & GENERATE_STORM_COST_TABLES) {
      print(paste0(dataFile, " Contents already loaded "))
    }
  }

  rawData <- LoadStormCostData(dataFile)
  ## Set up the aggregated financial and casualty data  ------------------------
  sourceData <- TransformRawToSource(rawData)
  
  sourceTableList <- GenerateSourceTableList(sourceData)
  agencyTableList <- GenerateAgencyMergedTable(sourceTableList)
  
  return (agencyTableList)
}  ## End of GenerateStormCostTables function

# DownloadDataFile #############################################################
DownloadDataFile <- function(url, destfile)
{
  if (VERBOSE & DOWNLOAD_DATA_FILE) {
    print(paste0("downloadDataFile: Download ", destfile, " from\n", url))
  }

  retrieve = download.file(url, destfile, mode = "wb")
  if (is.null(retrieve)) {
    print("ERROR: could not access ", url)
    stop()
  }
}  ## End of DownloadDataFile function
# LoadStormCostData #############################################################
LoadStormCostData <- function(dataFile)
{
  if (VERBOSE & LOAD_STORM_COST_DATA) {
    print(paste0("loadRawData: Reading raw storm data from ", dataFile))
  }

  ## set up table from the input file ------------------------------------------
  rawData <- read.table(dataFile, stringsAsFactors = FALSE, sep = ",",
                        colClasses = c(rep("NULL",5), NA, NA, NA, 
                                       rep("NULL",14), rep(NA, 6), 
                                       rep("NULL",9)),
                        header = TRUE)
  if(is.null(rawData)) {
    print(paste0("ERROR: could not read ", dataFile))
    stop()
  }
  
  if (VERBOSE & LOAD_STORM_COST_DATA) {
    print("Display raw data ")
    print(head(rawData))
  }
  
  rawData <- CleanRawData(rawData)
  
  return (rawData)
}  ## End of LoadStormCostData function

# CleanRawData #################################################################
CleanRawData <- function(rawData)
{
  if (VERBOSE & CLEAN_RAW_DATA) {
    print("CleanRawData: Set headers, transform data to numeric ")
  }

  ## convert data used for computation from string to numeric ------------------
  rawData <- rawData %>%
    mutate_at(c('FATALITIES', 'INJURIES', 'PROPDMG', 'CROPDMG'), as.numeric)

  rawData <- NormalizeEventTypes(rawData)
  
  
  if (VERBOSE & CLEAN_RAW_DATA) {
    print("CleanRawData: check raw data")
    print(head(rawData))
  }
  return (rawData)
}  ## End of CleanRawData function

# CleanRawData #################################################################
NormalizeEventTypes <- function(rawData) {
  return (rawData)
}

# TransformRawToSource #########################################################
TransformRawToSource <- function(rawData) 
{
  if (VERBOSE & TRANSFORM_RAW_TO_SOURCE) {
    print("TransformRawToSource: add aggregate casualties and damage data")
  }

  sourceData <- rawData %>% ungroup(.) %>%
    mutate (PROPDMGEXP = ifelse(PROPDMGEXP == 'K', 1000.,
                                ifelse(PROPDMGEXP == 'M', 1000000., 1.) ),
            CROPDMGEXP = ifelse(CROPDMGEXP == 'K', 1000.,
                                ifelse(CROPDMGEXP == 'M', 1000000., 1.) ),
            PROPDMG       = PROPDMG * PROPDMGEXP,
            CROPDMG       = CROPDMG * CROPDMGEXP,
            FATALITIES    = FATALITIES,
            INJURIES      = INJURIES,
            HUMAN_COST    = FATALITIES + INJURIES,
            ECON_COST     = PROPDMG + CROPDMG
            ) %>% 
    select(COUNTYNAME, STATE, EVTYPE, HUMAN_COST, ECON_COST)

  colnames(sourceData) <- 
    c("COUNTY", "STATE", "EVTYPE", "HUMAN_COST", "ECON_COST")

  if ((VERBOSE | DEBUG) & TRANSFORM_RAW_TO_SOURCE) {
    print("TransformRawToSource: check source data")
    print(head(sourceData))
  }

  return (sourceData)
}  ## End of TransformRawToSource function

# GenerateSourceTableList #####################################################
GenerateSourceTableList <- function(sourceData) {
  if (VERBOSE & GENERATE_SOURCE_TABLE_LIST) {
    print("GenerateSourceTableList: convert raw data to tables with the containing")
  }

  sourceTableList = list()
  for (i in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    sourceTable <- sourceData %>% 
      select(eval(parse(text = AGENCY_FIELD[i])))

    if (!is.null(AGENCY_FILTER[[i]])) {
      sourceTable <- sourceTable %>% 
        filter(eval(parse(text = AGENCY_FILTER[[i]])))
    }

    # this is agencyFieldTables 2-4
    sourceTableList[[length(sourceTableList)+1]] <- sourceTable
  }

  if (VERBOSE & GENERATE_SOURCE_TABLE_LIST) {
    print("GenerateSourceTableList: check source data frames")
    
    for (sourceTable in sourceTableList) {
      print(head(sourceTable))
    }
  }

  return(sourceTableList)
}  ## End of GenerateSourceTableList function

# GetHumanCost #####################################################
GetHumanCost <- function(agencyEventsTable){
  if (VERBOSE & GET_HUMAN_COST) {
    print("GetHumanCost: generate an ordered list of human cost by agency")
  }
  # summarize Agency Events Table by human cost and sort by event
  humanCost <- agencyEventsTable %>% 
    summarise(EVENT_HUMAN_COST = sum(HUMAN_COST), .groups = 'drop')
  humanCost <- humanCost %>% arrange(desc(.$EVTYPE), 
                                     .by_group = TRUE)
  if (DEBUG & GET_HUMAN_COST) {
    print("AgencyEventsTable: humanCost")
    print(head(humanCost))
  }
  
  return(humanCost)
} ## End of GetHumanCost function


# GetEconomicCost #####################################################
GetEconomicCost <- function(agencyEventsTable) {
  if (VERBOSE & GET_ECONOMIC_COST) {
    print("GetEconomicCost: generate ordered list of economic cost by agency")
  }
  # summarize Agency Events Table by economic cost and sort by event
  econCost <- agencyEventsTable %>% 
    summarise(EVENT_ECON_COST  = sum(ECON_COST), .groups = 'drop')
  econCost <- econCost %>% arrange(desc(.$EVTYPE), .by_group = TRUE)
  if (DEBUG & GET_ECONOMIC_COST) {
    print("AgencyEventsTable: econCost")
    print(head(econCost))
  }
  return(econCost)
} ## End of GetEconomicCost function

# MergedEventList #####################################################
MergedEventList <- function(agencyTableList){
  if (VERBOSE & MERGED_EVENT_LIST) {
    print("MergedEventList: generate an ordered list of economic cost by agency")
  }
  mergeTableList = list()
  
  for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    agencyEventsTable <- agencyTableList[[agency]] %>% group_by(EVTYPE)
    
    humanCost <- GetHumanCost(agencyEventsTable)
    econCost  <- GetEconomicCost(agencyEventsTable)
    # merge human and economic cost summaries
    mergeCost <- merge(humanCost, econCost, 
                       by.x = c('EVTYPE'), by.y = c('EVTYPE'))

    mergeTableList[[length(mergeTableList)+1]] <- mergeCost
  }
  
  return (mergeTableList)  
} ## End of MergedEventList function

# GenerateAgencyMergedTable #####################################################
GenerateAgencyMergedTable <- function(agencyTableList)  {
  if (VERBOSE & GENERATE_AGENCY_MERGED_TABLE) {
    print("GenerateAgencyMergedTable: group the rawTables to agency tables grouped ")
    print("by event type")
  }

  mergeTableList <- MergedEventList(agencyTableList)
  for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    humanOrder <- mergeTableList[[agency]] %>% arrange(desc(.$EVENT_HUMAN_COST), 
                                         .by_group = TRUE)
    agencyTableList[[length(agencyTableList)+1]] <- humanOrder
  }

  for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    costOrder  <- mergeTableList[[agency]] %>% arrange(desc(.$EVENT_ECON_COST), 
                                         .by_group = TRUE)
    agencyTableList[[length(agencyTableList)+1]] <- costOrder
  }

  rm (mergeTableList)

  return (agencyTableList)
}  ## End of GenerateAgencyMergedTable function

# ##############################################################################
# ##############################################################################
# 
# Analysis and Visualization Phase
# 
# ##############################################################################
# ##############################################################################

# GenerateStormCostAnalysis #####################################################
GenerateStormCostAnalysis <- function(agencyTableList) {
  if (VERBOSE & GENERATE_STORM_COST_ANALYSIS) {
    print("GenerateStormCostAnalysis: group the rawTables to agency ")
    print("tables grouped by event type")
  }

  ## Generate StormCost Tables (Top 10)
  humanCostList    <- GenerateTop10HumanTable(agencyTableList)
  economicCostList <- GenerateTop10EconomicTable(agencyTableList)
  
  ## Generate Human v. Economic Cost Plots
  # cvcPlots <- GenerateCvC(humanCostList)
  
}

# GenerateTop10HumanTable #####################################################
GenerateTop10HumanTable <- function(agencyTableList) {
  if ((VERBOSE | DEBUG) & GENERATE_TOP10_HUMAN_TABLE) {
    print("GenerateTop10HumanTable: starting data")
    print(head(agencyTableList))
  }

  humanCostList = list()
  for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    humanCostTitle <- switch (agency, 
            paste0("Top 10 Weather StormCosts, (Human Costs)\n", 
                   COUNTY_, " ", STATE_),
            paste0("Top 10 Weather StormCosts, (Human Costs)\n",
                   state.name[grep(STATE_, state.abb)]),
            "Top 10 Weather StormCosts, (Human Costs\n)"
    )
    humanCostList[length(humanCostList) + 1] = humanCostTitle
    humanCostList[length(humanCostList) + 1] = 
      head(agencyTableList[agency + HUMAN_COSTS])
  }

  return(humanCostList)
}

# GenerateTop10EconomicTable #####################################################
GenerateTop10EconomicTable <- function(agencyTableList) {
  if ((VERBOSE | DEBUG) & GENERATE_TOP10_ECONOMCIC_TABLE) {
    print("GenerateTop10HumanTable: starting data")
    print(head(agencyTableList))
  }

  economicCostList = list()
  for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
    economicCostTitle <- switch (agency, 
                              paste0("Top 10 Weather StormCosts, (Human Costs)\n", 
                                     COUNTY_, " ", STATE_),
                              paste0("Top 10 Weather StormCosts, (Human Costs)\n",
                                     state.name[grep(STATE_, state.abb)]),
                              "Top 10 Weather StormCosts, (Human Costs\n)"
    )
    economicCostList[length(economicCostList) + 1] = economicCostTitle
    economicCostList[length(economicCostList) + 1] = 
      head(agencyTableList[agency + ECONOMIC_COSTS], 10)
  }

  return(economicCostList)
}

# # GenerateCvC #####################################################
# GenerateCvC <- function(cvcTable) {
#   if ((VERBOSE | DEBUG) & GENERATE_CVC_IMAGE) {
#     print("GenerateCvC: starting processing")
#   }
# 
#   cvcImageList <- list()
# 
#   # print(paste0("agency list ", agencyTableList[1]))
#   for (agency in COUNTY_SOURCE_INDEX : FEDERAL_SOURCE_INDEX) {
#     print(head(cvcTable[agency + 2]))
#     sp <- switch (cvcTable[agency + 2],
#                   ggplot(cvcTable, aes(x = EVENT_HUMAN_COST,
#                                           y = EVENT_ECON_COST))
#                   )
#     cvcImageList[length(cvcImageList) + 1] <- sp
#   }
# 
#   return(cvcImageList)
# }
# 
# 

#   if (countyName != "") {
#     print(paste0("Raw financial and casualty data ordered by financial impact ",
#                  COUNTY_, ", ",
#                  STATE_))
#     print(head(data.frame(agencyTableList[i]), 10))
#   } else if (stateName != "") {
#     print(paste0("Raw financial and casualty data ordered by financial impact ",
#                  STATE_))
#     print(head(data.frame(agencyTableList[i]), 10))
#   } else  {
#     print("Raw financial and casualty data ordered by financial impact U.S. ")
#     table <- agencyTableList[i]
#     print(head(data.frame(agencyTableList[i]), 10))
#   }
# }


# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################


# ##############################################################################  
test_LoadStormCostData <- function() {
  print("TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST")
  print("test_LoadStormCostData")
  
  rawData <- LoadStormCostData(DATAFILE)
  
  print("test_LoadStormCostData raw data ######################################")
  print(head(rawData))
  
  return (rawData)
}

test_CleanRawData <- function(rawData) {
  cleanData <- CleanRawData(rawData)
  
  print("test_CleanRawData cleaned data ######################################")
  print(head(cleanData))
  
  
  return (cleanData)
  
}

test_TransformRawToSource <- function(cleanData) {
  sourceData <- TransformRawToSource(cleanData)
  
  print("test_TransformRawToSource transformed data ##########################")
  print(head(sourceData))

  return (sourceData)
}

test_GenerateSourceTables <- function(sourceData) {
  sourceTableList <- GenerateSourceTableList(sourceData)
  
  print("test_GenerateSourceTables source storm cost list ######################")
  for (sourceTable in sourceTableList) {
    print(head(sourceTable))
  }

  return (sourceTableList)
}

test_GenerateAgencyMergedTable <- function(sourceTableList) {
  agencyTableList <- GenerateAgencyMergedTable(sourceTableList)
  
  print("test_GenerateAgencyMergedTable agency storm cost list #################")
  i = 1
  for (agencyTable in agencyTableList) {
    print(paste0("List[", i, "]"))
    print(head(agencyTable))
    i = i + 1
  }
}

if (TEST & PHASE_GENERATE_DATA) {
  rawData         <- test_LoadStormCostData()
  cleanData       <- test_CleanRawData(rawData)
  sourceData      <- test_TransformRawToSource(cleanData)
  sourceTableList <- test_GenerateSourceTables(sourceData)
  agencyTableList <- test_AgencyEventsTable(sourceTableList)
}

test_GenerateStormCostTables <- function(url, datafile) {
  agencyTableList <- GenerateStormCostTables(url, datafile)

  print("test_GenerateAgencyMergedTable agency storm cost list #####################")
  i = 1
  for (agencyTable in agencyTableList) {
    print(paste0("List[", i, "]"))
    print(head(agencyTable))
    i = i + 1
  }
}

if (SYSTEM & PHASE_GENERATE_DATA) {
  agencyTableList <- GenerateStormCostTables(URL, DATAFILE)
  i = 1
  for (agencyTable in agencyTableList) {
    print(paste0("List[", i, "]"))
    print(head(agencyTable))
    i = i + 1
  }
}

# ##############################################################################
test_GenerateTop10HumanTable <- function(agencyTableList) {
  humanCostsTable <- GenerateTop10HumanTable(agencyTableList)
  i = 1
  for (humanCostTable in humanCostsTable) {
    print(paste0("List[", i, "]"))
    print(head(humanCostTable))
    i = i + 1
  }
  print("humanList")
  print(humanCostsTable)
  write.csv(humanCostsTable[6], "./us_groups-orig.csv")
}

test_GenerateTop10EconomicTable <- function(agencyTableList) {
  economicCostsTable <- GenerateTop10EconomicTable(agencyTableList)
  i = 1
  for (economicCostTable in economicCostsTable) {
    print(paste0("List[", i, "]"))
    print(head(economicCostsTable))
    i = i + 1
  }
}

test_GenerateCvC <- function(agencyTableList) {
  cvcImages <- GenerateCvC(agencyTableList)
  png("countyCvC.png")
  print(cvcImages[1])
  dev.off()
}

if (SYSTEM & PHASE_ANALYSIS) {
  # agencyTableList <- GenerateStormCostTables(URL, DATAFILE)
  resultList      <- GenerateStormCostAnalysis(agencyTableList)
  i = 1
  for (agencyTable in resultList) {
    print(paste0("List[", i, "]"))
    print(head(agencyTable))
    i = i + 1
  }
  
}

if (TEST & PHASE_ANALYSIS) {
  agencyTableList <- GenerateStormCostTables(URL, DATAFILE)
  if (GENERATE_TOP10_HUMAN_TABLE) {
    humanList       <- test_GenerateTop10HumanTable(agencyTableList)
  }
  if (GENERATE_TOP10_ECONOMCIC_TABLE) {
    econList       <- test_GenerateTop10EconomicTable(agencyTableList)
  }
  # if (GENERATE_CVC_IMAGE) {
  #   cvcImages       <- test_GenerateCvC(resultList)
  # }

}
# ##############################################################################
# 
# ##############################################################################
# GenerateStormCostData <- function(agencyTableList) {

#   png(file="plot4.png", width=480, height=480)
#   
#   plot( 
#     data$GROUP_CASUALTIES, 
#     data$GROUP_COST,
#     main="Number of Casualties",
#     xlab="Cost of event", 
#     ylab="Miles Per Gallon ", 
#     ylim=c(0,500000000),
#     xlim=c(0,100),
#     pch=19
#   )
#   
#   dev.off()
# }
