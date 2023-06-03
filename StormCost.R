# Required Libraries -----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)


# PROGRAM CONSTANTS-------------------------------------------------------------
VERBOSE         = FALSE     # TRUE  FALSE
DEBUG           = TRUE
TEST            = TRUE

COUNTY_SOURCE_INDEX      = 1
STATE_SOURCE_INDEX       = 2
NATIONAL_SOURCE_INDEX    = 3
COUNTY_COST_INDEX        = 4
STATE_COST_INDEX         = 5
NATIONAL_COST_INDEX      = 6
COUNTY_CASUALTY_INDEX    = 7
STATE_CASUALTY_INDEX     = 8
NATIONAL_CASUALTY_INDEX  = 9

REGION_NAME = c('NATIONAL', 'STATE', 'COUNTY',  
                'NATIONAL_HUMAN_COST', 'STATE_HUMAN_COST', 'COUNTY_HUMAN_COST', 
                'NATIONAL_ECON_COST', 'STATE_ECON_COST', 'COUNTY_ECON_COST')

NATIONAL_FIELD <- "c('EVTYPE', 'HUMAN_COST', 'ECON_COST')"
STATE_FIELD    <- "c('EVTYPE', 'STATE', 'HUMAN_COST', 'ECON_COST')"
COUNTY_FIELD   <- "c('EVTYPE', 'STATE', 'COUNTY', 'HUMAN_COST', 'ECON_COST')"

GOVT_FIELD <- list(NATIONAL_FIELD, STATE_FIELD, COUNTY_FIELD)

GOVT_FILTER <-  list( 
  ".$'COUNTY' == COUNTY_ & .$'STATE' == STATE_",
  ".$'STATE' == STATE_",
  NULL
) # end GOVT_FILTER

# USER DEFINED CONSTANTS  ------------------------------------------------------
URL      = 
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
DATAFILE = "storm.csv.bz2"

STATE_    <- "CA"
COUNTY_   <- "SAN BERNARDINO"

# ##############################################################################
# Program Control Phase
# ##############################################################################
Main <- function() {
  govtTableList <- GenerateDisasterTables(URL, DATAFILE)
  resultList   <- GenerateDisasterData(govtTableList)
  
  return (resultList)
}

# ##############################################################################
# Generate Analysis Data for Each Government Level
# ##############################################################################

# GenerateDisasterTables #######################################################
GenerateDisasterTables <- function(url, dataFile)  {
  if (VERBOSE) {
    print("GenerateDisasterTables(): coordinate the generation of the ")
    print("govtTableList from the origional source file")
  }
  
  # If the download file is not in the project directory, download it ----------  
  if (!file.exists(dataFile)) {
    DownloadDataFile(url, destfile)
  } else {
    if (VERBOSE) {
      print(paste0(dataFile, " Contents already loaded "))
    }
  }

  rawData <- LoadDisasterData(url, dataFile)
  ## Set up the aggregated financial and casualty data  ------------------------
  sourceData <- TransformRawToSource(rawData)
  
  sourceTableList <- GenerateSourceTables(sourceData)
  govtTableList <- groupGovtTable(sourceTableList)
  
  return (govtTableList)
}  ## End of GenerateDisasterTables function


# DownloadDataFile #############################################################
DownloadDataFile <- function(url, destfile)
{
  print(paste0("downloadDataFile: Download ", destfile, " from\n", url))

  retrieve = download.file(url, destfile, mode = "wb")
  if (is.null(retrieve)) {
    print("ERROR: could not access ", url)
    stop()
  }
}  ## End of DownloadDataFile function

# ##############################################################################  
LoadDisasterData <- function(dataFile)
{
  if (VERBOSE) {
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
  
  if (VERBOSE) {
    print("Display raw data ")
    print(head(rawData))
  }
  
  rawData <- CleanRawData(rawData)
  
  return (rawData)
}  ## End of LoadDisasterData function

# ##############################################################################  
CleanRawData <- function(rawData)
{
  if (VERBOSE) {
    print("CleanRawData: Set headers, transform data to numeric ")
  }

  ## convert data used for computation from string to numeric ------------------
  rawData <- rawData %>%
    mutate_at(c('FATALITIES', 'INJURIES', 'PROPDMG', 'CROPDMG'), as.numeric)

  if (VERBOSE) {
    print("CleanRawData: check raw data")
    print(head(rawData))
  }

  return (rawData)
}  ## End of CleanRawData function

# ##############################################################################  
TransformRawToSource <- function(rawData) 
{
  if (VERBOSE) {
    print("TransformRawToSource: add aggregate casualties and damage data")
  }

  sourceData <- rawData %>% # ungroup(.) %>%
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

  if (VERBOSE) {
    print("TransformRawToSource: check source data")
    print(head(sourceData))
  }

  return (sourceData)
}

# ##############################################################################  
GenerateSourceTablesList <- function(sourceData) {
  if (VERBOSE) {
    print("GenerateSourceTables: convert raw data to tables with the containing")
  }

  sourceTableList = list()
  for (i in COUNTY_SOURCE_INDEX : NATIONAL_SOURCE_INDEX) {
    if (is.null(GOVT_FILTER[[i]])) {
      sourceTable <- sourceData %>% 
        select(eval(parse(text = GOVT_FIELD[i])))
    } else {
      sourceTable <- sourceData %>% 
        select(eval(parse(text = GOVT_FIELD[i]))) %>% 
        filter(eval(parse(text = GOVT_FILTER[i])))
    }
    # this is govtFieldTables 2-4
    sourceTableList[[length(sourceTableList)+1]] <- sourceTable
  }

  if (VERBOSE) {
    print("GenerateSourceTables: check source data frames")
    
    for (sourceTable in sourceTableList) {
      print(head(sourceTable))
    }
  }

  return(sourceTableList)
}

# ##############################################################################  
GovtEventsTable <- function(govtTableList)  {
  if (VERBOSE) {
    print("GovtEventsTable: group the rawTables to govt tables grouped ")
    print("by event type")
  }

  mergeTableList = list()
  for (govt in COUNTY_SOURCE_INDEX : NATIONAL_SOURCE_INDEX) {
    govtEventsTable <- govtTableList[[govt]] %>% group_by(EVTYPE)

    # summarize Govt Events Table by human cost and sort by event
    humanCost <- govtEventsTable %>% 
      summarise(EVENT_HUMAN_COST = sum(HUMAN_COST), .groups = 'drop')
    humanCost <- humanCost %>% arrange(desc(.$EVTYPE), 
                                       .by_group = TRUE)
    if (DEBUG) {
      print("GovtEventsTable: humanCost")
      print(head(humanCost))
    }

    # summarize Govt Events Table by economic cost and sort by event
    econCost <- govtEventsTable %>% 
      summarise(EVENT_ECON_COST  = sum(ECON_COST), .groups = 'drop')
    econCost <- econCost %>% arrange(desc(.$EVTYPE), .by_group = TRUE)
    if (DEBUG) {
      print("GovtEventsTable: econCost")
      print(head(econCost))
    }
    
    # merge human and economic cost summaries
    mergeCost <- merge(humanCost, econCost, 
                       by.x = c('EVTYPE'), by.y = c('EVTYPE'))

    mergeTableList[[length(mergeTableList)+1]] <- mergeCost
  }

  for (govt in COUNTY_SOURCE_INDEX : NATIONAL_SOURCE_INDEX) {
    # govtEventsTable <-  %>% group_by(EVTYPE)

    humanOrder <- mergeTableList[[govt]] %>% arrange(desc(.$EVENT_HUMAN_COST), 
                                         .by_group = TRUE)
    govtTableList[[length(govtTableList)+1]] <- humanOrder
  }

  for (govt in COUNTY_SOURCE_INDEX : NATIONAL_SOURCE_INDEX) {
    costOrder  <- mergeTableList[[govt]] %>% arrange(desc(.$EVENT_ECON_COST), 
                                         .by_group = TRUE)
    govtTableList[[length(govtTableList)+1]] <- costOrder
  }

  rm (mergeTableList)

  return (govtTableList)
}  ## End of finalDataTransform function

# COUNTY_SOURCE_INDEX      = 1
# STATE_SOURCE_INDEX       = 2
# NATIONAL_SOURCE_INDEX    = 3
# COUNTY_COST_INDEX        = 4
# STATE_COST_INDEX         = 5
# NATIONAL_COST_INDEX      = 6
# COUNTY_CASUALTY_INDEX    = 7
# STATE_CASUALTY_INDEX     = 8
# NATIONAL_CASUALTY_INDEX  = 9


print("That's all folks")

# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################


# ##############################################################################  
test_LoadDisasterData <- function() {
  print("TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST")
  print("test_LoadDisasterData")
  
  rawData <- LoadDisasterData(DATAFILE)
  
  print("test_LoadDisasterData raw data ######################################")
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
  sourceTableList <- GenerateSourceTables(sourceData)
  
  print("test_GenerateSourceTables source disaster list ######################")
  for (sourceTable in sourceTableList) {
    print(head(sourceTable))
  }

  return (sourceTableList)
}

test_GovtEventsTable <- function(sourceTableList) {
  govtTableList <- GovtEventsTable(sourceTableList)
  
  print("test_GovtEventsTable govt disaster list #############################")
  i = 1
  for (govtTable in govtTableList) {
    print(paste0("List[", i, "]"))
    print(head(govtTable))
    i = i + 1
  }
}

if (TEST) {
  rawData         <- test_LoadDisasterData()
  cleanData       <- test_CleanRawData(rawData)
  sourceData      <- test_TransformRawToSource(cleanData)
  sourceTableList <- test_GenerateSourceTables(sourceData)
  govtTableList   <- test_GovtEventsTable(sourceTableList)
}



# ##############################################################################
# 
# ##############################################################################
# showRawTable <- function(govtTableList, i, stateName, countyName) {
#   
#   if (countyName != "") {
#     print(paste0("Raw financial and casualty data for ",  
#                  COUNTYNAME_, ", ",
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else if (stateName != "") {
#     print(paste0("Raw financial and casualty data for ", 
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else  {
#     print("Raw financial and casualty data for U.S.")
#     table <- govtTableList[i]
#     print(head(data.frame(govtTableList[i]), 10))
#   }
# }
# 
# showByCost <- function(govtTableList, i, stateName, countyName) {
#   
#   if (countyName != "") {
#     print(paste0("Raw financial and casualty data ordered by financial impact ",  
#                  COUNTYNAME_, ", ",
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else if (stateName != "") {
#     print(paste0("Raw financial and casualty data ordered by financial impact ", 
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else  {
#     print("Raw financial and casualty data ordered by financial impact U.S. ")
#     table <- govtTableList[i]
#     print(head(data.frame(govtTableList[i]), 10))
#   }
# }
# 
# showByCasualty <- function(govtTableList, i, stateName, countyName) {
#   if (countyName != "") {
#     print(paste0("Raw financial and casualty data ordered by casualties ",  
#                  COUNTYNAME_, ", ",
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else if (stateName != "") {
#     print(paste0("Raw financial and casualty data ordered by casualties ",
#                  STATE_))
#     print(head(data.frame(govtTableList[i]), 10))
#   } else {
#     print("Raw financial and casualty data ordered by casualties U.S.")
#     table <- govtTableList[i]
#     print(head(data.frame(govtTableList[i]), 10))
#   }
# }
# 
# 
# GenerateDisasterData <- function(govtTableList) {
#   
#   showRawTable(govtTableList, NATIONAL_SOURCE_INDEX, "", "")
#   showRawTable(govtTableList, STATE_SOURCE_INDEX, STATE_, "")
#   showRawTable(govtTableList, COUNTY_SOURCE_INDEX, STATE_, COUNTYNAME_)
#   
#   showByCost(govtTableList, NATIONAL_COST_INDEX, "", "")
#   showByCost(govtTableList, STATE_COST_INDEX, STATE_, "")
#   showByCost(govtTableList, COUNTY_COST_INDEX, STATE_, COUNTYNAME_)
#   
#   showByCasualty(govtTableList, NATIONAL_CASUALTY_INDEX, "", "")
#   showByCasualty(govtTableList, STATE_CASUALTY_INDEX, STATE_, "")
#   showByCasualty(govtTableList, COUNTY_CASUALTY_INDEX, STATE_, COUNTYNAME_)
#   
#   # scatter plot of COST vs CASUALTY from group_results
#   # bar plots of COST vs CASUALTY from byCost and byCasualty
#   # bar plots of COST vs CASUALTY from byCost and byCasualty
#   
#   # print("list test")
#   # print(class(govtTableList[NATIONAL_COST_INDEX]))
#   
#   data <- data.frame(govtTableList[NATIONAL_COST_INDEX])
#   print("data.frame test")
#   print(class(data))
#   
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
