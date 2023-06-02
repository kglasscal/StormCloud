# Required Libraries -----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)


# PROGRAM CONSTANTS-------------------------------------------------------------
VERBOSE         = FALSE     # TRUE  FALSE
DEBUG           = FALSE
TEST            = TRUE

NATIONAL_SOURCE_INDEX    = 1
STATE_SOURCE_INDEX       = 2
COUNTY_SOURCE_INDEX      = 3
NATIONAL_COST_INDEX      = 4
NATIONAL_CASUALTY_INDEX  = 5
STATE_COST_INDEX         = 6
STATE_CASUALTY_INDEX     = 7
COUNTY_COST_INDEX        = 8
COUNTY_CASUALTY_INDEX    = 9

REGION_NAME = c('NATIONAL', 'STATE', 'COUNTY', 'NATIONAL_COST', 
                'NATIONAL_CASUALTY', 'STATE_COST', 'STATE_CASUALTY', 
                'COUNTY_COST', 'COUNTY_CASUALTY')

NATIONAL_FIELD <- "c('EVTYPE', 'TOTALFIN', 'TOTALCASUALTY')"
STATE_FIELD    <- "c('EVTYPE', 'STATE', 'TOTALFIN', 'TOTALCASUALTY')"
COUNTY_FIELD   <- "c('EVTYPE', 'STATE', 'COUNTYNAME', 'TOTALFIN', 
'TOTALCASUALTY')"

REGIONAL_FIELD <- list(NATIONAL_FIELD, STATE_FIELD, COUNTY_FIELD)

REGIONAL_FILTER <-  list( 
  NULL, 
  ".$'STATE' == STATE_",
  ".$'COUNTY' == COUNTYNAME_ & .$'STATE' == STATE_"
) # end REGIONAL_FILTER

# USER DEFINED CONSTANTS  ------------------------------------------------------
URL      = 
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
DATAFILE = "storm.csv.bz2"

STATE_          <- "CA"
COUNTYNAME_     <- "SAN BERNARDINO"

# ##############################################################################
# Program Control Phase
# ##############################################################################
Main <- function() {
  regionalTableList <- GenerateDisasterTables(URL, DATAFILE)
  resultList   <- GenerateDisasterData(regionalTableList)
  
  return (resultList)
}

# ##############################################################################
# Generate Analysis Data for Each Government Level
# ##############################################################################

# GenerateDisasterTables #######################################################
GenerateDisasterTables <- function(url, dataFile)  {
  if (VERBOSE) {
    print("GenerateDisasterTables(): coordinate the generation of the ")
    print("regionalTableList from the origional source file")
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
  
  sourceTableList <- generateSourceTable(sourceData)
  regionalTableList <- groupRegionalTable(sourceTableList)
  
  return (regionalTableList)
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
  print(paste0("loadRawData: Reading raw storm data from ", dataFile))
  
  ## set up table from the input file ------------------------------------------
  rawData <- read.table(dataFile, stringsAsFactors = FALSE, sep = ",",
                        colClasses = c(rep("NULL",5), NA, NA, NA, 
                                       rep("NULL",14), rep(NA, 6), 
                                       rep("NULL",9)))
  if(is.null(rawData)) {
    print(paste0("ERROR: could not read ", dataFile))
    stop()
  }
  
  rawData <- CleanRawData(rawData)
  
  if (VERBOSE) {
    print("Display raw data ")
    print(head(rawData))
  }
  
  return (rawData)
}  ## End of LoadDisasterData function

# ##############################################################################  
CleanRawData <- function(rawData)
{
  names(rawData) <- rawData[1,]
  rawData <- rawData[-1,]
  rownames(rawData) <- 1:nrow(rawData)
  
  ## convert data used for computation from string to numeric ------------------
  rawData <- rawData %>%
    mutate_at(c('FATALITIES', 'INJURIES', 'PROPDMG', 'CROPDMG'), as.numeric)
  
  return (rawData)
}  ## End of CleanRawData function

# ##############################################################################  
transformRawData <- function(rawData) 
{
  print("transformRawData: add aggregate casualties and damage data")
  
  sourceData <- rawData %>% # ungroup(.) %>%
    mutate (PROPDMGEXP = ifelse(PROPDMGEXP == 'K', 1000.,
                                ifelse(PROPDMGEXP == 'M', 1000000., 1.) ),
            CROPDMGEXP = ifelse(CROPDMGEXP == 'K', 1000.,
                                ifelse(CROPDMGEXP == 'M', 1000000., 1.) ),
            PROPDMG       = PROPDMG * PROPDMGEXP,
            CROPDMG       = CROPDMG * CROPDMGEXP,
            FATALITIES    = FATALITIES,
            INJURIES      = INJURIES,
            TOTALFIN      = PROPDMG + CROPDMG,
            TOTALCASUALTY = FATALITIES + INJURIES
    )
  
  return (sourceData)
}

# ##############################################################################  
generateSourceTable <- function(sourceData) {
  print("generateSourceTable(): convert raw data to tables with the containing")
  
  sourceTableList = list()
  for (i in NATIONAL_SOURCE_INDEX : COUNTY_SOURCE_INDEX) {
    if (is.null(REGIONAL_FILTER[[i]])) {
      sourceTable <- sourceData %>% select(eval(parse(text = REGIONAL_FIELD[i])))
    } else {
      sourceTable <- sourceData %>% select(eval(parse(text = REGIONAL_FIELD[i]))) %>% 
        filter(eval(parse(text = REGIONAL_FILTER[i])))
    }
    # this is regionalFieldTables 2-4
    sourceTableList[[length(sourceTableList)+1]] <- sourceTable
  }
  
  return(sourceTableList)
}

# ##############################################################################  
groupRegionalTable <- function(regionalTableList)  {
  print("groupRegionalTable(): group the rawTables to regional tables grouped ")
  print("by event type")
  
  for (i in NATIONAL_SOURCE_INDEX : COUNTY_SOURCE_INDEX) {
    groupedTable <- regionalTableList[[i]] %>% group_by(EVTYPE)
    
    group_cost <- groupedTable %>% 
      summarise(GROUP_COST = sum(TOTALFIN), 
                .groups = 'drop')
    group_casualty <-
      summarise(groupedTable, GROUP_CASUALTIES = sum(TOTALCASUALTY), 
                .groups = 'drop')
    groupedResults <- merge(group_cost, group_casualty,
                            by.x = c('EVTYPE'),
                            by.y = c('EVTYPE'))
    if (i > 2) {
      groupedResults$STATE <- rep(STATE_, nrow(groupedResults))
    }
    
    if (i > 3) {
      groupedResults$COUNTY <- rep(COUNTYNAME_, nrow(groupedResults))
    }
    
    byCost <- groupedResults %>% arrange(desc(.$GROUP_COST), 
                                         .by_group = TRUE)
    byCasualty <- groupedResults %>% arrange(desc(.$GROUP_CASUALTIES), 
                                             .by_group = TRUE)
    
    rm (groupedTable)
    rm (groupedResults)
    rm (group_cost)
    rm (group_casualty)
    
    regionalTableList[[length(regionalTableList)+1]] <- byCost
    regionalTableList[[length(regionalTableList)+1]] <- byCasualty
  }
  
  return (regionalTableList)
}  ## End of finalDataTransform function



# ##############################################################################
# 
# ##############################################################################
showRawTable <- function(regionalTableList, i, stateName, countyName) {
  
  if (countyName != "") {
    print(paste0("Raw financial and casualty data for ",  
                 COUNTYNAME_, ", ",
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else if (stateName != "") {
    print(paste0("Raw financial and casualty data for ", 
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else  {
    print("Raw financial and casualty data for U.S.")
    table <- regionalTableList[i]
    print(head(data.frame(regionalTableList[i]), 10))
  }
}

showByCost <- function(regionalTableList, i, stateName, countyName) {
  
  if (countyName != "") {
    print(paste0("Raw financial and casualty data ordered by financial impact ",  
                 COUNTYNAME_, ", ",
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else if (stateName != "") {
    print(paste0("Raw financial and casualty data ordered by financial impact ", 
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else  {
    print("Raw financial and casualty data ordered by financial impact U.S. ")
    table <- regionalTableList[i]
    print(head(data.frame(regionalTableList[i]), 10))
  }
}

showByCasualty <- function(regionalTableList, i, stateName, countyName) {
  if (countyName != "") {
    print(paste0("Raw financial and casualty data ordered by casualties ",  
                 COUNTYNAME_, ", ",
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else if (stateName != "") {
    print(paste0("Raw financial and casualty data ordered by casualties ",
                 STATE_))
    print(head(data.frame(regionalTableList[i]), 10))
  } else {
    print("Raw financial and casualty data ordered by casualties U.S.")
    table <- regionalTableList[i]
    print(head(data.frame(regionalTableList[i]), 10))
  }
}


GenerateDisasterData <- function(regionalTableList) {
  
  showRawTable(regionalTableList, NATIONAL_SOURCE_INDEX, "", "")
  showRawTable(regionalTableList, STATE_SOURCE_INDEX, STATE_, "")
  showRawTable(regionalTableList, COUNTY_SOURCE_INDEX, STATE_, COUNTYNAME_)
  
  showByCost(regionalTableList, NATIONAL_COST_INDEX, "", "")
  showByCost(regionalTableList, STATE_COST_INDEX, STATE_, "")
  showByCost(regionalTableList, COUNTY_COST_INDEX, STATE_, COUNTYNAME_)
  
  showByCasualty(regionalTableList, NATIONAL_CASUALTY_INDEX, "", "")
  showByCasualty(regionalTableList, STATE_CASUALTY_INDEX, STATE_, "")
  showByCasualty(regionalTableList, COUNTY_CASUALTY_INDEX, STATE_, COUNTYNAME_)
  
  # scatter plot of COST vs CASUALTY from group_results
  # bar plots of COST vs CASUALTY from byCost and byCasualty
  # bar plots of COST vs CASUALTY from byCost and byCasualty
  
  # print("list test")
  # print(class(regionalTableList[NATIONAL_COST_INDEX]))
  
  data <- data.frame(regionalTableList[NATIONAL_COST_INDEX])
  print("data.frame test")
  print(class(data))
  
  png(file="plot4.png", width=480, height=480)
  
  plot( 
    data$GROUP_CASUALTIES, 
    data$GROUP_COST,
    main="Number of Casualties",
    xlab="Cost of event", 
    ylab="Miles Per Gallon ", 
    ylim=c(0,500000000),
    xlim=c(0,100),
    pch=19
  )
  
  dev.off()
}


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
  
  print("Display disaster data ###############################################")