library(lubridate)
library(RSQLite)
options(useFancyQuotes =FALSE)

insertInto <- function(data, table){
  lapply(1:nrow(data), function(i){
    charCol <- sapply(data, is.character)
    data[i, charCol] <- sQuote(data[i, charCol])
    if(any(is.na(data)))
      data[is.na(data)] <- sQuote(data[is.na(data)])
    top <- paste0("INSERT INTO ", table, " (", paste(names(data), collapse=", "), ")", collapse= " ")
    values <- data[i, ]
    names(values) <- NULL
    row.names(values) <- NULL
    values <- paste(values, collapse =", ")
    values <-  paste0("(", values, ")")
    paste(top, "VALUES", values, ";", collapse="")
  })
}

connector <- function(){
  if(.Platform$OS == "unix")
    dbConnect("SQLite", "/var/www/b13micro/files/b13Micro.db")
  else
    dbConnect("SQLite", "b13Micro.db")
}

submitData <- function(combinedData, organization, protocol){
  con <- connector()
  on.exit(dbDisconnect(con))
  # Check/add organization to DB
  oname <- data.frame(Init = "NA", FirstName = "NA", LastName = "NA", Organization=organization)
  orgs <- dbGetQuery(con, paste0("SELECT Organization, TechID FROM LabTech WHERE Organization=", sQuote(oname$Organization)))
  if(nrow(orgs) == 0){
    dbGetQuery(con, insertInto(oname, "LabTech")[[1]])
    orgs <- dbGetQuery(con, paste0("SELECT Organization, TechID FROM LabTech WHERE Organization=", sQuote(oname$Organization)))
  }
  
  combinedData$TechID <- orgs$TechID[1]
  
  # Throw out data already in the DB
  currentPlates <- dbGetQuery(con, "SELECT TimeStamp, TechID FROM plate")
  currentPlates$TimeStamp <- currentPlates$TimeStamp
  
  inputData <- combinedData[combinedData$Date %nin% currentPlates$TimeStamp |
                              combinedData$TechID %nin% currentPlates$TechID, ]
  if(nrow(inputData) == 0) return(odbcClose(con))
  
  
  # Insert new plate
  plates <- data.frame(TimeStamp = unique(inputData$Date))
  plates$Protocol <- protocol
  lapply(insertInto(plates, "Plate"), function(q)dbGetQuery(con, q))
  
  PlateID <- dbGetQuery(con, paste0("SELECT DISTINCT(PlateID) FROM plate WHERE TimeStamp =", sQuote(unique(plates$TimeStamp))))
  inputData$PlateID <- PlateID[1,1]
  
  # Insert sample names
  samples <- unique(inputData[, c("Sample", "Project", "PlateID")])
  lapply(insertInto(samples, "sample"),  function(q)dbGetQuery(con, q))
  
  s <- dbGetQuery(con, "SELECT * FROM sample")
  inputData$SampleID <- mapply(function(samp, proj)s$SampleID[s$Sample == samp & s$Project==proj][1],
                               inputData$Sample, inputData$Project)
  
  # Set mix ID
  inputData$MixID <- ifelse(protocol == "HF183_duplex", 1, 2)
  
  # Insert reaction data
  reaction <- inputData[, c("SampleID", "MixID", "Well", "Content", "Cq", "QuantPerReaction", "Quantifier",
                            "QuantPerFilter", "log10QuantPerFilter", "sk_Ct", "sk_dct", "IAC_Ct", "Inhibition")]

  reaction$ND <- ifelse(reaction$log10QuantPerFilter == "ND", "ND", "=")
   reaction <<- reaction 
  lapply(insertInto(reaction, "reaction"), function(q)dbGetQuery(con, q))
  
  # Insert standards data
  standards <- unique(inputData[, c("PlateID", "Slope", "yint", "r2", "Efficiency")])
  standards$Target <- dbGetQuery(con, paste0("SELECT Target FROM mixture WHERE MixID=", unique(inputData$MixID)))[1, 1]
  
  lapply(insertInto(standards, "standard"), function(q)dbGetQuery(con, q))
  
  
}

