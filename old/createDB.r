source("qpcrDB.r")
options(useFancyQuotes =FALSE)

con <- connector()

statement <- Reduce(function(x, y)paste(x, y, collpase="
                                        "), readLines("qpcrSchema.sql"))
statements <- strsplit(statement, ";")[[1]]
statements <- head(statements, length(statements)-1)
lapply(statements, function(s){
  print(s)
  dbGetQuery(con, s)
  })

dbListTables(con)


mix <- data.frame(
                  reactionVolume = c(25, 25),
                  Target = c("HF183", "Enterococcus"),
                  PPmixDescription = c(NA, NA),
                  BSAVolume = c(NA, NA),
                  DIH2OVolume = c(NA, NA),
                  MasterMix = c(NA, NA),
                  MMVolume = c(NA, NA),
                  MgClConc = c(NA, NA),
                  MgClVolume = c(NA, NA),
                  sampleVolume = c(2, 2))


query <- insertInto(mix, "mixture")
lapply(query, function(q)dbGetQuery(con, q))


dbReadTable(con, "mixture")
dbDisconnect(con)
