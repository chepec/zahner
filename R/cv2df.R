#' Read Zahner CV data
#'
#' This function converts Zahner CV (exported as csv)
#' to dataframe while retaining metadata.
#' Supports getting WE area from either datafile (metadata field) or
#' function argument. The datafile value takes priority.
#'
#' @param datafilename  complete path to csv file
#' @param wearea        defaults to NA
#'
#' @return dataframe with data and metadata
#' @export
cv2df <- function(datafilename, wearea = NA) {
   # n = -1 to read all lines
   filecontents <- readLines(datafilename, n = -1)

   # identify the start of the data block
   # (always starts with digit, comma, digittime),
   # and by extension, the metadata lines
   rows.of.data <- regexpr("^\\d+,\\d", filecontents)
   # throw away the attributes (we don't need them)
   attributes(rows.of.data) <- NULL
   rownumbers.of.data <- which(rows.of.data == 1)
   # there's always a header line, and I like this to be remain with the data
   rownumbers.of.data <- c(min(rownumbers.of.data) - 1, rownumbers.of.data)

   ff <- utils::read.csv(textConnection(filecontents[rownumbers.of.data]), header=T)
   names(ff) <- c("number", "time", "potential", "current")
   # add sampleid column
   ff$sampleid <- common::ProvideSampleId(datafilename)


   # Now read and save metadata
   metadataraw <- filecontents[which(rows.of.data == -1)]
   # only bother about certain predefined rows
   rgxp.meta <-
      "^(System|Temperature|Time|Slewrate|Electr\\.area\\/sqcm)\\.*:\\s+"
   metadata <-
      metadataraw[grep(rgxp.meta,
           metadataraw)]
   # extract the metadata values (the part after the colon) and
   # make it a one-row dataframe
   dd <-
      data.frame(matrix(gsub(rgxp.meta,
                             "",
                             metadata),
                        nrow = 1))
   names(dd) <- c("system", "temperature", "time", "slewrate", "area")
   # if area was supplied in datafile, use it
   # otherwise, use this function's wearea argument if the user supplied it
   # otherwise set to NA
   dd$area <- as.numeric(dd$area)
   # maybe the following if-clause should be reversed: perhaps it is
   # more user-friendly to always let the function argument have priority?
   if (is.na(dd$area)) {
      # datafile WE area is NA, go ahead and use the function arg instead
      dd$area <- wearea
   }

   # add sampleid column
   dd$sampleid <- unique(ff$sampleid)

   # parse time and calculate duration
   dd$exp.started <- unlist(strsplit(dd$time, "-"))[1]
   dd$exp.ended <- unlist(strsplit(dd$time, "-"))[2]
   dd$exp.length.seconds <-
      lubridate::period_to_seconds(lubridate::hms(dd$exp.ended) - lubridate::hms(dd$exp.started))
   # drop the original time column
   dd <- dd[,-which(names(dd) == "time")]

   # join dd dataframe with ff
   df <- dplyr::right_join(ff, dd, "sampleid")


   return(df)
}
