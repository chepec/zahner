#' Read Zahner EIS data
#'
#' This function converts Zahner impedance data
#' to dataframe while retaining metadata.
#'
#' @param datafilename  complete path to csv file
#' @param wearea        defaults to NA
#'
#' @return dataframe
#' @export
eis2df <- function(datafilename, wearea = NA) {
   # n = -1 to read all lines
   filecontents <- readLines(datafilename, n = -1)
   # remove all empty rows (makes it easier to identify metadata/data later)
   filecontents <- filecontents[-grep("^\\s*?$", filecontents)]

   # identify the start of the data block
   # (always starts with one or more digits, comma, one more digit),
   # and by extension, the metadata lines
   rows.of.data <- regexpr("^\\d+,\\d", filecontents)
   # throw away the attributes (we don't need them)
   attributes(rows.of.data) <- NULL
   rownumbers.of.data <- which(rows.of.data == 1)
   # there's always a header line, and I like this to include this with the data
   rownumbers.of.data <- c(min(rownumbers.of.data) - 1, rownumbers.of.data)

   ff <- utils::read.csv(textConnection(filecontents[rownumbers.of.data]), header = TRUE)

   # add sampleid column
   ff$sampleid <-
      common::ProvideSampleId(datafilename) %>% sub("-.*$", "", .)
   ff$runid <-
      common::ProvideSampleId(datafilename) %>% sub("^.*-", "", .)

   # the WE area
   ff$area <- wearea

   # Now read and save metadata
   metadataraw <- filecontents[which(rows.of.data == -1)]
   # only bother about certain predefined rows
   rgxp.meta <-
      "^(Time|Potential|Current)\\.*:\\s+"
   metadata <-
      metadataraw[grep(rgxp.meta,
           metadataraw)]
   # note that the timestamp contains colons, and we need a unique
   # sep symbol to find the columns, so we create a new one on-the-fly
   metadata.table <-
      utils::read.table(textConnection(sub(":", ";", metadata)), sep = ";")

   # clean up...
   # in V1, remove trailing dots
   metadata.table$V1 <- sub("\\.+$", "", metadata.table$V1)
   # in V2, remove leading spaces and handle units
   metadata.table$V2 <- sub("^\\s+", "", metadata.table$V2)
   # this grep can detect rows that contains one or more
   # digits (value) followed by one or more characters (unit)
   # grep("\\d+[A-Za-z]+$", metadata.table$V2)
   metadata.table$V3 <- sub("^-?\\d+\\.\\d+", "", metadata.table$V2)
   metadata.table$V2 <- sub("[A-Za-z]+$", "", metadata.table$V2)

   # now transpose the metadata df
   metadata.df <-
      metadata.table %>%
      # we don't want the unit column to complicate matters...
      dplyr::select(-V3) %>%
      dplyr::mutate(group = 1) %>%
      tidyr::spread(V1, V2) %>%
      # drop the added "group" column
      dplyr::select(-group)

   # parse the timestamps and calculate duration
   metadata.df$exp.started <- unlist(strsplit(metadata.df$Time, " - "))[1]
   metadata.df$exp.ended <- unlist(strsplit(metadata.df$Time, " - "))[2]
   metadata.df$exp.length.seconds <-
      lubridate::period_to_seconds(lubridate::hms(metadata.df$exp.ended) - lubridate::hms(metadata.df$exp.started))
   # drop the original time column
   metadata.df <-
      metadata.df %>%
      dplyr::select(-Time)

   # some more cleanup - make numeric data numeric
   metadata.df[,c("Current","Potential")] <-
      as.numeric(unlist(metadata.df[,c("Current","Potential")]))

   # add sampleid column
   metadata.df$sampleid <- unique(ff$sampleid)
   # join metadata dataframe with ff
   df <- dplyr::right_join(ff, metadata.df, "sampleid")

   return(df)
}
