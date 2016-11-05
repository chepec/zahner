#' Import Zahner impedance series data
#'
#' This function imports Zahner (Thales) impedance data
#' that was converted using the Bin2Txt.exe utility.
#'
#' @param datafilename   path to data file (string)
#' @param wearea         WE area (numeric)
#' @param runid.width    width of the runid (numeric)
#'
#' @importFrom dplyr "%>%"
#'
#' @return dataframe with experimental data and metadata
#'    LOGF               : log(frequency) / Hz
#'    LOGO               : log(Omega)
#'    LOGZm              : log(Z) measured / Ohm
#'    Phim               : Phase / degrees
#'    Real               : Real part (of impedance)
#'    Imag               : Imaginary part (of impedance)
#'    Phis               : Smoothed phase / degrees
#'    LOGZs              : Smoothed log(Z)
#'    LOGZzhit           : log(Z) ZHIT / Ohm
#'    sampleid           : sampleid
#'    runid              : runid
#'    Current            : current (metadata)
#'    Potential          : potential during measurement
#'    exp.started        : start time
#'    exp.ended          : end time
#'    exp.length.seconds : duration in seconds
#' @export
txt2df <- function(datafilename, wearea = NA, runid.width = 3) {
   # n = -1 to read all lines
   filecontents <- readLines(datafilename, n = -1)
   # remove all empty rows (makes it easier to identify metadata/data later)
   #filecontents <- filecontents[-grep("^\\s*?$", filecontents)]

   # identify the start of the data block
   # (always starts with one digits, comma, one or more digits),
   # (and by process of elimination, the metadata rows)
   rows.of.data <- regexpr("^\\d,\\d+", filecontents)
   # throw away the attributes (we don't need them)
   attributes(rows.of.data) <- NULL
   rownumbers.of.data <- which(rows.of.data == 1)
   # there's always a header line, and I like this to include this with the data
   rownumbers.of.data <- c(min(rownumbers.of.data) - 1, rownumbers.of.data)

   ff <-
      utils::read.csv(textConnection(filecontents[rownumbers.of.data]),
                      header = TRUE,
                      sep = "\t",
                      dec = ",")

   # get sampleid
   # (the line below will actually get the sampleid most of the time, irrespective of where
   # in the path the sampleid is located)
   ff$sampleid <-
      common::ExtractSampleIdString(datafilename)

   # temporarily store the current filename (but strip of the file extension)
   filename.with.runid <-
      basename(datafilename) %>%
      sub("\\.txt", "", .)
   # The way the runid is read means that a separating char between sampleid and runid
   # in the filename is not strictly necessary, as long as the sampleid always
   # adheres to having between 2 and 4 numbers in its last part.
   ff$runid <-
      # extracts the last <runid.width> characters from <filename.with.runid>
      # extracts the runid from the filename
      # runid is assumed to be the trailing digit or digits on the filename
      substr(filename.with.runid,
             nchar(filename.with.runid) - runid.width + 1,
             nchar(filename.with.runid))
   # if runid is less than 3 chars wide, pad it
   # (we are assuming it will never be longer than that)
   if (nchar(ff$runid[1]) < 3) {
      ff$runid <-
         common::int2padstr(as.numeric(ff$runid), "0", 3)
   }

   # the WE area
   ff$area <- wearea

   # Now read and save metadata
   metadataraw <- filecontents[which(rows.of.data == -1)]
   # only bother about certain predefined rows
   rgxp.meta <-
      "^(Time|Potential|Current)\\s+:\\s+"
   metadata <-
      metadataraw[grep(rgxp.meta,
                       metadataraw)]
   # note that the timestamp contains colons, and we need a unique
   # sep symbol to find the columns, so we create a new one on-the-fly
   metadata.table <-
      utils::read.table(textConnection(sub(":", ";", metadata)), sep = ";")

   # clean up...
   # In V1, remove trailing spaces
   metadata.table$V1 <- sub("\\s+$", "", metadata.table$V1)
   # in V2, remove leading spaces and handle units
   metadata.table$V2 <- sub("^\\s+", "", metadata.table$V2)
   # this grep can detect rows that contains one or more
   # digits (value) followed by one or more characters (unit)
   # example: grep("\\d+[A-Za-z]+$", metadata.table$V2)
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
