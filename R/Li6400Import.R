#' Imports Licor 6400 photosynthesis system files.
#'
#' @param file Filename of the Li6400 text file (usually .csv or .tsv)
#' @param sep Character string to identify columns in the Li6400 file. Default is "\\t" for tab-separated. Use "," for comma-separated.
#' @return Returns a list with two items: a data.frame with the imported file without the remarks, and a data.frame with the remarks and a new vector RemarkRow that provides the original row number of the remark before the split of data and remarks.
#' @export

Li6400Import <- function(file, sep = "\t") {

  x <- readLines(file)

  # add the date to the HHMMSS vector - by default the file only has the time of day
  # grab date from second row in file
  the.date <- x[2]
  the.date <- gsub("\\\"", "",  the.date)

  # Licor uses non standard weekday names!
  the.date <- gsub("Thr", "Thu", the.date)

  the.date <- as.POSIXct(the.date,
                         format = "%a %b %d %Y %H:%M:%S")

  # look for phrase "$STARTOFDATA$" to get row after which import should start
  start.data <- grep("STARTOFDATA", x)

  if (length(start.data) > 1) {
    message("File has multiple starts. Obs likely will not be unique. Use Li6400RemarkReshuffle with caution.")
  }

  y <- utils::read.csv(file,
                       skip = start.data,
                       #sep = "\t",
                       sep = sep,
                       na.strings = c("NA", ""))

  # assemble full date based on date in file header, as Li6400 only records time in the HHMMSS field
  the.day <- format(the.date, "%Y-%m-%d")
  y$HHMMSS <- as.character(y$HHMMSS)
  y$HHMMSS[!is.na(y$HHMMSS)] <- paste(the.day, y$HHMMSS[!is.na(y$HHMMSS)], sep = " ")

  y$HHMMSS <- as.POSIXct(y$HHMMSS)

  # move Remarks out of the way, they are provided separately
  # get information on the rows that the remarks were in
  # also need to retrieve the first remark

  # figure out where the remark is based on the assumption it is after the constants
  start.remark <- grep("Const=", x)
  start.remark.1 <- start.remark[length(start.remark)]

  # read in just that one remark
  remarks.1 <- utils::read.csv(file,
                               sep = sep,
                               skip = start.remark.1,
                               nrows = 1,
                               header = FALSE,
                               na.strings = c("NA", ""))
  # format it to match the original coding
  remarks.1$Remarks <- remarks.1$V1
  remarks.1$V1 <- NULL
  remarks.1$RemarkRow <- 1

  # get the other remarks
  remarks <- y[is.na(y$FTime), ]
  remarks.row <- which(is.na(y$FTime))
  remarks$RemarkRow <- remarks.row


  # rename to avoid overwriting Obs in case the remarks will be merged back with the data later
  names(remarks) <- gsub("^Obs", "Remarks", names(remarks))

  # only keep row information and remarks
  remarks <- remarks[, names(remarks) %in% c("Remarks", "RemarkRow")]

  # add in first remark
  remarks <- rbind(remarks, remarks.1)

  y <- y[!is.na(y$FTime), ]
  out <- list(data = y,
              remarks = remarks)
  return(out)
}
