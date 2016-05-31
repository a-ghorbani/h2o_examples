
# H2O verion: 3.9.1.3405 
# In case H2O access require authentication, as.data.frame will fail.
# Here a work around to make it work.
# in as.data.frame.H2OFrame function the username and password are retrieved 
# from connection and passed to getURL function.

require(RCurl)
require(h2o)

# Init H2O
h2o.init(port     = 54324,  startH2O = F, 
         password = "pass", username = "uname")

# Create a data.frame
df  <- data.frame(a = c(0.2, 0.3, 0.1, 0.5, 0.1, 0.2),
                  b = c(0.1, 0.1, 0.7, 0.3, 0.2, 0.9))

# upload the data frame into H2O cluster
h2o_df <- as.h2o(df, "h2o_df")

# Show few rows
head(h2o_df)

# download back the data frame back into R
as.data.frame(h2o_df)
# which fails

# a Fix for as.data.frame.H2OFrame
# The code is a copy pase from 
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/h2o-package/R/frame.R#L2146
# + the fix
#=================================================
# FIX
#=================================================
.h2o.__DOWNLOAD_FRAME <- function(frame_id, use_hex_string) {
  paste0('DownloadDataset?frame_id=', URLencode(frame_id), 
         '&hex_string=', as.numeric(use_hex_string))
}

as.data.frame.H2OFrame <- function(x, ...) {
  # Force loading of the types
  h2o:::.fetch.data(x,1L)
  # Versions of R prior to 3.1 should not use hex string.
  # Versions of R including 3.1 and later should use hex string.
  
  use_hex_string <- getRversion() >= "3.1"
  
  urlSuffix <- .h2o.__DOWNLOAD_FRAME(h2o.getId(x), use_hex_string)
  
  ttt <- h2o:::.h2o.doSafeGET(urlSuffix = urlSuffix)
  
  n <- nchar(ttt)
  
  # Delete last 1 or 2 characters if it's a newline.
  # Handle \r\n (for windows) or just \n (for not windows).
  chars_to_trim <- 0L
  if (n >= 2L) {
    c <- substr(ttt, n, n)
    if (c == "\n") chars_to_trim <- chars_to_trim + 1L
    if (chars_to_trim > 0L) {
      c <- substr(ttt, n-1L, n-1L)
      if (c == "\r") chars_to_trim <- chars_to_trim + 1L
    }
  }
  
  if (chars_to_trim > 0L) {
    ttt2 <- substr(ttt, 1L, n-chars_to_trim)
    ttt <- ttt2
  }
  
  # Get column types from H2O to set the dataframe types correctly
  colClasses <- attr(x, "types")
  colClasses <- gsub("numeric", NA, colClasses) # let R guess the appropriate numeric type
  colClasses <- gsub("int", NA, colClasses) # let R guess the appropriate numeric type
  colClasses <- gsub("real", NA, colClasses) # let R guess the appropriate numeric type
  colClasses <- gsub("enum", "factor", colClasses)
  colClasses <- gsub("uuid", "character", colClasses)
  colClasses <- gsub("string", "character", colClasses)
  colClasses <- gsub("time", NA, colClasses) # change to Date after ingestion
  # Substitute NAs for blank cells rather than skipping
  df <- read.csv((tcon <- textConnection(ttt)), blank.lines.skip = FALSE, na.strings = "", colClasses = colClasses, ...)
  close(tcon)
  # Convert all date columns to POSIXct
  dates <- attr(x, "types") %in% "time"
  if (length(dates) > 0) # why do some frames come in with no attributes but many columns?
    for (i in 1:length(dates)) { if (dates[[i]]) class(df[[i]]) = "POSIXct" }
  df
}

# Give it another try
as.data.frame(h2o_df)
# yoohoo works
