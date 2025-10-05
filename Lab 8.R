commit()
## Extract numeric amount values from 'lines', which
## is a vector of character values that contain amount values
getAmounts <- function(lines) {
  pieces <- strsplit(lines, "$", fixed=TRUE)
  amountChar <- sapply(pieces, function(x) x[2])
  amount <- as.numeric(gsub(",", "", amountChar))
  amount
}

## Find the numeric indices of the character values
## in 'lines' that contain amount values
getAmountLines <- function(lines) {
  amountLines <- grep("No", lines)
  amounts <- getAmounts(lines[amountLines])
  amountNA <- is.na(amounts)
  if (any(amountNA)) {
    amountLines[amountNA] <- amountLines[amountNA] - 1
  }
  amountLines
}

## Return a single logical value indicating whether the
## character value 'line' contains a "name"
containsName <- function(line) {
  grepl("[[:alpha:]] [[:alpha:]][[:alpha:]]+)?,", line)
}

## Extract just the "names" from 'lines', which is 
## is a vector of character values that contain "names"
getNames <- function(lines) {
  gsub("^[[:digit:]]*[[:space:]]*|,.+", "", lines)

}
