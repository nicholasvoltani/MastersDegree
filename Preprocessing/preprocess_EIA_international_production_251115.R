library(stringr)
library(readr)

REGEX_SEPARATOR = '^"","total'

input_file = "C:/Users/nicho/Downloads/EIA_International_Production_251112.csv"

split_csv_on_total <- function(input_file, output_prefix = "part") {
  # Read raw lines so that separators are not parsed away by read.csv
  lines <- readLines(input_file)
  
  # Header of values
  header <- lines[2]
  
  # Identify separator lines:  "",total*
  sep_idx <- grep(REGEX_SEPARATOR, lines)  # regex match
  
  # Add start and end bounds
  bounds <- c(0, sep_idx, length(lines) + 1)
  
  # Iterate over chunks
  for (i in seq_len(length(bounds) - 1)) {
    start <- bounds[i] + 1
    end   <- bounds[i + 1] - 1
    
    if (start > end) next  # skip empty chunks
    
    # Extract the text chunk
    chunk_lines <- paste(header, lines[start:end])
    
    # Parse chunk into a dataframe
    df <- read.csv(text = paste(chunk_lines, collapse = "\n"),
                   stringsAsFactors = FALSE)
    
    # Write it back to disk
    out_file <- sprintf("%s_%02d.csv", output_prefix, i)
    write.csv(df, out_file, row.names = FALSE)
    
    message("Wrote: ", out_file)
  }
}
