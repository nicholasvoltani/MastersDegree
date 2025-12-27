library(stringr)
library(readr)

DATA_INPUT_PATH = "C:/Users/nicho/Documents/MastersDegree/DataInputs/"
FILE_PATH = "C:/Users/nicho/Documents/MastersDegree/DataInputs/EIA_International_Production_251112.csv"

# Define the regex pattern for the separator line.
# Based on your request: "",total*\n
# ^      -> Start of the line
# \"\"   -> A literal "" (quotes are escaped with \)
# ,total -> A literal ,total
# .* -> Any character, zero or more times (your "*" placeholder)
# $      -> End of the line
separator_pattern <- "^\"\",\"total.*$"

input_file = FILE_PATH

# --- 2. Read and Process File ---
# Check if the file exists
if (!file.exists(input_file)) {
  stop("Error: Input file not found at ", input_file)
}


# Read the entire file as individual lines
all_lines <- readLines(input_file)

# Header is in second line: '''API, "", 1980, ...'''
header <- all_lines[2]
header <- new_string <- str_replace(header, '\"\"', '\"Region\"')
all_lines <- all_lines[3:length(all_lines)]

# --- 3. Identify and Group Chunks ---
# Create a logical vector (TRUE/FALSE) where TRUE means the line is a separator
is_separator <- grepl(separator_pattern, all_lines)

# We need to create a grouping ID for each chunk.
# A new chunk starts on the line *after* a separator line.
# We shift the `is_separator` vector by one and add `TRUE` at the
# beginning, because the very first line always starts a new chunk.
is_new_group_start <- c(TRUE, is_separator[-length(is_separator)])

# By taking the cumulative sum, we create a unique ID for each group.
# All lines in the first chunk get "1", the second chunk "2", etc.
group_id <- cumsum(is_new_group_start)

# Split the lines into a list of chunks based on the group ID
line_chunks <- split(all_lines, group_id)

# Creating prefixes for filenames
prefixes <- gsub('^"",\"(total[^\"]*)\".*$',
                 '\\1', 
                 all_lines[is_separator], 
                 perl = TRUE)


# --- 4. Write Chunks to New Files ---

# Loop through the list of chunks and save each one
for (id in names(line_chunks)) {
  
  # Get the actual lines for this chunk
  chunk_lines <- line_chunks[[id]]
  chunk_lines <- c(header, chunk_lines)
  
  # Create a new filename
  output_filename <- file.path(DATA_INPUT_PATH, 
                               paste0(output_prefix, "_", id, ".csv"))
  
  # Write the lines to the new file
  writeLines(chunk_lines, output_filename)
  
  print(paste("Created:", output_filename, "with", length(chunk_lines), "lines."))
}

print("---")
print(paste("Done. Split", input_file, "into", length(line_chunks), "parts."))
